/* This file is part of the 'atomes' software

'atomes' is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

'atomes' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with 'atomes'.
If not, see <https://www.gnu.org/licenses/>

Copyright (C) 2022-2024 by CNRS and University of Strasbourg */

/*!
* @file init_p.c
* @short Functions to initialize an atomes project
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'init_p.c'
*
* Contains:
*

  - The functions to initialize an atomes project

*
* List of functions:

  void init_curves_and_calc (project * this_proj);
  void init_project (gboolean alloc_box);

*/

#include "global.h"
#include "bind.h"
#include "callbacks.h"
#include "interface.h"
#include "project.h"
#include "curve.h"
#include "glview.h"
#include "glwindow.h"

/*!
  \fn void init_curves_and_calc (project * this_proj)

  \brief for a project reset analysis, curves, data to not performed

  \param this_proj the target project
*/
void init_curves_and_calc (project * this_proj)
{
  int i;
  for (i=0; i<NGRAPHS; i++)
  {
    this_proj -> runok[i] = FALSE;
    this_proj -> initok[i] = FALSE;
    this_proj -> visok[i] = FALSE;
  }
}

/*!
  \fn void init_project (gboolean alloc_box)

  \brief initialize a new project

  \param alloc_box allocate data for the MD box (1/0)
*/
void init_project (gboolean alloc_box)
{
  int i;
  project * new_proj = g_malloc0 (sizeof*proj);
  nprojects ++;
  activep = nprojects - 1;
  new_proj -> id = activep;
  new_proj -> name = g_strdup_printf("%s%2d", "Project N°", activep);

  new_proj -> delta[RI] = new_proj -> delta[CH] = 1.0;
  new_proj -> min[RI] = new_proj -> min[CH] = 1;
  new_proj -> delta[SP] = 2.0;
  for (i=0; i<5; i++) new_proj -> rsparam[i][1] = 10;
  new_proj -> csparam[5] = 10;
  new_proj -> rsearch[0] = -1;
  new_proj -> rsearch[1] = new_proj -> csearch = 500;

  new_proj -> tfile = -1;
  new_proj -> newproj = TRUE;
  new_proj -> steps = 1;
  new_proj -> xcor = 1;
  new_proj -> tunit = -1;

  new_proj -> sk_advanced[0] = 1.0;
  new_proj -> sk_advanced[1] = 15.0;

  new_proj -> coord = g_malloc0 (sizeof*new_proj -> coord);
  if (alloc_box) new_proj -> cell.box = g_malloc0(sizeof*new_proj -> cell.box);

  remove_edition_actions ();
  init_curves_and_calc (new_proj);
  new_proj -> numwid = -1;
  if (nprojects == 1)
  {
    workzone.first = g_malloc0 (sizeof*workzone.first);
    workzone.first = new_proj;
    workzone.last = g_malloc0 (sizeof*workzone.last);
  }
  else
  {
    new_proj -> prev = workzone.last;
    workzone.last -> next = new_proj;
  }
  workzone.last = new_proj;
  active_project_changed (new_proj -> id);
  prep_calc_actions ();
  new_proj -> newproj = FALSE;
}
