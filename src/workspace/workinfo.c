/* This file is part of the 'atomes' software

'atomes' is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

'atomes' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with 'atomes'.
If not, see <https://www.gnu.org/licenses/>

Copyright (C) 2022-2025 by CNRS and University of Strasbourg */

/*!
* @file workinfo.c
* @short Functions to display project information in GtkTextBuffer
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'workinfo.c'
*
* Contains:
*

 - Functions to display project information in GtkTextBuffer

*
* List of functions:

  void workinfo (project * this_proj, int i);

*/

#include "global.h"
#include "interface.h"
#include "project.h"

extern void update_rdf_view (project * this_proj, int rdf);
extern void update_sq_view (project * this_proj, int sqk);
extern void update_angle_view (project * this_proj);
extern void update_rings_view (project * this_proj, int c);
extern void update_chains_view (project * this_proj);
extern void update_spherical_view (project * this_proj);
extern void update_msd_view (project * this_proj);
extern void model_info (project * this_proj, GtkTextBuffer * buf);
extern void opengl_info (project * this_proj, GtkTextBuffer * buf);

/*!
  \fn void workinfo (project * this_proj, int i)

  \brief display information about a workspace menu item for a project

  \param this_proj the target project
  \param i the properties to display from the menu
*/
void workinfo (project * this_proj, int i)
{
  gchar * str;
  int j;
  project * tmp_proj;
  switch (i)
  {
    case 0:
      this_proj -> text_buffer[i] = add_buffer (NULL, NULL, NULL);
      print_info ("\n\nWorkspace information\n\n\n", "heading", this_proj -> text_buffer[0]);
      if (g_strcmp0(workspacefile, "(null)") == 0) workspacefile = NULL;
      if (workspacefile != NULL)
      {
        print_info ("\tWorkspace file: ", "italic", this_proj -> text_buffer[0]);
        print_info (workspacefile, NULL, this_proj -> text_buffer[0]);
      }
      str = g_strdup_printf ("\n\n\t%d", nprojects);
      print_info (str, "bold_red", this_proj -> text_buffer[0]);
      g_free (str);
      print_info (" project(s) in workspace: ", NULL, this_proj -> text_buffer[0]);
      tmp_proj = workzone.first;
      for (j=0; j<nprojects; j++)
      {
        print_info ("\n\n\t\t- ", NULL, this_proj -> text_buffer[0]);
        print_info (prepare_for_title(tmp_proj -> name), "italic", this_proj -> text_buffer[0]);
        if (tmp_proj -> next != NULL) tmp_proj = tmp_proj -> next;
      }
      print_info ("\n\n\tActive project: ", NULL, this_proj -> text_buffer[0]);
      print_info (prepare_for_title(active_project -> name), "bold_green", this_proj -> text_buffer[0]);
      break;
    case 1:
      this_proj -> text_buffer[i] = add_buffer (NULL, NULL, NULL);
      model_info (this_proj, this_proj -> text_buffer[1]);
      break;
    case 2:
      this_proj -> text_buffer[i] = add_buffer (NULL, NULL, NULL);
      opengl_info (this_proj, this_proj -> text_buffer[i]);
      break;
    case GR+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[GR]) update_rdf_view (this_proj, GR);
      break;
    case SQ+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[SQ]) update_sq_view (this_proj, SQ);
      break;
    case SK+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[SK]) update_sq_view (this_proj, SK);
      break;
    case GK+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[GK]) update_rdf_view (this_proj, GK);
      break;
    case AN+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[AN]) update_angle_view (this_proj);
      break;
    case RI+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[RI])
      {
        for (j=0; j<5; j++) if (this_proj -> rsparam[j][5]) update_rings_view (this_proj, j);
      }
      break;
    case CH+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[CH]) update_chains_view (this_proj);
      break;
    case SP+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[SP]) update_spherical_view (this_proj);
      break;
    case MS+OT:
      if (this_proj -> text_buffer[i] == NULL && this_proj -> visok[MS]) update_msd_view (this_proj);
      break;
  }
  if (this_proj -> text_buffer[i] == NULL) this_proj -> text_buffer[i] = add_buffer (NULL, NULL, NULL);
  view_buffer (this_proj -> text_buffer[i]);
}
