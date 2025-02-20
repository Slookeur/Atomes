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
* @file initc.c
* @short Curve data buffer initialization
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'initc.c'
*
* Contains:
*

 - Curve data buffer initialization

*
* List of functions:

  void clean_curves_data (int calc, int start, int end);
  void alloc_curves (int rid);
  void initcwidgets ();
  void prepostcalc (GtkWidget * widg, gboolean status, int run, int adv, double opc);

*/

#include "global.h"
#include "callbacks.h"
#include "project.h"

extern void clean_this_curve_window (int cid, int rid);

/*!
  \fn void clean_curves_data (int calc, int start, int end)

  \brief clean curve data on a range of curve id

  \param calc the calculation
  \param start the starting value
  \param end the ending value
*/
void clean_curves_data (int calc, int start, int end)
{
  int i;
  for (i=start; i<end; i++)
  {
    if (active_project -> curves[calc])
    {
      clean_this_curve_window (i, calc);
    }
  }
}

/*!
  \fn void alloc_curves (int rid)

  \brief allocating curve data

  \param rid analysis id
*/
void alloc_curves (int rid)
{
  int i;
  if (active_project -> idcc[rid] != NULL)
  {
    g_free (active_project -> idcc[rid]);
    active_project -> idcc[rid] = NULL;
  }
  active_project -> idcc[rid] = g_malloc0 (active_project -> numc[rid]*sizeof*active_project -> idcc[rid]);
  if (active_project -> curves[rid] != NULL)
  {
    g_free (active_project -> curves[rid]);
    active_project -> curves[rid] = NULL;
  }
  active_project -> curves[rid] = g_malloc (active_project -> numc[rid]*sizeof*active_project -> curves);
  for (i = 0; i < active_project -> numc[rid]; i++)
  {
    active_project -> curves[rid][i] = g_malloc0 (sizeof*active_project -> curves[rid][i]);
    active_project -> curves[rid][i] -> cfile = NULL;
    active_project -> curves[rid][i] -> name = NULL;
    active_project -> curves[rid][i] -> axis_title[0] = NULL;
    active_project -> curves[rid][i] -> axis_title[1] = NULL;
  }
}

/*!
  \fn void initcwidgets ()

  \brief initializing curve values
*/
void initcwidgets ()
{
  int i, j;

  j=active_project -> nspec;
  active_project -> numc[GR] = 16+5*j*j;
  active_project -> numc[SQ] = 8+4*j*j;
  active_project -> numc[SK] = 8+4*j*j;
  active_project -> numc[GK] = active_project -> numc[GR];
  active_project -> numc[BD] = j*j;
  active_project -> numc[AN] = j*j*j + j*j*j*j;
  active_project -> numc[RI] = 20*(j+1);
  active_project -> numc[CH] = j+1;
  active_project -> numc[SP] = 0;
  active_project -> numc[MS] = 0;
  if (active_project -> steps > 1) active_project -> numc[MS] = 14*j+6;

  if (j == 2)
  {
    active_project -> numc[GR] = active_project -> numc[GR] + 6;
    active_project -> numc[SQ] = active_project -> numc[SQ] + 8;
    active_project -> numc[SK] = active_project -> numc[SK] + 8;
    active_project -> numc[GK] = active_project -> numc[GK] + 6;
  }
  active_project -> numwid = active_project -> numc[GR]
                       + active_project -> numc[SQ]
                       + active_project -> numc[SK]
                       + active_project -> numc[GK]
                       + active_project -> numc[BD]
                       + active_project -> numc[AN]
                       + active_project -> numc[RI]
                       + active_project -> numc[CH]
                       + active_project -> numc[MS];
  for (i=0; i<NGRAPHS; i++)
  {
    if (i != SP) alloc_curves (i);
  }
}

/*!
  \fn void prepostcalc (GtkWidget * widg, gboolean status, int run, int adv, double opc)

  \brief to just before and just after running a calculation

  \param widg the GtkWidget sending the signal
  \param status calculation completed (1/0)
  \param run calculation id
  \param adv calculation result
  \param opc opacity
*/
void prepostcalc (GtkWidget * widg, gboolean status, int run, int adv, double opc)
{
  //int i;
//  char * bar[2] = {"bond properties", "nearest neigbhors table"};
//  char * mess;
  if (run < NGRAPHS && run > -1) active_project -> visok[run] = adv;
  if (! status)
  {
#ifdef GTK3
    if (widg != NULL) gdk_window_set_opacity (gtk_widget_get_window(widg), opc);
#endif
/*    if (adv)
    {
      // bar[run]
      mess = g_strdup_printf ("Please wait calculation in progress");
      pop = show_popup (mess, widg);
      g_free (mess);
      mess = g_strdup_printf ("Computing");
      //statusval = gtk_statusbar_push (statusbar, run, mess);
      g_free (mess);
      show_the_widgets (pop);
    }*/
  }
  else
  {
    if (adv && run > -1)
    {
      //gtk_statusbar_remove (statusbar, run, statusval);
      //destroy_this_widget(pop);
    }
#ifdef GTK3
    if (widg != NULL) gdk_window_set_opacity (gtk_widget_get_window(widg), opc);
#endif
  }
}
