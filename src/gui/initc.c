/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include "global.h"
#include "bind.h"
#include "callbacks.h"
#include "project.h"

extern void clean_this_curve_window (int cid, int rid);
extern void initgr (int r, int s);
extern void initsq (int r, int s);
extern void initbd (int s);
extern void initang (int s);
extern void initrng (int s);
extern void initchn (int s);
extern void initmsd (int s);
extern void initsh (int s);

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

void alloc_curves (int c)
{
  int i;
  if (active_project -> idcc[c] != NULL)
  {
    g_free (active_project -> idcc[c]);
    active_project -> idcc[c] = NULL;
  }
  active_project -> idcc[c] = g_malloc0 (active_project -> numc[c]*sizeof*active_project -> idcc[c]);
  if (active_project -> curves[c] != NULL)
  {
    g_free (active_project -> curves[c]);
    active_project -> curves[c] = NULL;
  }
  active_project -> curves[c] = g_malloc (active_project -> numc[c]*sizeof*active_project -> curves);
  for (i = 0; i < active_project -> numc[c]; i++)
  {
    active_project -> curves[c][i] = g_malloc0 (sizeof*active_project -> curves[c][i]);
    active_project -> curves[c][i] -> cfile = NULL;
    active_project -> curves[c][i] -> name = NULL;
    active_project -> curves[c][i] -> axis_title[0] = NULL;
    active_project -> curves[c][i] -> axis_title[1] = NULL;
  }
}

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

void initcnames (int w, int s)
{
  switch (w)
  {
    case GR:
      initgr (w, s);
      break;
    case SQ:
      initsq (w, s);
      break;
    case SK:
      initsq (w, s);
      break;
    case GK:
      initgr (w, s);
      break;
    case BD:
      initbd (s);
      break;
    case AN:
      initang (s);
      break;
    case RI:
      initrng (s);
      break;
    case CH:
      initchn (s);
      break;
    case SP:
      initsh (s);
      break;
    default:
      initmsd (s);
      break;
  }
}

void prepostcalc (GtkWidget * widg, gboolean status, int run, int adv, double opc)
{
  //int i;
//  char * bar[2] = {"bond properties", "nearest neigbhors table"};
//  char * mess;
  if (run < NGRAPHS && run > -1) active_project -> visok[run] = adv;
  if (! status)
  {
#ifndef GTK4
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
#ifndef GTK4
    if (widg != NULL) gdk_window_set_opacity (gtk_widget_get_window(widg), opc);
#endif
  }
}
