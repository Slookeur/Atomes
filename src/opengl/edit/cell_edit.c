/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

/*
* This file: 'cell_edit.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  G_MODULE_EXPORT gboolean close_cell (GtkWindow * widg, gpointer data);
  G_MODULE_EXPORT gboolean close_cell (GtkWidget * widg, GdkEvent * event, gpointer data);

  G_MODULE_EXPORT void close_cell_edit (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void edition_win (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void edition_win (GtkWidget * widg, gpointer data);

  GtkWidget * cell_tab (int i, struct project * this_proj);
  GtkWidget * create_cell_notebook (struct project * this_proj, GtkWidget * vbox);
  GtkWidget * create_cell_edition_window (struct project * this_proj, gpointer data);

*/

#include "cell_edit.h"

gchar * edit_names[7] = {"Wrap All Atoms in", "Shift Center", "Add Extra(s)", "Create Super-Cell", "Adjust Density", "Cut Slab", "PBC Pixels Debug"};

/*
*  G_MODULE_EXPORT void close_cell_edit (GtkButton * but, gpointer data)
*
*  Usage:
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void close_cell_edit (GtkButton * but, gpointer data)
{
  struct project * this_proj = (struct project *)data;
  int i;
  for (i=0; i<3; i++) this_proj -> modelgl -> cshift[i] = this_proj -> modelgl -> cell_win -> cparam[i];
  this_proj -> modelgl -> cell_win -> win = destroy_this_widget (this_proj -> modelgl -> cell_win -> win);
  g_free (this_proj -> modelgl -> cell_win);
  this_proj -> modelgl -> cell_win = NULL;
  cleaning_shaders (this_proj -> modelgl, SLABS);
  update (this_proj -> modelgl);
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT gboolean close_cell (GtkWindow * widg, gpointer data)
*
*  Usage:
*
*  GtkWindow * widg :
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean close_cell (GtkWindow * widg, gpointer data)
#else
/*
*  G_MODULE_EXPORT gboolean close_cell (GtkWidget * widg, GdkEvent * event, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  GdkEvent * event : the GdkEvent triggering the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean close_cell (GtkWidget * widg, GdkEvent * event, gpointer data)
#endif
{
  close_cell_edit (NULL, data);
  return FALSE;
}

/*
*  GtkWidget * cell_tab (int i, struct project * this_proj)
*
*  Usage:
*
*  int i                      :
*  struct project * this_proj : the target project
*/
GtkWidget * cell_tab (int i, struct project * this_proj)
{
  switch (i)
  {
    case 0:
      return shift_center_tab (this_proj);
      break;
    case 1:
      return add_extra_cell_tab (this_proj -> modelgl);
      break;
    case 2:
      return adjust_density_tab (this_proj);
      break;
    case 3:
      return cut_in_model (this_proj);
      break;
#ifdef DEBUG
    default:
      return pixels_tab (this_proj);
      break;
#else
    default:
      return NULL;
      break;
#endif
  }
}

/*
*  GtkWidget * create_cell_notebook (struct project * this_proj, GtkWidget * vbox)
*
*  Usage:
*
*  struct project * this_proj : the target project
*  GtkWidget * vbox           : the GtkWidget sending the signal
*/
GtkWidget * create_cell_notebook (struct project * this_proj, GtkWidget * vbox)
{
  GtkWidget * notebook = gtk_notebook_new ();
  gchar * str;
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, notebook, TRUE, TRUE, 0);

  int i, j;
  gboolean doit;
#ifdef DEBUG
  for (i=0; i<5; i ++)
#else
  for (i=0; i<4; i ++)
#endif
  {
#ifdef DEBUG
    doit = (i == 4 && ! this_proj -> pix[0]) ? FALSE : TRUE;
#else
    doit = TRUE;
#endif
    if (doit)
    {
      j = (i < 2) ? i+1: i+2;
      str = g_strdup_printf ("<b>%s</b>", edit_names[j]);
      gtk_notebook_insert_page (GTK_NOTEBOOK(notebook), cell_tab (i, this_proj), markup_label(str, -1, -1, 0.0, 0.5), i);
      if (i > 1) widget_set_sensitive (gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), i), (this_proj -> steps) > 1 ? 0 : 1);
      g_free (str);
    }
  }

  show_the_widgets (notebook);
  return notebook;
}

/*
*  GtkWidget * create_cell_edition_window (struct project * this_proj, gpointer data)
*
*  Usage:
*
*  struct project * this_proj : the target project
*  gpointer data              : the associated data pointer
*/
GtkWidget * create_cell_edition_window (struct project * this_proj, gpointer data)
{
  gchar * str = g_strdup_printf ("Cell edition - %s", this_proj -> name);
  GtkWidget * win = create_win (str, this_proj -> modelgl -> win, FALSE, FALSE);
  g_free (str);
  GtkWidget * vbox = create_vbox (5);
  add_container_child (CONTAINER_WIN, win, vbox);
  gtk_widget_set_size_request (vbox, 700 , -1);

  int i;
  for (i=0; i<3; i++) this_proj -> modelgl -> cell_win -> cparam[i] = this_proj -> modelgl -> cshift[i];
  this_proj -> modelgl -> cell_win -> notebook = create_cell_notebook (this_proj, vbox);

  GtkWidget * hbox = create_hbox (5);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, TRUE, FALSE, 0);
  GtkWidget * but = create_button ("Close", IMG_STOCK, FCLOSE, -1, -1, GTK_RELIEF_NORMAL, G_CALLBACK(close_cell_edit), this_proj);
  add_box_child_end (hbox, but, FALSE, FALSE, 5);
  add_gtk_close_event (win, G_CALLBACK(close_cell), this_proj);

  return win;
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void edition_win (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage:
*
*  GSimpleAction * action :
*  GVariant * parameter   :
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT void edition_win (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*
*  G_MODULE_EXPORT void edition_win (GtkWidget * widg, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void edition_win (GtkWidget * widg, gpointer data)
#endif
{
  tint * id = (tint *) data;
  int i;
  struct project * this_proj = get_project_by_id(id -> a);
  if (this_proj -> modelgl -> cell_win == NULL)
  {
    this_proj -> modelgl -> cell_win = g_malloc0 (sizeof*this_proj -> modelgl -> cell_win);
    this_proj -> modelgl -> cell_win -> homo_density = TRUE;
    this_proj -> modelgl -> cell_win -> slab_alpha = 0.75;
    this_proj -> modelgl -> cell_win -> slab_lot = allocint (this_proj -> nspec);
    this_proj -> modelgl -> cell_win -> slab_pbc = this_proj -> cell.pbc;
    for (i=0; i<5; i++)
    {
      this_proj -> modelgl -> cell_win -> slab_pointer[i].a = id -> a;
      this_proj -> modelgl -> cell_win -> slab_pointer[i].b = i;
    }
    for (i=9; i<13; i++) this_proj -> modelgl -> cell_win -> cparam[i] = 5.0;
    for (i=13; i<15; i++) this_proj -> modelgl -> cell_win -> cparam[i] = 2.5;
    for (i=15; i<18; i++) this_proj -> modelgl -> cell_win -> cparam[i] = 90.0;
    this_proj -> modelgl -> cell_win -> win = create_cell_edition_window (this_proj, data);
  }
  show_the_widgets (this_proj -> modelgl -> cell_win -> win);

  for (i=1; i<6; i++)
  {
    if (i < 3) gtk_widget_hide (this_proj -> modelgl -> cell_win -> slab_hbox[i]);
    gtk_widget_hide (this_proj -> modelgl -> cell_win -> slab_box[i]);
  }
  int page = id -> b;
  gtk_notebook_set_current_page (GTK_NOTEBOOK (this_proj -> modelgl -> cell_win -> notebook), page);
}
