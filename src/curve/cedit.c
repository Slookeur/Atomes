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
* This file: 'cedit.c'
*
*  Contains:
*

- The initialization of the curve edition dialog

*
*  List of subroutines:

  int numi (int r, int c);

  void prepbox (tint * cd);
  void set_set (int a, int b, int c);

  static void fill_proj_model (GtkTreeStore * store);

  G_MODULE_EXPORT void run_curve_edit (GtkDialog * dial, gint response_id, gpointer data);
  G_MODULE_EXPORT void edit_curve (GtkWidget * curve, gpointer data);

  GtkWidget * create_projects_tree ();

*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gtk/gtk.h>
#include <cairo.h>
#include <cairo-pdf.h>
#include <cairo-svg.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "global.h"
#include "interface.h"
#include "callbacks.h"
#include "project.h"
#include "curve.h"

extern G_MODULE_EXPORT void set_data_aspect (GtkComboBox * box, gpointer data);
extern G_MODULE_EXPORT void update_axis (GtkComboBox * widg, gpointer data);
extern G_MODULE_EXPORT void action_to_plot (GtkWidget * widg, gpointer data);
extern gboolean was_not_added (ExtraSets * sets, int a, int b, int c);
extern G_MODULE_EXPORT void choose_set (GtkComboBox * box, gpointer data);
extern GtkWidget * create_org_list (gpointer data);
extern GtkWidget * create_tab_1 (gpointer data);
extern GtkWidget * create_tab_2 (gpointer data);
extern GtkWidget * create_tab_3 (gpointer data);
extern GtkWidget * create_tab_4 (gpointer data);
extern GtkWidget * data_aspect;
extern GtkWidget * Glyph_box;
extern GtkWidget * Hist_box;
extern GtkWidget * orgtree;
extern GtkWidget * datascroll;
extern char * ctext[2];

GtkWidget * setcolorbox = NULL;
GtkWidget * thesetbox = NULL;
tint prc;
GtkTreePath ** ppath = NULL;
GtkTreePath ** cpath = NULL;
GtkWidget * projtree = NULL;
GtkWidget * xyp[2];

char * lapos[2]={"x: ", "y: "};

/*
*  void prepbox (tint * cd)
*
*  Usage: prepare the curve selection combo box
*
*  tint * cd : the associated data pointer
*/
void prepbox (tint * cd)
{
  int i, k, l;
  int m, n, o, p;
  gchar * str;

  k = cd -> a;
  l = cd -> b;
  m = cd -> c;
  struct project * extra_proj;
  struct project * this_proj = get_project_by_id(k);
  str = g_strdup_printf ("%s - %s", prepare_for_title(this_proj -> name), this_proj -> curves[l][m] -> name);
  combo_text_append (setcolorbox, str);
  g_free (str);
  struct cextra * ctmp = this_proj -> curves[l][m] -> extrac -> first;
  for ( i=0 ; i < this_proj -> curves[l][m] -> extrac -> extras ; i++ )
  {
    n = ctmp -> id.a;
    o = ctmp -> id.b;
    p = ctmp -> id.c;
    extra_proj = get_project_by_id(n);
    str = g_strdup_printf ("%s - %s", prepare_for_title(extra_proj -> name), extra_proj -> curves[o][p] -> name);
    combo_text_append (setcolorbox, str);
    g_free (str);
    if (ctmp -> next != NULL) ctmp = ctmp -> next;
  }
  if (this_proj -> curves[l][m] -> extrac -> extras > 0)
  {
    widget_set_sensitive (setcolorbox, 1);
  }
  else
  {
    widget_set_sensitive (setcolorbox, 0);
  }
  gtk_widget_set_size_request (setcolorbox, -1, 40);
  gtk_combo_box_set_active (GTK_COMBO_BOX (setcolorbox), 0);
  g_signal_connect (G_OBJECT(setcolorbox), "changed", G_CALLBACK(choose_set), NULL);
}

/*
*  void set_set (int a, int b, int c)
*
*  Usage:
*
*  int a :
*  int b :
*  int c :
*/
void set_set (int a, int b, int c)
{
  setcolorbox = destroy_this_widget (setcolorbox);
  setcolorbox = create_combo ();
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, thesetbox, setcolorbox, FALSE, FALSE, 0);
  gtk_widget_show(setcolorbox);
  action_to_plot (NULL, & get_project_by_id(a) -> idcc[b][c]);
  prepbox (& get_project_by_id(activeg) -> idcc[activer][activec]);
  choose_set (GTK_COMBO_BOX(setcolorbox), NULL);
  orgtree = destroy_this_widget (orgtree);

  add_container_child (CONTAINER_SCR, datascroll, create_org_list(& get_project_by_id(activeg) -> idcc[activer][activec]));
  gtk_widget_show (orgtree);
  widget_set_sensitive (orgtree, get_project_by_id(activeg) -> curves[activer][activec] -> extrac -> extras);
}

/*
*  int numi (int r, int c)
*
*  Usage:
*
*  int r :
*  int c :
*/
int numi (int r, int c)
{
  if (activer == 3 && activec > 0 && r == 3)
  {
    return -1;
  }
  else
  {
    return 0;
  }
}

/*
*  static void fill_proj_model (GtkTreeStore * store)
*
*  Usage:
*
*  GtkTreeStore * store : the GtkTreeStore to fill
*/
static void fill_proj_model (GtkTreeStore * store)
{
  GtkTreeIter projlevel;
  GtkTreeIter calclevel;
  GtkTreeIter curvelevel;
  struct project * this_proj;
  int i, j, k;
  int start, end, step;
  gboolean append;

   /* Append a top level row and leave it empty */
  if (ppath != NULL) g_free (ppath);
  if (cpath != NULL) g_free (cpath);
  ppath = g_malloc0 (nprojects*sizeof*ppath);
  cpath = g_malloc0 (nprojects*sizeof*cpath);
  for (i=0; i<nprojects; i++)
  {
    this_proj = get_project_by_id(i);
    gtk_tree_store_append (store, & projlevel, NULL);
    gtk_tree_store_set (store, & projlevel, 0, 0, 1, prepare_for_title(this_proj -> name), 2, TRUE, 3, -1, -1);
    ppath[i] = gtk_tree_model_get_path ((GtkTreeModel *)store, & projlevel);
    if (activer == 0 || activer == 3)
    {
      start = 0;
      end = 4;
      step = 3;
    }
    else if (activer == 1 || activer == 2)
    {
      start = 1;
      end = 3;
      step = 1;
    }
    else
    {
      start = activer;
      end = start + 1;
      step = 1;
    }
    for (j=start; j<end; j=j+step)
    {
      if (this_proj -> initok[j])
      {
        gtk_tree_store_append (store, & calclevel, & projlevel);
        gtk_tree_store_set (store, & calclevel, 0, 0, 1, graph_name[j], 2, TRUE, 3, -1, -1);
        if (j == start)
        {
          cpath[i] = gtk_tree_model_get_path ((GtkTreeModel *)store, & calclevel);
        }
        for (k = 0 ; k < this_proj -> numc[j] ; k++)
        {
          append = FALSE;
          if (i != activeg && this_proj -> curves[j][k] -> ndata != 0)
          {
            append = TRUE;
          }
          else if (((i != activeg) || (j != activer || k != activec)) && this_proj -> curves[j][k] -> ndata != 0)
          {
            append = TRUE;
          }
          if (append)
          {
            gtk_tree_store_append (store, & curvelevel, & calclevel);
            gtk_tree_store_set (store, & curvelevel,
                                0, 1,
                                1, this_proj -> curves[j][k] -> name,
                                2, ! was_not_added (active_project -> curves[activer][activec] -> extrac, i, j, k),
                                3, i,
                                4, j,
                                5, k, -1);
          }
        }
      }
    }
  }
}

GtkTreeStore * projmodel;

/*
*  void toggle_curve (GtkCellRendererToggle * cell_renderer, gchar * string_path, gpointer data)
*
*  Usage:
*
*  GtkCellRendererToggle * cell_renderer :
*  gchar * string_path                   :
*  gpointer data              :
*/
G_MODULE_EXPORT void toggle_curve (GtkCellRendererToggle * cell_renderer, gchar * string_path, gpointer data)
{
  int i, j, k;
  gboolean status;
  GtkTreeIter iter;
  GtkTreePath * path = gtk_tree_path_new_from_string (string_path);
  gtk_tree_model_get_iter (GTK_TREE_MODEL(projmodel), & iter, path);
  gtk_tree_model_get (GTK_TREE_MODEL(projmodel), & iter, 2, & status, 3, & i, 4, & j, 5, & k, -1);
  gtk_tree_store_set (projmodel, & iter, 2, ! status, -1);
  set_set (i, j, k);
}

/*
*  void set_visible_data (GtkTreeViewColumn * col, GtkCellRenderer * renderer,
                          GtkTreeModel * mod, GtkTreeIter * iter, gpointer data)
*
*  Usage:
*
*  GtkTreeViewColumn * col    :
*  GtkCellRenderer * renderer :
*  GtkTreeModel * mod         :
*  GtkTreeIter * iter         :
*  gpointer data              :
*/
void set_visible_data (GtkTreeViewColumn * col, GtkCellRenderer * renderer,
                       GtkTreeModel * mod, GtkTreeIter * iter, gpointer data)
{
  int m;
  gtk_tree_model_get (mod, iter, 0, & m, -1);
  gtk_cell_renderer_set_visible (renderer, m);
}

/*
*  GtkWidget * create_projects_tree ()
*
*  Usage:
*
*   :
*/
GtkWidget * create_projects_tree ()
{
  int i;
  GtkTreeViewColumn * projcol[6];
  GtkCellRenderer * projcell[6];
  gchar * col_title[6] = {" ", "Data sets", "Select", " ", " ", " "};
  gchar * ctype[6]={"text", "text", "active", "text", "text", "text"};
  GType col_type[6] = {G_TYPE_INT, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT};
  projmodel = gtk_tree_store_newv (6, col_type);
  projtree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(projmodel));
  for (i=0; i<6; i++)
  {
    if (i == 2)
    {
      projcell[i] = gtk_cell_renderer_toggle_new ();
      projcol[i] = gtk_tree_view_column_new_with_attributes(col_title[i],  projcell[i], ctype[i], i, NULL);
      g_signal_connect (G_OBJECT(projcell[i]), "toggled", G_CALLBACK(toggle_curve), NULL);
      gtk_tree_view_column_set_cell_data_func (projcol[i], projcell[i], set_visible_data, NULL, NULL);
    }
    else
    {
      projcell[i] = gtk_cell_renderer_text_new();
      projcol[i] =  gtk_tree_view_column_new_with_attributes(col_title[i], projcell[i], ctype[i], i, NULL);
    }
    gtk_tree_view_append_column(GTK_TREE_VIEW(projtree), projcol[i]);
    if (i == 0 || i > 2) gtk_tree_view_column_set_visible (projcol[i], FALSE);
  }
  fill_proj_model (projmodel);
  g_object_unref (projmodel);
  gtk_tree_view_expand_all (GTK_TREE_VIEW(projtree));
  return projtree;
}

/*
*  G_MODULE_EXPORT void run_curve_edit (GtkDialog * dial, gint response_id, gpointer data)
*
*  Usage:
*
*  GtkDialog * dial :
*  gint response_id :
*  gpointer data    :
*/
G_MODULE_EXPORT void run_curve_edit (GtkDialog * dial, gint response_id, gpointer data)
{
  destroy_this_dialog (dial);
  int i;
  for (i=0 ; i<2; i++) xyp[i] = NULL;
  axischoice = NULL;
}

/*
*  G_MODULE_EXPORT void edit_curve (GtkWidget * curve, gpointer data)
*
*  Usage:
*
*  GtkWidget * curve :
*  gpointer data     :
*/
G_MODULE_EXPORT void edit_curve (GtkWidget * curve, gpointer data)
{
  GtkWidget * edit_box;
  GtkWidget * ebox;
  GtkWidget * enoote;
  GtkWidget * dbox;
  GtkWidget * scrollsets;

// Axis data

  tint * cd = (tint *) data;
  int a = activeg = cd -> a;
  int b = activer = cd -> b;
  int c = activec = cd -> c;

#ifdef DEBUG
  g_debug ("CEDIT: a= %d, b= %d, c= %d", a, b, c);
#endif

  struct project * this_proj = get_project_by_id(a);

  ctext[0] = "x ∈ [0.0, 1.0]";
  ctext[1] = "y ∈ [0.0, 1.0]";
  edit_box = dialogmodal ("Edit curve", GTK_WINDOW(this_proj -> curves[b][c] -> window));
  gtk_window_set_resizable (GTK_WINDOW (edit_box), FALSE);
#ifndef GTK4
  gtk_window_set_icon (GTK_WINDOW (edit_box), THETD);
#endif
  ebox = dialog_get_content_area (edit_box);
  enoote = gtk_notebook_new ();
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ebox, enoote, FALSE, FALSE, 0);

// The first tab of the notebook
  gtk_notebook_append_page (GTK_NOTEBOOK(enoote), create_tab_1 (data), gtk_label_new ("Graph"));
//  gtk_notebook_set_tab_label (GTK_NOTEBOOK (enoote), gtk_notebook_get_nth_page (GTK_NOTEBOOK (enoote), 0), gtk_label_new ("Graph"));

// The second tab of the notebook
  gtk_notebook_append_page (GTK_NOTEBOOK(enoote), create_tab_2 (data), gtk_label_new ("Data"));
//  gtk_notebook_set_tab_label (GTK_NOTEBOOK (enoote), gtk_notebook_get_nth_page (GTK_NOTEBOOK (enoote), 1), gtk_label_new ("Data"));

// The third tab of the notebook
  gtk_notebook_append_page (GTK_NOTEBOOK(enoote), create_tab_3 (data), gtk_label_new ("Legend"));
//  gtk_notebook_set_tab_label (GTK_NOTEBOOK (enoote), gtk_notebook_get_nth_page (GTK_NOTEBOOK (enoote), 2), gtk_label_new ("Legend"));

// The fourth tab of the notebook
  gtk_notebook_append_page (GTK_NOTEBOOK(enoote), create_tab_4 (data), gtk_label_new ("Axis"));
//  gtk_notebook_set_tab_label (GTK_NOTEBOOK (enoote), gtk_notebook_get_nth_page (GTK_NOTEBOOK (enoote), 3), gtk_label_new ("Axis"));

// The fifth tab of the notebook
  dbox = create_vbox (BSEP);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, dbox, markup_label("<b>Add data set(s) to the active window</b>", -1, 30, 0.5, 0.5), FALSE, FALSE, 0);
  scrollsets = create_scroll (dbox, 250, 525, GTK_SHADOW_ETCHED_IN);
  add_container_child (CONTAINER_SCR, scrollsets, create_projects_tree ());
  gtk_notebook_append_page (GTK_NOTEBOOK(enoote), dbox, gtk_label_new ("Add data set"));
//  gtk_notebook_set_tab_label (GTK_NOTEBOOK (enoote), gtk_notebook_get_nth_page (GTK_NOTEBOOK (enoote), 4), gtk_label_new ("Add data set"));

  add_gtk_close_event (edit_box, G_CALLBACK(destroy_this_window), NULL);

  if (gtk_combo_box_get_active (GTK_COMBO_BOX(data_aspect)))
  {
    gtk_widget_hide (Glyph_box);
  }
  else
  {
    gtk_widget_hide (Hist_box);
  }

  g_signal_connect (G_OBJECT(edit_box), "response", G_CALLBACK(run_curve_edit), NULL);
  show_the_widgets (edit_box);
  set_data_aspect (GTK_COMBO_BOX(data_aspect), data);
  update_axis (GTK_COMBO_BOX(axischoice), data);

  dialog_id ++;
  Event_loop[dialog_id] = g_main_loop_new (NULL, FALSE);
  g_main_loop_run (Event_loop[dialog_id]);
}
