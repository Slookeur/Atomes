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
* This file: 'w_measures.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  int get_measure_type (glwin * view);

  G_MODULE_EXPORT gboolean measure_tree_selection_event (GtkWidget * widget, GdkEventButton * event, gpointer data);
  G_MODULE_EXPORT gboolean close_measure_event (GtkWindow * widg, gpointer data);
  G_MODULE_EXPORT gboolean close_measure_event (GtkWidget * widg, GdkEvent * event, gpointer data);

  gchar * create_measure_label (glwin * view, int pi);

  void fill_bond_model_row (int p, int a, int b, GtkTreeStore * store);
  void fill_angle_model_row (int p, int a, int b, int c, GtkTreeStore * store);
  void fill_dihedral_model_row (int p, int a, int b, int c, int d, GtkTreeStore * store);
  void update_selection_tree (glwin * view, int pi, int id);
  void update_label_selection (glwin * view, int pi);

  G_MODULE_EXPORT void measure_tree_button_event (GtkWidget * widget, double event_x, double event_y, guint event_button, gpointer data);
  G_MODULE_EXPORT void measure_tree_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
  G_MODULE_EXPORT void close_ml (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void measure_labels (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void window_measures (GtkWidget * widg, gpointer data);

  GtkWidget * create_selection_tree (glwin * view, int pi, int id);
  GtkWidget * measurment_tab (glwin * view, int pi, int id);

*/

#include "global.h"
#include "glview.h"

extern void bonds_loop (glwin * view, int id, int pi, GtkTreeStore * store);
extern void angles_loop (glwin * view, int id, int pi, GtkTreeStore * store);
extern void dihedrals_loop (glwin * view, int id, int pi, GtkTreeStore * store);
extern int num_bonds (int i);
extern int num_angles (int i);
extern int num_dihedrals (int i);

int bond_id;
int angle_id;
int dihedral_id;

/*
*  int get_measure_type (glwin * view)
*
*  Usage:
*
*  glwin * view : the target glwin
*/
int get_measure_type (glwin * view)
{
  if (view -> mode == EDITION && view -> selection_mode == NSELECTION-1)
  {
    return 1;
  }
  else if (view -> atom_win)
  {
    return 1;
  }
  else
  {
    return 0;
  }
}

/*
*  void fill_bond_model_row (int p, int a, int b, GtkTreeStore * store)
*
*  Usage:
*
*  int p                :
*  int a                :
*  int b                :
*  GtkTreeStore * store :
*/
void fill_bond_model_row (int p, int a, int b, GtkTreeStore * store)
{
  GtkTreeIter bond_level;
  gchar * str_a, * str_b, * str_c;
  struct project * this_proj = get_project_by_id(p);
  struct atom * at, * bt;
  gboolean pbc = this_proj -> cell.pbc;
  struct distance dist;
  int s = this_proj -> modelgl -> anim -> last -> img -> step;
  at = & this_proj -> atoms[s][a];
  bt = & this_proj -> atoms[s][b];
  bond_id ++;
  dist = distance_3d (& this_proj -> cell, (this_proj -> cell.npt) ? s : 0, at, bt);
  str_a = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[at -> sp], a+1);
  str_b = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[bt -> sp], b+1);
  str_c = g_strdup_printf("%.3lf", dist.length);
  if (pbc)
  {
    gtk_tree_store_append (store, & bond_level, NULL);
    gtk_tree_store_set (store, & bond_level, 0, bond_id,
                                             1, str_a,
                                             2, str_b,
                                             3, str_c,
                                             4, dist.pbc, -1);
  }
  else
  {
    gtk_tree_store_append (store, & bond_level, NULL);
    gtk_tree_store_set (store, & bond_level, 0, bond_id,
                                             1, str_a,
                                             2, str_b,
                                             3, str_c, -1);
  }
  g_free (str_a);
  g_free (str_b);
  g_free (str_c);
}

/*
*  void fill_angle_model_row (int p, int a, int b, int c, GtkTreeStore * store)
*
*  Usage:
*
*  int p                :
*  int a                :
*  int b                :
*  int c                :
*  GtkTreeStore * store :
*/
void fill_angle_model_row (int p, int a, int b, int c, GtkTreeStore * store)
{
  GtkTreeIter angle_level;
  gchar * str_a, * str_b, * str_c, * str_d;
  struct project * this_proj = get_project_by_id(p);
  struct atom * at, * bt, * ct;
  gboolean pbc = this_proj -> cell.pbc;
  struct angle theta;
  int s = this_proj -> modelgl -> anim -> last -> img -> step;
  at = & this_proj -> atoms[s][a];
  bt = & this_proj -> atoms[s][b];
  ct = & this_proj -> atoms[s][c];
  angle_id ++;
  theta = angle_3d (& this_proj -> cell, (this_proj -> cell.npt) ? s : 0, at, bt, ct);
  str_a = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[at -> sp], a+1);
  str_b = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[bt -> sp], b+1);
  str_c = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[ct -> sp], c+1);
  str_d = g_strdup_printf("%.2lf", theta.angle);
  if (pbc)
  {
    gtk_tree_store_append (store, & angle_level, NULL);
    gtk_tree_store_set (store, & angle_level, 0, angle_id,
                                              1, str_a,
                                              2, str_b,
                                              3, str_c,
                                              4, str_d,
                                              5, theta.pbc, -1);
  }
  else
  {
    gtk_tree_store_append (store, & angle_level, NULL);
    gtk_tree_store_set (store, & angle_level, 0, angle_id,
                                              1, str_a,
                                              2, str_b,
                                              3, str_c,
                                              4, str_d, -1);
  }
  g_free (str_a);
  g_free (str_b);
  g_free (str_c);
  g_free (str_d);
}

/*
*  void fill_dihedral_model_row (int p, int a, int b, int c, int d, GtkTreeStore * store)
*
*  Usage:
*
*  int p                :
*  int a                :
*  int b                :
*  int c                :
*  int d                :
*  GtkTreeStore * store :
*/
void fill_dihedral_model_row (int p, int a, int b, int c, int d, GtkTreeStore * store)
{
  GtkTreeIter dihedral_level;
  gchar * str_a, * str_b, * str_c, * str_d, * str_e;
  struct project * this_proj = get_project_by_id(p);
  struct atom * at, * bt, * ct, * dt;
  gboolean pbc = this_proj -> cell.pbc;
  struct angle phi;
  int s = this_proj -> modelgl -> anim -> last -> img -> step;
  at = & this_proj -> atoms[s][a];
  bt = & this_proj -> atoms[s][b];
  ct = & this_proj -> atoms[s][c];
  dt = & this_proj -> atoms[s][d];
  dihedral_id ++;
  phi = dihedral_3d (& this_proj -> cell, (this_proj -> cell.npt) ? s : 0, at, bt, ct, dt);
  str_a = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[at -> sp], a+1);
  str_b = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[bt -> sp], b+1);
  str_c = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[ct -> sp], c+1);
  str_d = g_strdup_printf("%s<sub>%d</sub>", this_proj -> chemistry -> label[dt -> sp], d+1);
  str_e = g_strdup_printf("%.2lf", phi.angle);
  if (pbc)
  {
    gtk_tree_store_append (store, & dihedral_level, NULL);
    gtk_tree_store_set (store, & dihedral_level, 0, dihedral_id,
                                                 1, str_a,
                                                 2, str_b,
                                                 3, str_c,
                                                 4, str_d,
                                                 5, str_e,
                                                 6, phi.pbc, -1);
  }
  else
  {
    gtk_tree_store_append (store, & dihedral_level, NULL);
    gtk_tree_store_set (store, & dihedral_level, 0, dihedral_id,
                                                 1, str_a,
                                                 2, str_b,
                                                 3, str_c,
                                                 4, str_d,
                                                 5, str_e, -1);
  }
  g_free (str_a);
  g_free (str_b);
  g_free (str_c);
  g_free (str_d);
  g_free (str_e);
}

extern ColRGBA init_color (int id, int numid);

void measure_set_visible (GtkTreeViewColumn * col,
                          GtkCellRenderer   * renderer,
                          GtkTreeModel      * mod,
                          GtkTreeIter       * iter,
                          gpointer          data)
{
  int i;
  tint * dat = (tint *)data;
  gtk_tree_model_get (mod, iter, dat -> b, & i, -1);
  gtk_cell_renderer_set_visible (renderer, i);
}

void measure_set_color (GtkTreeViewColumn * col,
                        GtkCellRenderer   * renderer,
                        GtkTreeModel      * mod,
                        GtkTreeIter       * iter,
                        tint * dat)
{
  int i, j;
  gboolean docolor = FALSE;
  gtk_tree_model_get (mod, iter, 0, & i, -1);
  struct project * this_proj = get_project_by_id(dat -> a);
  image * img = this_proj -> modelgl -> anim -> last -> img;
  int pi = get_measure_type (this_proj -> modelgl);
  if (img -> selected[pi] -> selected)
  {
    if ((dat -> c == 2 && img -> selected[pi] -> selected < MAX_IN_SELECTION-10) || (dat -> c < 2 && img -> selected[pi] -> selected < MAX_IN_SELECTION))
    {
      switch (dat -> c)
      {
        case 0:
          j = num_bonds (img -> selected[pi] -> selected);
          if (img -> selected[pi] -> selected_bonds[i]) docolor = TRUE;
          break;
        case 1:
          j = num_angles (img -> selected[pi] -> selected);
          if (img -> selected[pi] -> selected_angles[i]) docolor = TRUE;
          break;
        case 2:
          j = num_dihedrals (img -> selected[pi] -> selected);
          if (img -> selected[pi] -> selected_dihedrals[i]) docolor = TRUE;
          break;
      }
    }
  }
  set_renderer_color (docolor, renderer, init_color (i, j));
}

void dihedral_set_color_and_markup (GtkTreeViewColumn * col,
                                    GtkCellRenderer   * renderer,
                                    GtkTreeModel      * mod,
                                    GtkTreeIter       * iter,
                                    gpointer          data)
{
  tint dat = * ((tint *)data);
  dat.c = 2;
  measure_set_color (col, renderer, mod, iter, & dat);
  gchar * str = NULL;
  gtk_tree_model_get (mod, iter, dat.b, & str, -1);
  g_object_set (renderer, "markup", str, NULL, NULL);
  g_free (str);
}

void measure_set_color_and_markup (GtkTreeViewColumn * col,
                                   GtkCellRenderer   * renderer,
                                   GtkTreeModel      * mod,
                                   GtkTreeIter       * iter,
                                   gpointer          data)
{
  tint * dat = (tint *)data;
  measure_set_color (col, renderer, mod, iter, dat);
  gchar * str = NULL;
  gtk_tree_model_get (mod, iter, dat -> b, & str, -1);
  g_object_set (renderer, "markup", str, NULL, NULL);
  g_free (str);
}

GtkWidget * create_selection_tree (glwin * view, int pi, int id);

/*
*  void update_selection_tree (glwin * view, int pi, int id)
*
*  Usage:
*
*  glwin * view : the target glwin
*  int pi       :
*  int id       :
*/
void update_selection_tree (glwin * view, int pi, int id)
{
  GtkWidget * cont = gtk_widget_get_parent (view -> measure_win -> selection_tree[id]);
  view -> measure_win -> selection_tree[id] = destroy_this_widget (view -> measure_win -> selection_tree[id]);
  view -> measure_win -> selection_tree[id] = create_selection_tree (view, pi, id);
  add_container_child (CONTAINER_SCR, cont, view -> measure_win -> selection_tree[id]);
  show_the_widgets (cont);
}

/*
*  G_MODULE_EXPORT void measure_tree_button_event (GtkWidget * widget, double event_x, double event_y, guint event_button, gpointer data)
*
*  Usage:
*
*  GtkWidget * widget : the GtkWidget sending the signal
*  double event_x     :
*  double event_y     :
*  guint event_button :
*  gpointer data      : the associated data pointer
*/
G_MODULE_EXPORT void measure_tree_button_event (GtkWidget * widget, double event_x, double event_y, guint event_button, gpointer data)
{
  if (event_button == 1)
  {
    tint * dat = (tint *)data;
    glwin * view = get_project_by_id(dat -> a) -> modelgl;
    int pi = get_measure_type (view);
    GtkTreeModel * measure_model = gtk_tree_view_get_model(GTK_TREE_VIEW(widget));
    GtkTreePath * path;
    GtkTreeViewColumn * column;
    int i, j;
#ifdef GTK4
    int e_x, e_y;
    gtk_tree_view_convert_widget_to_bin_window_coords (GTK_TREE_VIEW(widget), event_x, event_y, & e_x, & e_y);
    if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW(widget), e_x, e_y, & path, & column, & i, & j))
#else
    if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW(widget), event_x, event_y, & path, & column, & i, & j))
#endif
    {
      GtkTreeIter row;
      if (gtk_tree_model_get_iter (measure_model, & row, path))
      {
        gtk_tree_model_get (measure_model, & row, 0, & j, -1);
        // select bonds, angles or dihedraks:
        switch (dat -> c)
        {
          case 0:
            view -> anim -> last -> img -> selected[pi] -> selected_bonds[j] = ! view -> anim -> last -> img -> selected[pi] -> selected_bonds[j];
            break;
          case 1:
            view -> anim -> last -> img -> selected[pi] -> selected_angles[j] = ! view -> anim -> last -> img -> selected[pi] -> selected_angles[j];
            break;
          case 2:
            view -> anim -> last -> img -> selected[pi] -> selected_dihedrals[j] = ! view -> anim -> last -> img -> selected[pi] -> selected_dihedrals[j];
            break;
        }
      }
    }
    view -> create_shaders[MEASU] = TRUE;
    update (view);
  }
}

#ifdef GTK3
/*
*  G_MODULE_EXPORT gboolean measure_tree_selection_event (GtkWidget * widget, GdkEventButton * event, gpointer data)
*
*  Usage:
*
*  GtkWidget * widget     : the GtkWidget sending the signal
*  GdkEventButton * event :
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT gboolean measure_tree_selection_event (GtkWidget * widget, GdkEventButton * event, gpointer data)
{
  if (event -> type == GDK_BUTTON_PRESS)
  {
    GdkEventButton * bevent = (GdkEventButton*)event;
    measure_tree_button_event (widget, bevent -> x, bevent -> y, bevent -> button, data);
  }
  return FALSE;
}
#else
/*
*  G_MODULE_EXPORT void measure_tree_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
*
*  Usage:
*
*  GtkGesture * gesture : the GtkGesture sending the signal
*  int n_press          : number of times it was pressed
*  double x             : x position
*  double y             : y position
*  gpointer data        : the associated data pointer
*/
G_MODULE_EXPORT void measure_tree_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  measure_tree_button_event (gtk_event_controller_get_widget ((GtkEventController*)gesture), x, y, gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), data);
}
#endif

/*
*  GtkWidget * create_selection_tree (glwin * view, int pi, int id)
*
*  Usage:
*
*  glwin * view : the target glwin
*  int pi       :
*  int id       :
*/
GtkWidget * create_selection_tree (glwin * view, int pi, int id)
{
  int i, j;
  GtkTreeViewColumn * sel_col[7];
  GtkCellRenderer * sel_cell[7];
  GtkTreeStore * sel_model;
  gchar * ctitle[3][7]={{"Id", "Atom 1", "Atom 2", "Distance [Å]", "Using PBC", "NULL", "NULL"},
                        {"Id", "Atom 1", "Atom 2", "Atom 3", "θ [°]", "Using PBC", "NULL"},
                        {"Id", "Atom 1", "Atom 2", "Atom 3", "Atom 4", "ϕ [°]", "Using PBC"}};
  gchar * ctype[3][7]={{"text", "text", "text", "text", "active", "text", "text"},
                       {"text", "text", "text", "text", "text", "active", "text"},
                       {"text", "text", "text", "text", "text", "text", "active"}};
  int tree_dim[3]={4, 5, 6};
  GType col_type[3][7]= {{G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_STRING},
                         {G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_STRING},
                         {G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN}};
  if (get_project_by_id(view -> proj) -> cell.pbc)
  {
    sel_model = gtk_tree_store_newv (tree_dim[id]+1, col_type[id]);
    j = 1;
  }
  else
  {
    sel_model = gtk_tree_store_newv (tree_dim[id], col_type[id]);
    j = 0;
  }

  GtkWidget * selection_tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(sel_model));
  for (i=0; i<4+j+id; i++)
  {
    if ((id == 0 && i < 4) || (id == 1 && i < 5) || (id == 2 && i < 6))
    {
      sel_cell[i] = gtk_cell_renderer_text_new();
      sel_col[i] = gtk_tree_view_column_new_with_attributes (ctitle[id][i], sel_cell[i], ctype[id][i], i, NULL);
      if (i == 0) gtk_tree_view_column_set_visible (sel_col[i], FALSE);
    }
    else if ((id == 0 && i == 4) || (id == 1 && i == 5) || (id == 2 && i == 6))
    {
      sel_cell[i] = gtk_cell_renderer_toggle_new ();
      sel_col[i] = gtk_tree_view_column_new_with_attributes (ctitle[id][i], sel_cell[i], ctype[id][i], i, NULL);
    }
    gtk_tree_view_column_set_alignment (sel_col[i], 0.5);
    gtk_tree_view_append_column(GTK_TREE_VIEW(selection_tree), sel_col[i]);
    if (i > 0 && i < 4+id)
    {
      if (id < 2)
      {
        gtk_tree_view_column_set_cell_data_func (sel_col[i], sel_cell[i], measure_set_color_and_markup, & view -> colorp[i][id], NULL);
      }
      else
      {
        gtk_tree_view_column_set_cell_data_func (sel_col[i], sel_cell[i], dihedral_set_color_and_markup, & view -> colorp[i][0], NULL);
      }
    }
    else if (i > 0)
    {
      gtk_tree_view_column_set_cell_data_func (sel_col[i], sel_cell[i], measure_set_visible, & view -> colorp[i][0], NULL);
    }
  }
  image * img = view -> anim -> last -> img;
  if (img -> selected[pi] -> selected < MAX_IN_SELECTION)
  {
    if ( (id == 0 && img -> selected[pi] -> selected > 1)
      || (id == 1 && img -> selected[pi] -> selected > 2)
      || (id == 2 && img -> selected[pi] -> selected > 3 && img -> selected[pi] -> selected < MAX_IN_SELECTION-10))
    {
      if (id == 0)
      {
        bond_id = -1;
        bonds_loop (view, 2, pi, GTK_TREE_STORE(sel_model));
#ifdef GTK4
        add_widget_gesture_and_key_action (selection_tree, "bond-tree-pressed", G_CALLBACK(measure_tree_button_pressed), & view -> colorp[0][id],
                                           NULL, NULL, NULL, NULL, NULL, NULL,
                                           NULL, NULL, NULL, NULL, NULL, NULL);
#else
        g_signal_connect (G_OBJECT(selection_tree), "button_press_event", G_CALLBACK(measure_tree_selection_event), & view -> colorp[0][id]);
#endif
      }
      else if (id == 1)
      {
        angle_id = -1;
        angles_loop (view, 2, pi, GTK_TREE_STORE(sel_model));
#ifdef GTK4
        add_widget_gesture_and_key_action (selection_tree, "angle-tree-pressed", G_CALLBACK(measure_tree_button_pressed), & view -> colorp[0][id],
                                           NULL, NULL, NULL, NULL, NULL, NULL,
                                           NULL, NULL, NULL, NULL, NULL, NULL);
#else
        g_signal_connect (G_OBJECT(selection_tree), "button_press_event", G_CALLBACK(measure_tree_selection_event), & view -> colorp[0][id]);
#endif
      }
      else if (id == 2)
      {
        dihedral_id = -1;
        dihedrals_loop (view, 2, pi, GTK_TREE_STORE(sel_model));
/*#ifdef GTK4
        add_widget_gesture_and_key_action (selection_tree, "dihedral-tree-pressed", G_CALLBACK(measure_tree_button_pressed), & view -> colorp[0][id],
                                           NULL, NULL, NULL, NULL, NULL, NULL,
                                           NULL, NULL, NULL, NULL, NULL, NULL);
#else
        g_signal_connect (G_OBJECT(selection_tree), "button_press_event", G_CALLBACK(measure_tree_selection_event), & view -> colorp[0][id]);
#endif*/
      }
    }
  }
  GtkTreeSelection *  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(selection_tree));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_NONE);
  gtk_tree_view_expand_all (GTK_TREE_VIEW(selection_tree));
  return selection_tree;
}

/*
*  GtkWidget * measurment_tab (glwin * view, int pi, int id)
*
*  Usage:
*
*  glwin * view : the target glwin
*  int pi       :
*  int id       :
*/
GtkWidget * measurment_tab (glwin * view, int pi, int id)
{
  GtkWidget * scrollsets = create_scroll (NULL, 350+50*id, 350, GTK_SHADOW_NONE);
  view -> measure_win -> selection_tree[id] = create_selection_tree (view, pi, id);
  add_container_child (CONTAINER_SCR, scrollsets, view -> measure_win -> selection_tree[id]);
  return scrollsets;
}

/*
*  gchar * create_measure_label (glwin * view, int pi)
*
*  Usage:
*
*  glwin * view : the target glwin
*  int pi       :
*/
gchar * create_measure_label (glwin * view, int pi)
{
  gchar * str;
  image * img = view -> anim -> last -> img;
  if (img -> selected[pi] -> selected < MAX_IN_SELECTION)
  {
    str = g_strdup_printf ("\tAtom(s) in selection<sup>*</sup> :\t<b>%d</b>", img -> selected[pi] -> selected);
  }
  else
  {
    str = g_strdup_printf ("\tSorry too many [%d] atoms in selection<sup>*</sup>", img -> selected[pi] -> selected);
  }
  return str;
}

/*
*  void update_label_selection (glwin * view, int pi)
*
*  Usage:
*
*  glwin * view : the target glwin
*  int pi       :
*/
void update_label_selection (glwin * view, int pi)
{
  gtk_label_set_text (GTK_LABEL(view -> measure_win -> label), create_measure_label(view, pi));
  gtk_label_set_use_markup (GTK_LABEL(view -> measure_win -> label), TRUE);
}

extern GtkWidget * labels_tab (glwin * view, int id);

/*
*  G_MODULE_EXPORT void close_ml (GtkButton * but, gpointer data)
*
*  Usage:
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void close_ml (GtkButton * but, gpointer data)
{
  destroy_this_widget (GTK_WIDGET(data));
}

/*
*  G_MODULE_EXPORT void measure_labels (GtkButton * but, gpointer data)
*
*  Usage:
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void measure_labels (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  gchar * str = g_strdup_printf ("%s - measures - style", get_project_by_id(view -> proj) -> name);
  GtkWidget * win = create_win (str, view -> measure_win -> win, TRUE, FALSE);
  //gtk_widget_set_size_request (win, -1, -1480);
  g_free (str);
  GtkWidget * vbox = create_vbox (BSEP);
  add_container_child (CONTAINER_WIN, win, vbox);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, labels_tab (view, (is_atom_win_active(view) || (view -> mode == EDITION && view -> selection_mode == NSELECTION-1)) ? 4 : 3), FALSE, FALSE, 0);
  GtkWidget * hbox = create_hbox (0);
  GtkWidget * close_but = create_button ("Close", IMG_NONE, NULL, 100, -1, GTK_RELIEF_NORMAL, G_CALLBACK(close_ml), win);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, close_but, FALSE, TRUE, 150);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 20);
  add_gtk_close_event (win, G_CALLBACK(hide_this_window), NULL);
  show_the_widgets (win);
  gtk_window_set_resizable (GTK_WINDOW(win), FALSE);
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT gboolean close_measure_event (GtkWindow * widg, gpointer data)
*
*  Usage:
*
*  GtkWindow * widg :
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean close_measure_event (GtkWindow * widg, gpointer data)
#else
/*
*  G_MODULE_EXPORT gboolean close_measure_event (GtkWidget * widg, GdkEvent * event, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  GdkEvent * event : the GdkEvent triggering the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean close_measure_event (GtkWidget * widg, GdkEvent * event, gpointer data)
#endif
{
  glwin * view = (glwin *) data;
  view -> measure_win -> win = destroy_this_widget (view -> measure_win -> win);
  int i;
  for (i=0; i<3; i++)
  {
    view -> measure_win -> selection_tree[i] = NULL;
  }
  g_free (view -> measure_win);
  view -> measure_win = NULL;
  return FALSE;
}

/*
*  G_MODULE_EXPORT void window_measures (GtkWidget * widg, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void window_measures (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *) data;
  if (view -> measure_win == NULL)
  {
    view -> measure_win = g_malloc0 (sizeof*view -> measure_win);
    gchar * str = g_strdup_printf ("%s - measures", get_project_by_id(view -> proj) -> name);
    int pi = get_measure_type (view);
    view -> measure_win -> win = create_win (str, view -> win, FALSE, FALSE);
    gtk_widget_set_size_request (view -> measure_win -> win, 450, 500);
    g_free (str);
    GtkWidget * vbox = create_vbox (BSEP);
    add_container_child (CONTAINER_WIN, view -> measure_win -> win, vbox);
    GtkWidget * notebook = gtk_notebook_new ();
    view -> measure_win -> label = markup_label(create_measure_label(view, pi), -1, 50, 0.0, 0.5);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, view -> measure_win -> label, FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, notebook, FALSE, FALSE, 0);
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), measurment_tab (view, pi, 0), gtk_label_new ("Distances"));
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), measurment_tab (view, pi, 1), gtk_label_new ("Angles"));
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), measurment_tab (view, pi, 2), gtk_label_new ("Dihedrals"));
    str = g_strdup_printf (" <sub>* You can select up to %d atoms for both inter-atomic distance(s) and angle(s),\n"
                           "     and up to %d atoms for dihedral angle(s) measurement(s)</sub>", MAX_IN_SELECTION-1, MAX_IN_SELECTION-11);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, markup_label(str, -1, -1, 0.0, 0.5), FALSE, FALSE, 0);
    g_free (str);
    GtkWidget * hbox = create_hbox (0);
    GtkWidget * but = create_button ("Font and style", IMG_NONE, NULL, 150, -1, GTK_RELIEF_NORMAL, G_CALLBACK(measure_labels), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, but, FALSE, TRUE, 150);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 10);
    add_gtk_close_event (view -> measure_win -> win,  G_CALLBACK(close_measure_event), view);
    show_the_widgets (view -> measure_win -> win);
  }
  else
  {
    gtk_widget_show (view -> measure_win -> win);
  }
}
