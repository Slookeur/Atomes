/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include <gtk/gtk.h>
#include <cairo.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "global.h"
#include "interface.h"
#include "callbacks.h"
#include "project.h"
#include "curve.h"

int nrows;
GtkTreeModel * curve_model;
GList * lrows;
GList * rows;
GtkTreeSelection * sel;
GtkTreePath * path;
GtkTreeIter row;
gchar * text;

void get_tree_data (GtkWidget * tree)
{
  sel = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
  curve_model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree));
  lrows = gtk_tree_selection_get_selected_rows (sel, & curve_model);
  nrows = gtk_tree_selection_count_selected_rows (sel);
}

void save_row (gpointer data, gpointer user_data)
{
  path = data;
  tint * id = (tint *)user_data;
  if (gtk_tree_model_get_iter (curve_model, & row, path))
  {

    gtk_tree_model_get (curve_model, & row, 1, & get_project_by_id(id -> a) -> curves[id -> b][id -> c] -> data[0][nrows],
                                            2, & get_project_by_id(id -> a) -> curves[id -> b][id -> c] -> data[1][nrows], -1);
  }
  nrows = nrows + 1;
}

void update_first_row (gpointer data, gpointer user_data)
{
  path = data;
  if (gtk_tree_model_get_iter (curve_model, & row, path))
  {
    nrows = nrows + 1;
    gtk_list_store_set (GTK_LIST_STORE(curve_model), & row, 0, nrows, -1);
  }
}

void update_first_col ()
{
  gtk_tree_selection_select_all (sel);
  lrows = gtk_tree_selection_get_selected_rows (sel, & curve_model);
  nrows = 0;
  g_list_foreach (lrows, (GFunc)update_first_row, NULL);
  gtk_tree_selection_unselect_all (sel);
}

void add_to_last_row (gpointer data, gpointer user_data)
{
  path = data;
  double vold;
  double * cte = (double *)user_data;
  if (gtk_tree_model_get_iter (curve_model, & row, path))
  {
    gtk_tree_model_get (curve_model, & row, 2, & vold, -1);
    gtk_list_store_set (GTK_LIST_STORE(curve_model), & row, 2, * cte + vold, -1);
  }
}

void add_to_last_col (double cte, gpointer data)
{
  qint * dat = (qint *)data;
  struct project * this_proj = get_project_by_id(dat -> a);
  curve_model = gtk_tree_view_get_model (GTK_TREE_VIEW(this_proj -> curves[dat -> b][dat -> c] -> datatree));
  sel = gtk_tree_view_get_selection (GTK_TREE_VIEW(this_proj -> curves[dat -> b][dat -> c] -> datatree));
  gtk_tree_selection_select_all (sel);
  get_tree_data (this_proj -> curves[dat -> b][dat -> c] -> datatree);
  g_list_foreach (lrows, (GFunc)add_to_last_row, & cte);
  gtk_tree_selection_unselect_all (sel);
}

void multiply_last_row (gpointer data, gpointer user_data)
{
  path = data;
  double vold;
  double * cte = (double *) user_data;
  if (gtk_tree_model_get_iter (curve_model, & row, path))
  {
    gtk_tree_model_get (curve_model, & row, 2, & vold, -1);
    gtk_list_store_set (GTK_LIST_STORE(curve_model), & row, 2, * cte * vold, -1);
  }
}

void multiply_last_col (double cte, gpointer data)
{
  qint * dat = (qint *)data;
  struct project * this_proj = get_project_by_id(dat -> a);
  curve_model = gtk_tree_view_get_model (GTK_TREE_VIEW(this_proj -> curves[dat -> b][dat -> c] -> datatree));
  sel = gtk_tree_view_get_selection (GTK_TREE_VIEW(this_proj -> curves[dat -> b][dat -> c] -> datatree));
  gtk_tree_selection_select_all (sel);
  get_tree_data (this_proj -> curves[dat -> b][dat -> c] -> datatree);
  g_list_foreach (lrows, (GFunc)multiply_last_row, & cte);
  gtk_tree_selection_unselect_all (sel);
}

void select_row (gpointer data, gpointer user_data)
{
  path = data;
  if (gtk_tree_model_get_iter (curve_model, & row, path))
  {
    gtk_tree_selection_select_iter (sel, & row);
  }
}

void copy_row (gpointer data, gpointer user_data)
{
  path = data;
  int  v1;
  double v2, v3;
  gchar * tmp;

  if (gtk_tree_model_get_iter (curve_model, & row, path))
  {
    gtk_tree_model_get (curve_model, & row, 0, & v1, 1, & v2, 2, & v3, -1);
    if (text == NULL)
    {
      text = g_strdup_printf ("%d\t%f\t%f\n", v1, v2, v3);
    }
    else
    {
      tmp = g_strdup_printf ("%s%d\t%f\t%f\n", text, v1, v2, v3);
      g_free (text);
      text = g_strdup_printf ("%s", tmp);
      g_free (tmp);
    }
  }
}

void copy_content (gpointer data)
{
  text = NULL;
  g_list_foreach (lrows, (GFunc)copy_row, NULL);
#ifdef GTK3
  gtk_clipboard_set_text (gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), text, -1);
  gtk_clipboard_store (gtk_clipboard_get(GDK_SELECTION_CLIPBOARD));
#endif
  g_free (text);
}

void add_row (gpointer data, gpointer user_data)
{
  GtkTreeIter newrow;
  if (gtk_tree_model_get_iter (curve_model, & row, path))
  {
    if (GPOINTER_TO_INT(user_data) == 0)
    {
      gtk_list_store_insert_before (GTK_LIST_STORE(curve_model), & newrow, & row);
    }
    else
    {
      gtk_list_store_insert_after (GTK_LIST_STORE(curve_model), & newrow, & row);
    }
    gtk_list_store_set (GTK_LIST_STORE(curve_model), & newrow, 0, 0, 1, 0.00000, 2, 0.00000, -1);
  }
}

void delete_row (gpointer data, gpointer user_data)
{
  path = data;
  if (gtk_tree_model_get_iter (curve_model, & row, path))
  {
    gtk_list_store_remove (GTK_LIST_STORE(curve_model), & row);
  }
}

void insert_cell (gpointer data)
{
  if (GPOINTER_TO_INT(data) == 0)
  {
    path = (GtkTreePath *) g_list_nth_data (lrows, 0);
  }
  else
  {
    path = (GtkTreePath *) g_list_nth_data (lrows, nrows-1);
  }
  g_list_foreach (lrows, (GFunc)add_row, data);
  update_first_col ();
}

void delete_cell (gpointer data)
{
  g_list_foreach (g_list_reverse (lrows), (GFunc)delete_row, NULL);
  update_first_col ();
}

G_MODULE_EXPORT void edit_cell (GtkCellRendererText * cell,
                                gchar * path_string,
                                gchar * new_text,
                                gpointer user_data)
{
  qint * id = (qint *)user_data;
  struct project * this_proj = get_project_by_id (id -> a);
  curve_model = gtk_tree_view_get_model(GTK_TREE_VIEW(this_proj -> curves[id -> b][id -> c] -> datatree));
  gtk_tree_model_get_iter_from_string (curve_model, & row, path_string);
  double val = atof(new_text);
  gtk_list_store_set (GTK_LIST_STORE(curve_model), & row, id -> d, val, -1);
}

GtkWidget * col_entry;

G_MODULE_EXPORT void adjust_value (GtkEntry * res, gpointer data)
{
  const gchar * m = entry_get_text (res);
  double v = atof(m);
  update_entry_double (res, v);
}

G_MODULE_EXPORT void run_add_to_column (GtkDialog * wind, gint response_id, gpointer data)
{
  if (response_id == GTK_RESPONSE_APPLY)
  {
    const gchar * p = entry_get_text (GTK_ENTRY(col_entry));
    double c = atof(p);
    update_entry_double (GTK_ENTRY(col_entry), c);
    add_to_last_col (c, data);
  }
  destroy_this_dialog (wind);
}

void add_to_column (gpointer data)
{
  GtkWidget * wind;
  GtkWidget * box;
  GtkWidget * hbox;
  GtkWidget * lab;
  gchar * str;
  qint * dat = (qint *)data;
  wind = dialogmodal ("Add constant to column", GTK_WINDOW(get_project_by_id(dat -> a) -> curves[dat -> b][dat -> c] -> window));
  gtk_dialog_add_button (GTK_DIALOG (wind), "Apply", GTK_RESPONSE_APPLY);
  box = dialog_get_content_area (wind);
  hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box, hbox, TRUE, TRUE, 0);

  str = g_strdup_printf ("Add Constant to Last Column");
  lab = gtk_label_new (str);
  gtk_widget_set_size_request (lab, 200, -1);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, lab, TRUE, FALSE, 0);
  col_entry = create_entry (G_CALLBACK(adjust_value), 100, 15, FALSE, NULL);
  gtk_entry_set_alignment (GTK_ENTRY(col_entry), 1.0);
  update_entry_double (GTK_ENTRY(col_entry), 1.0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, col_entry, FALSE, FALSE, 0);
  run_this_gtk_dialog (wind, G_CALLBACK(run_add_to_column), data);
}

G_MODULE_EXPORT void run_multiply_column (GtkDialog * wind, gint response_id, gpointer data)
{
  if (response_id == GTK_RESPONSE_APPLY)
  {
    const gchar * p = entry_get_text (GTK_ENTRY(col_entry));
    double c = atof(p);
    update_entry_double (GTK_ENTRY(col_entry), c);
    multiply_last_col (c, data);
  }
  destroy_this_dialog (wind);
}

void multiply_column (gpointer data)
{
  GtkWidget * wind;
  GtkWidget * box;
  GtkWidget * hbox;
  GtkWidget * lab;
  gchar * str;
  qint * dat = (qint *)data;
  wind = dialogmodal ("Multiply Column by Constant", GTK_WINDOW(get_project_by_id(dat -> a) -> curves[dat -> b][dat -> c] -> window));
  gtk_dialog_add_button (GTK_DIALOG (wind), "Apply", GTK_RESPONSE_APPLY);
  box = dialog_get_content_area (wind);
  hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box, hbox, TRUE, TRUE, 0);

  str = g_strdup_printf ("Multiply Last Column by Constant");
  lab = gtk_label_new (str);
  gtk_widget_set_size_request (lab, 200, -1);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, lab, TRUE, FALSE, 0);
  col_entry = create_entry (G_CALLBACK(adjust_value), 100, 15, FALSE, NULL);
  gtk_entry_set_alignment (GTK_ENTRY(col_entry), 1.0);
  update_entry_double (GTK_ENTRY(col_entry), 1.0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, col_entry, FALSE, FALSE, 0);
  run_this_gtk_dialog (wind, G_CALLBACK(run_multiply_column), data);
}

GMenu * insert_place ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Before", "data-pop.insert.before", NULL, NULL, IMG_STOCK, (gpointer)GO_UP, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "After", "data-pop.insert.after", NULL, NULL, IMG_STOCK, (gpointer)GO_DOWN, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu * insert_data ()
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Insert Row(s)", (GMenuModel*)insert_place());
  return menu;
}

GMenu * delete_data ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Delete Row(s)", "data-pop.delete", NULL, NULL, IMG_STOCK, (gpointer)LIST_REM, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu * cell_title ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Cell Based Operations", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu* cell_actions ()
{
  GMenu * menu = g_menu_new ();
  if (! registered_atomes || testing_atomes)
  {
    append_menu_item (menu, "Copy Selected Row(s)", "None", NULL, NULL, IMG_STOCK, (gpointer)EDITC, FALSE, FALSE, FALSE, NULL);
  }
  else
  {
    append_menu_item (menu, "Copy Selected Row(s)", "data-pop.copy", NULL, NULL, IMG_STOCK, (gpointer)EDITC, FALSE, FALSE, FALSE, NULL);
  }
  g_menu_append_section (menu, NULL, (GMenuModel*)insert_data());
  g_menu_append_section (menu, NULL, (GMenuModel*)delete_data());
  return menu;
}

GMenu * column_actions ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Add Constant to Last Column", "data-pop.add", NULL, NULL, IMG_STOCK, (gpointer)LIST_ADD, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "Multiply Last Column by Constant", "data-pop.mul", NULL, NULL, IMG_STOCK, (gpointer)FCLOSE, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu * column_title ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Column Based Operations", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu * data_menu ()
{
  GMenu * menu = g_menu_new ();
  g_menu_append_section (menu, NULL, (GMenuModel*)cell_title());
  g_menu_append_section (menu, NULL, (GMenuModel*)cell_actions());
  g_menu_append_section (menu, NULL, (GMenuModel*)column_title());
  g_menu_append_section (menu, NULL, (GMenuModel*)column_actions());
  return menu;
}

G_MODULE_EXPORT void data_pop_action (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  gchar * name = g_strdup_printf ("%s", g_action_get_name(G_ACTION(action)));
  if (g_strcmp0 (name, "copy") == 0)
  {
    copy_content (data);
  }
  else if (g_strcmp0 (name, "insert.before") == 0)
  {
    insert_cell (data);
  }
  else if (g_strcmp0 (name, "insert.after") == 0)
  {
    insert_cell (data);
  }
  else if (g_strcmp0 (name, "delete") == 0)
  {
    delete_cell (data);
  }
  else if (g_strcmp0 (name, "add") == 0)
  {
    add_to_column (data);
  }
  else if (g_strcmp0 (name, "mul") == 0)
  {
    multiply_column (data);
  }
}

#ifdef GTK4
void data_popup_menu (GtkWidget * top_level, double x, double y, gpointer data)
#else
void data_popup_menu (GtkWidget * top_level, GdkEvent * event, gpointer data)
#endif
{
  GSimpleActionGroup * data_popup = g_simple_action_group_new ();
  GSimpleAction * pop_data[6];
  pop_data[0]  = g_simple_action_new ("copy", NULL);
  pop_data[1]  = g_simple_action_new ("insert.before", NULL);
  pop_data[2]  = g_simple_action_new ("insert.after", NULL);
  pop_data[3]  = g_simple_action_new ("delete", NULL);
  pop_data[4]  = g_simple_action_new ("add", NULL);
  pop_data[5]  = g_simple_action_new ("mul", NULL);

  g_signal_connect (pop_data[0], "activate", G_CALLBACK(data_pop_action), NULL);
  g_signal_connect (pop_data[1], "activate", G_CALLBACK(data_pop_action), GINT_TO_POINTER(0));
  g_signal_connect (pop_data[2], "activate", G_CALLBACK(data_pop_action), GINT_TO_POINTER(1));
  g_signal_connect (pop_data[3], "activate", G_CALLBACK(data_pop_action), NULL);
  g_signal_connect (pop_data[4], "activate", G_CALLBACK(data_pop_action), data);
  g_signal_connect (pop_data[5], "activate", G_CALLBACK(data_pop_action), data);

  int i;
  for (i=0; i<6; i++) g_action_map_add_action (G_ACTION_MAP(data_popup), G_ACTION(pop_data[i]));

  GtkWidget * menu;
#ifdef GTK4
  menu = gtk_popover_menu_new_from_model_full ((GMenuModel *)data_menu(), GTK_POPOVER_MENU_NESTED);
  gtk_widget_set_parent  (menu, top_level);
#else
  menu = gtk_menu_new_from_model ((GMenuModel *)data_menu ());
#endif
  gtk_widget_insert_action_group (menu, "data-pop", G_ACTION_GROUP(data_popup));

#ifdef GTK4
  gtk_widget_set_size_request (menu, -1, 240);
  pop_menu_at_pointer (menu, x, y);
#else
  pop_menu_at_pointer (menu, event);
#endif
}

#ifdef GTK4
void data_button_event (GtkWidget * data_tree, double event_x, double event_y, guint event_button, guint event_type, gpointer data)
#else
void data_button_event (GtkWidget * data_tree, GdkEvent * event, guint event_button, guint event_type, gpointer data)
#endif
{
  if (event_type == GDK_BUTTON_PRESS)
  {
    get_tree_data (data_tree);
    if (event_button == 3)
    {
#ifdef GTK4
      data_popup_menu ((GtkWidget *)get_top_level(data_tree), event_x, event_y, data);
#else
      data_popup_menu (data_tree, event, data);
#endif
    }
  }
  else if (event_type == GDK_BUTTON_RELEASE)
  {
    g_list_foreach (lrows, (GFunc)gtk_tree_path_free, NULL);
    g_list_free (lrows);
  }
}

#ifdef GTK4
G_MODULE_EXPORT void on_data_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  data_button_event (gtk_event_controller_get_widget ((GtkEventController*)gesture), x, y,
                     gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), GDK_BUTTON_PRESS, data);
}

G_MODULE_EXPORT void on_data_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  data_button_event (gtk_event_controller_get_widget ((GtkEventController*)gesture), x, y,
                     gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), GDK_BUTTON_RELEASE, data);
}
#else
G_MODULE_EXPORT gboolean on_data_button_event (GtkWidget * widget, GdkEvent * event, gpointer data)
{
  GdkEventButton * bevent = (GdkEventButton*)event;
  data_button_event (widget, event, bevent -> button, bevent -> type, data);
  return FALSE;
}
#endif

static void fill_data_model (GtkListStore * store, struct project * this_proj, int b, int c)
{
  GtkTreeIter datalevel;
  int i;
  for (i=0; i<this_proj -> curves[b][c] -> ndata; i++)
  {
    gtk_list_store_append (store, & datalevel);
    gtk_list_store_set (store, & datalevel,
                        0, i+1,
                        1, this_proj -> curves[b][c] -> data[0][i],
                        2, this_proj -> curves[b][c] -> data[1][i], -1);
  }
}

GtkWidget * setview (struct project * this_proj, int b, int c)
{
  GtkWidget * dataview;
  GtkListStore * datamodel;
  GType type[3]={G_TYPE_INT, G_TYPE_DOUBLE, G_TYPE_DOUBLE};
  GtkTreeViewColumn * datacol[3];
  GtkCellRenderer * datacel[3];
  GtkTreeSelection * dataselect;
//  ColRGBA col[3];
  gchar * name[3];
  int i;
  name[0]=g_strdup_printf (" ");
  name[1]=g_strdup_printf ("%s", this_proj -> curves[b][c] -> axis_title[0]);
  name[2]=g_strdup_printf ("%s\n%s", prepare_for_title(this_proj -> name), this_proj -> curves[b][c] -> name);
  datamodel = gtk_list_store_newv (3, type);
  fill_data_model (datamodel, this_proj, b, c);
  dataview = gtk_tree_view_new_with_model (GTK_TREE_MODEL(datamodel));
  //gtk_tree_view_set_rules_hint (GTK_TREE_VIEW(dataview), TRUE);
  for (i=0; i<3; i++)
  {
    datacel[i] = gtk_cell_renderer_text_new();
 /*   if (i == 1)
    {
      col[i].red = 62965;
      col[i].green = 62965;
      col[i].blue  = 62965;
    }
    else
    {
      col[i].red = 51914;
      col[i].green = 57825;
      col[i].blue = 65535;
    }
    g_object_set (datacel[i], "cell-background-gdk", & col[i], "cell-background-set", TRUE, NULL); */
    if (i > 0)
    {
      g_object_set (datacel[i], "editable", TRUE, NULL);
      gtk_cell_renderer_set_alignment (datacel[i], 0.5, 0.5);
      this_proj -> curves[b][c] -> idcol[i-1].a = this_proj -> id;
      this_proj -> curves[b][c] -> idcol[i-1].b = b;
      this_proj -> curves[b][c] -> idcol[i-1].c = c;
      this_proj -> curves[b][c] -> idcol[i-1].d = i;
      g_signal_connect (G_OBJECT(datacel[i]), "edited", G_CALLBACK(edit_cell), & this_proj -> curves[b][c] -> idcol[i-1]);
    }
    datacol[i] = gtk_tree_view_column_new_with_attributes(name[i], datacel[i], "text", i, NULL);
    gtk_tree_view_column_set_alignment (datacol[i], 0.5);
    gtk_tree_view_column_set_resizable (datacol[i], TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(dataview), datacol[i]);
    if (i > 0)
    {
      gtk_tree_view_column_set_min_width (datacol[i], 100);
    }
  }
  g_object_unref (datamodel);
  dataselect = gtk_tree_view_get_selection (GTK_TREE_VIEW(dataview));
  gtk_tree_selection_set_mode (dataselect, GTK_SELECTION_MULTIPLE);
#ifdef GTK3
  g_signal_connect (G_OBJECT(dataview), "button_press_event", G_CALLBACK(on_data_button_event), & this_proj -> curves[b][c] -> idcol[1]);
#else
  add_widget_gesture_and_key_action (dataview, "datab-context-click", G_CALLBACK(on_data_button_pressed), & this_proj -> curves[b][c] -> idcol[1],
                                               "datab-context-release", G_CALLBACK(on_data_button_released), & this_proj -> curves[b][c] -> idcol[1],
                                                NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
#endif
  gtk_tree_view_expand_all (GTK_TREE_VIEW(dataview));
  return dataview;
}

void cancel_changes (GtkWidget * widg, gpointer data)
{
  tint * id = (tint *)data;
  destroy_this_widget (widg);
  get_project_by_id(id -> a) -> curves[id -> b][id -> c] -> datatree = NULL;
}

G_MODULE_EXPORT void cancel_but (GtkButton * but, gpointer data)
{
  cancel_changes (get_top_level(GTK_WIDGET(but)), data);
}

#ifdef GTK4
G_MODULE_EXPORT gboolean cancel_win (GtkWindow * win, gpointer data)
#else
G_MODULE_EXPORT gboolean cancel_win (GtkWidget * win, GdkEvent * event, gpointer data)
#endif
{
  cancel_changes ((GtkWidget *)win, data);
  return TRUE;
}

G_MODULE_EXPORT void validate_changes (GtkButton * but, gpointer data)
{
  tint * id = (tint *)data;
  struct project * this_proj = get_project_by_id(id -> a);
  curve_model = gtk_tree_view_get_model(GTK_TREE_VIEW(this_proj -> curves[id -> b][id -> c] -> datatree));
  sel = gtk_tree_view_get_selection (GTK_TREE_VIEW(this_proj -> curves[id -> b][id -> c] -> datatree));
  gtk_tree_selection_select_all (sel);
  get_tree_data (this_proj -> curves[id -> b][id -> c] -> datatree);
  if (nrows != this_proj -> curves[id -> b][id -> c] -> ndata)
  {
    this_proj -> curves[id -> b][id -> c] -> ndata = nrows;
    g_free (this_proj -> curves[id -> b][id -> c] -> data[0]);
    this_proj -> curves[id -> b][id -> c] -> data[0] = g_malloc0 (nrows*sizeof*this_proj -> curves[id -> b][id -> c] -> data[0]);
    g_free (this_proj -> curves[id -> b][id -> c] -> data[1]);
    this_proj -> curves[id -> b][id -> c] -> data[1] = g_malloc0 (nrows*sizeof*this_proj -> curves[id -> b][id -> c] -> data[1]);
  }
  nrows = 0;
  g_list_foreach (lrows, (GFunc)save_row, data);
  cancel_changes (get_top_level(GTK_WIDGET(but)), data);
  update_curves ();
}

G_MODULE_EXPORT void edit_data (GtkWidget * but, gpointer data)
{
  GtkWidget * win;
  tint * id = (tint *)data;
  struct project * this_proj = get_project_by_id(id -> a);
  if (this_proj -> curves[id -> b][id -> c] -> datatree != NULL)
  {
    win = get_top_level(GTK_WIDGET(this_proj -> curves[id -> b][id -> c] -> datatree));
  }
  else
  {
    gchar * str = g_strdup_printf ("%s - %s", prepare_for_title (this_proj -> name), this_proj -> curves[id -> b][id -> c] -> name);
    win = create_win (str, MainWindow, FALSE, TRUE);
    g_free (str);
    gtk_window_set_default_size (GTK_WINDOW(win), 300, 600);
    GtkWidget * vbox = create_vbox (BSEP);
    this_proj -> curves[id -> b][id -> c] -> datatree = setview (this_proj, id -> b, id -> c);
    GtkWidget * scrol = create_scroll (vbox, -1, 570, GTK_SHADOW_ETCHED_IN);
    add_container_child (CONTAINER_SCR, scrol, this_proj -> curves[id -> b][id -> c] -> datatree);
    GtkWidget * hbox = create_hbox (0);
    GtkWidget * butc = create_button ("Cancel", IMG_STOCK, CANCEL, -1, -1, GTK_RELIEF_NORMAL, G_CALLBACK(cancel_but), data);
    GtkWidget * butv = create_button ("Apply", IMG_STOCK, EXECUTE, -1, -1, GTK_RELIEF_NORMAL, G_CALLBACK(validate_changes), data);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, butc, TRUE, TRUE, 0);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, butv, TRUE, TRUE, 0);
    gtk_widget_set_size_request (hbox, -1, 40);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, TRUE, TRUE, 0);
    add_container_child (CONTAINER_WIN, win, vbox);
    add_gtk_close_event (win, G_CALLBACK(cancel_win), data);
  }
  show_the_widgets (win);
}
