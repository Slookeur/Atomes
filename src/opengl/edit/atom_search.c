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
* This file: 'atom_search.c'
*
*  Contains: 
*
*
*
*
*  List of subroutines: 

  void set_status_for_all (struct project * this_proj, int * data, int status, int start, int stop);

  G_MODULE_EXPORT void turn_rebuild_on (GtkCheckButton * but, gpointer data);
  G_MODULE_EXPORT void turn_rebuild_on (GtkToggleButton * but, gpointer data);
  G_MODULE_EXPORT void turn_bonding_on (GtkCheckButton * but, gpointer data);
  G_MODULE_EXPORT void turn_bonding_on (GtkToggleButton * but, gpointer data);
  G_MODULE_EXPORT void set_atoms_for_action (GtkComboBox * box, gpointer data);
  G_MODULE_EXPORT void expanding_atoms (GtkWidget * exp, gpointer data);

  GtkWidget * create_search_box (int aid, struct project * this_proj);
  GtkWidget * create_action_combo (int id, struct project * this_proj);
  GtkWidget * action_tab (int aid, struct project * this_proj);

*/

#include "atom_edit.h"

/*
*  void set_status_for_all (struct project * this_proj, int * data, int status, int start, int stop)
*
*  Usage: 
*
*  struct project * this_proj : 
*  int * data                 : 
*  int status                 : 
*  int start                  : 
*  int stop                   : 
*/
void set_status_for_all (struct project * this_proj, int * data, int status, int start, int stop)
{
  int i, j;
  for (i=start; i<stop; i++)
  {
    for (j=0; j<this_proj -> natomes; j++)
    {
      if (this_proj -> atoms[0][j].pick[0] == i)
      {
        data[j] = ! status;
      }
    }
  }
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void turn_rebuild_on (GtkCheckButton * but, gpointer data)
*
*  Usage: 
*
*  GtkCheckButton * but : 
*  gpointer data        : 
*/
G_MODULE_EXPORT void turn_rebuild_on (GtkCheckButton * but, gpointer data)
#else
/*
*  G_MODULE_EXPORT void turn_rebuild_on (GtkToggleButton * but, gpointer data)
*
*  Usage: 
*
*  GtkToggleButton * but : 
*  gpointer data         : 
*/
G_MODULE_EXPORT void turn_rebuild_on (GtkToggleButton * but, gpointer data)
#endif
{
  tint * dat = (tint *) data;
  int i;
#ifdef GTK4
  i = gtk_check_button_get_active (but);
#else
  i = gtk_toggle_button_get_active (but);
#endif
  get_project_by_id(dat -> a) -> modelgl -> rebuild[0][dat -> c] = i;
  check_menu_item_set_active ((gpointer)get_project_by_id(dat -> a) -> modelgl -> rbuild[i], i);
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void turn_bonding_on (GtkCheckButton * but, gpointer data)
*
*  Usage: 
*
*  GtkCheckButton * but : 
*  gpointer data        : 
*/
G_MODULE_EXPORT void turn_bonding_on (GtkCheckButton * but, gpointer data)
#else
/*
*  G_MODULE_EXPORT void turn_bonding_on (GtkToggleButton * but, gpointer data)
*
*  Usage: 
*
*  GtkToggleButton * but : 
*  gpointer data         : 
*/
G_MODULE_EXPORT void turn_bonding_on (GtkToggleButton * but, gpointer data)
#endif
{
  atom_search * asearch = (atom_search *) data;
  int i;
#ifdef GTK4
  i = gtk_check_button_get_active (but);
#else
  i = gtk_toggle_button_get_active (but);
#endif
  asearch -> update_bonding = i;
}

/*
*  GtkWidget * create_search_box (int aid, struct project * this_proj)
*
*  Usage: 
*
*  int aid                    : 
*  struct project * this_proj : 
*/
GtkWidget * create_search_box (int aid, struct project * this_proj)
{
  gchar * appl[5] = {" Move atom(s)", " Replace atom(s)", " Remove atom(s)", " Insert atom(s)", " Move atom(s)"};
  gchar * img[4]= {EDITA, LIST_REM, LIST_ADD, MEDIA_PLAY};
  GtkWidget * vbox = create_vbox (BSEP);
  GtkWidget * hbox;
  GtkWidget * widg;
  int i;
  if (aid == 4)
  {
    i = (this_proj -> modelgl -> search_widg[aid+1] != NULL) ? this_proj -> modelgl -> search_widg[aid+1] -> in_selection : 0;
  }
  else if (aid == 8)
  {
    i = this_proj -> nspec;
  }
  else
  {
    i = this_proj -> modelgl -> search_widg[aid+1] -> todo_size;
  }
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, selection_tab (this_proj -> modelgl -> search_widg[aid+1], i), FALSE, FALSE, 0);
  if (aid == 1 || aid == 5)
  {
    int j = (aid == 1) ? 0 : 1;
    hbox = create_hbox (0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 0);
    this_proj -> modelgl -> atom_win -> edition_but[j] = check_button("Extract/rebuild the object(s) to be moved, ie. cut/clean bonds with nearest neighbor(s)",
                                                                       -1, 25, this_proj -> modelgl -> rebuild[0][j], G_CALLBACK(turn_rebuild_on), & this_proj -> modelgl -> colorp[0][j]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, this_proj -> modelgl -> atom_win -> edition_but[j], FALSE, FALSE, 50);
  }
  if (aid == 1 || aid == 3 || aid == 5)
  {
    hbox = create_hbox (0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 0);
    widg = check_button("Update bonding information, but do not recompute bonding using bond cutoff(s)",
                        -1, 25, this_proj -> modelgl -> search_widg[aid+1] -> update_bonding, G_CALLBACK(turn_bonding_on),  this_proj -> modelgl -> search_widg[aid+1]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, widg, FALSE, FALSE, 50);
  }

  if (aid > 1 && aid < 6)
  {
    GtkWidget * lay = create_layout (-1, 100);
    GtkWidget * but;
    if (aid == 5)
    {
      layout_add_widget (lay, markup_label("Repeat <i>n</i> times, <i>n</i>= ", 100, -1, 0.0, 0.5), 125, 25);
      layout_add_widget (lay, spin_button (G_CALLBACK(repeat_move), 1, 1, 1000, 1, 0, 100, this_proj), 275, 20);
    }
    but = create_button (appl[aid-1], IMG_STOCK, img[aid-2], 100, 35, GTK_RELIEF_NORMAL, G_CALLBACK(take_action), & this_proj -> modelgl -> search_widg[aid+1] -> pointer[0]);
    layout_add_widget (lay, but, 500, 20);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, lay, FALSE, FALSE, 0);
  }
  return vbox;
}

/*
*  G_MODULE_EXPORT void set_atoms_for_action (GtkComboBox * box, gpointer data)
*
*  Usage: 
*
*  GtkComboBox * box : 
*  gpointer data     : 
*/
G_MODULE_EXPORT void set_atoms_for_action (GtkComboBox * box, gpointer data)
{
  tint * id = (tint *)data;
  struct project * this_proj = get_project_by_id (id -> a);
  int i, j, k;
  i = gtk_combo_box_get_active (box);
  this_proj -> modelgl -> search_widg[id -> c] -> status = i;
  if (id -> c == 2)
  {
    for (j=0; j<2; j++)
    {
      gtk_combo_box_set_active (GTK_COMBO_BOX(this_proj -> modelgl -> atom_win -> axis_combo[j]), this_proj -> modelgl -> atom_win -> axis[j]);
      for (k=0; k<6; k++)
      {
        gtk_range_set_value (GTK_RANGE(this_proj -> modelgl -> atom_win -> edit_scale[k]), this_proj -> modelgl -> atom_win -> new_param[i][j][k]);
        update_range_and_entry (this_proj, i, this_proj -> modelgl -> atom_win -> axis[j], k);
      }
    }
  }
  clean_picked_and_labelled (this_proj -> modelgl -> search_widg[id -> c]);
  update_search_tree (this_proj -> modelgl -> search_widg[id -> c]);
}

/*
*  GtkWidget * create_action_combo (int id, struct project * this_proj)
*
*  Usage: 
*
*  int id                     : 
*  struct project * this_proj : 
*/
GtkWidget * create_action_combo (int id, struct project * this_proj)
{
  GtkWidget * combo;
  if (id == 3 || id == 5)
  {
    GtkTreeModel * model = replace_combo_tree (TRUE, this_proj -> id);
    combo = gtk_combo_box_new_with_model (model);
    g_object_unref (model);
    GtkCellRenderer * renderer = gtk_cell_renderer_combo_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), renderer, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo), renderer, "text", 0, NULL);
    g_signal_connect (G_OBJECT(combo), "changed", G_CALLBACK(set_atoms_to_insert), & this_proj -> modelgl -> search_widg[id+2] -> pointer[0]);
    gtk_combo_box_set_active (GTK_COMBO_BOX(combo), 0);
    GList * cell_list = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(combo));
    if(cell_list && cell_list -> data)
    {
      gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combo), cell_list -> data, "markup", 0, NULL);
    }
    set_atoms_to_insert (GTK_COMBO_BOX(combo), & this_proj -> modelgl -> search_widg[id+2] -> pointer[0]);
  }
  else
  {
    combo = create_combo ();
    int i;
    for (i=0; i<3; i++) combo_text_append (combo, action_atoms[i]);
    gtk_combo_box_set_active (GTK_COMBO_BOX(combo), this_proj -> modelgl -> search_widg[id+2] -> status);
    g_signal_connect (G_OBJECT (combo), "changed", G_CALLBACK(set_atoms_for_action), & this_proj -> modelgl -> search_widg[id+2] -> pointer[0]);
  }

  return combo;
}

/*
*  G_MODULE_EXPORT void expanding_atoms (GtkWidget * exp, gpointer data)
*
*  Usage: 
*
*  GtkWidget * exp : 
*  gpointer data   : 
*/
G_MODULE_EXPORT void expanding_atoms (GtkWidget * exp, gpointer data)
{
  tint * dat = (tint *)data;
  struct project * this_proj = get_project_by_id (dat -> a);
  int i;
  for (i=0; i<3; i++)
  {
    if (i != dat -> c - 1)
    {
      if (gtk_expander_get_expanded (GTK_EXPANDER (this_proj -> modelgl -> atom_win -> at_expand[i])))
      {
        gtk_expander_set_expanded (GTK_EXPANDER (this_proj -> modelgl -> atom_win -> at_expand[i]), FALSE);
      }
    }
  }
}

/*
*  GtkWidget * action_tab (int aid, struct project * this_proj)
*
*  Usage: 
*
*  int aid                    : 
*  struct project * this_proj : 
*/
GtkWidget * action_tab (int aid, struct project * this_proj)
{
  gchar * action[7] = {"moved", "replaced", "removed", "inserted", "moved randomly", " ", "passivated"};
  GtkWidget * vbox = create_vbox (BSEP);
  atom_search * asearch = this_proj -> modelgl -> search_widg[aid+2];
  GtkWidget * hbox;
  if (aid < 5)
  {
    hbox = create_hbox (5);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 10);
    gchar * str = g_strdup_printf ("<u>Select the object(s) to be %s in:</u> ", action[aid]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label(str, 200, -1, 0.0, 0.5), FALSE, FALSE, 20);
    g_free (str);
    this_proj -> modelgl -> atom_win -> atom_combo[aid] = create_action_combo (aid, this_proj);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, this_proj -> modelgl -> atom_win -> atom_combo[aid], FALSE, FALSE, 20);
  }
  GtkWidget * sbox = create_vbox (BSEP);
  GtkWidget * tbox = create_search_box (aid+1, this_proj);
  if (! aid)
  {
    gchar * exp_name[3] = {"<b>Atom selection:</b>", "<b>Translate:</b>", "<b>Rotate:</b>"};
    int i;
    for (i=0; i<3; i++)
    {
      this_proj -> modelgl -> atom_win -> at_expand[i] = create_expander (exp_name[i], NULL, i);
      add_box_child_start (GTK_ORIENTATION_VERTICAL, sbox, this_proj -> modelgl -> atom_win -> at_expand[i], TRUE, TRUE, 10);
      if (! i)
      {
        add_container_child (CONTAINER_EXP, this_proj -> modelgl -> atom_win -> at_expand[i], tbox);
        gtk_expander_set_expanded (GTK_EXPANDER(this_proj -> modelgl -> atom_win -> at_expand[i]), TRUE);
      }
      else
      {
        add_container_child (CONTAINER_EXP, this_proj -> modelgl -> atom_win -> at_expand[i], add_motion_interaction (asearch, i-1, this_proj));
        gtk_expander_set_expanded (GTK_EXPANDER(this_proj -> modelgl -> atom_win -> at_expand[i]), FALSE);
        if (i == 2) widget_set_sensitive (this_proj -> modelgl -> atom_win -> at_expand[2], asearch -> object);
      }
      g_signal_connect (G_OBJECT(this_proj -> modelgl -> atom_win -> at_expand[i]), "activate", G_CALLBACK(expanding_atoms), & asearch -> pointer[0]);
    }
  }
  else
  {
    if (aid < 3)
    {
      asearch -> mode_box = create_combo ();
      combo_text_append (asearch -> mode_box, "Normally");
      combo_text_append (asearch -> mode_box, "Randomly");
      gtk_combo_box_set_active (GTK_COMBO_BOX(asearch -> mode_box), 0);
      g_signal_connect (G_OBJECT (asearch -> mode_box), "changed", G_CALLBACK(set_search_mode), asearch);
      GtkWidget * box = create_hbox (0);
      add_box_child_start (GTK_ORIENTATION_VERTICAL, sbox, box, FALSE, FALSE, 5);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, markup_label("<b>.</b>", 5, -1, 0.0, 0.5), FALSE, FALSE, 10);
      gchar * type_of[2]={"Replace: ", "Remove: "};
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, markup_label(type_of[aid-1], 50, -1, 0.0, 0.5), FALSE, FALSE, 0);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, asearch -> mode_box, FALSE, FALSE, 5);
    }
    add_box_child_start (GTK_ORIENTATION_VERTICAL, sbox, tbox, FALSE, FALSE, 0);
  }
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, sbox, FALSE, FALSE, 5);
  if (aid == 4)
  {
    hbox = create_hbox (5);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 20);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox,
                         check_button ("Reset transformation(s)", -1, 35, FALSE, G_CALLBACK(set_reset_transformation), & asearch -> pointer[0]),
                         FALSE, FALSE, 10);
  }
  show_the_widgets (vbox);
  if (aid != 3) widget_set_sensitive (vbox, this_proj -> nspec);
  return vbox;
}
