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
* This file: 'atom_edit.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  gboolean is_atom_win_active (glwin * view);

  G_MODULE_EXPORT gboolean delete_action (GtkWindow * widg, gpointer data);
  G_MODULE_EXPORT gboolean delete_action (GtkWidget * widg, GdkEvent * event, gpointer data);

  void clean_coord_window (struct project * this_proj);
  void clean_other_window_after_edit (struct project * this_proj);
  void clean_atom_win (struct project * this_proj);
  void prepare_atom_edition (gpointer data, gboolean visible);

  G_MODULE_EXPORT void close_edit (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void set_reset_transformation (GtkCheckButton * but, gpointer data);
  G_MODULE_EXPORT void set_reset_transformation (GtkToggleButton * but, gpointer data);
  G_MODULE_EXPORT void apply_edit (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void action_window (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void action_window (GtkWidget * widg, gpointer data);

  GtkWidget * create_atom_notebook (struct project * this_proj, GtkWidget * vbox);
  GtkWidget * create_edition_window (struct project * this_proj);

  atom_search * allocate_atom_search (int proj, int action, int searchid, int tsize);

*/

#include "atom_edit.h"

float limit[2] = {100.0, 180.0};
gchar * action_name[5] = {"Move", "Replace", "Remove", "Insert", "Random move"};
gchar * action_atoms[3] = {"All non-selected atoms", "All selected atoms", "All atoms"};
gboolean was_moved;

/*
*  gboolean is_atom_win_active (glwin * view)
*
*  Usage:
*
*  glwin * view : the target glwin
*/
gboolean is_atom_win_active (glwin * view)
{
  if (view -> atom_win)
  {
    return view -> atom_win -> visible;
  }
  return FALSE;
}

/*
*  void clean_coord_window (struct project * this_proj)
*
*  Usage:
*
*  struct project * this_proj : the target project
*/
void clean_coord_window (struct project * this_proj)
{
  if (this_proj -> modelgl -> coord_win)
  {
#ifdef GTK3
    int x, y;
    gtk_window_get_position (GTK_WINDOW(this_proj -> modelgl -> coord_win -> win), & x, & y);
#endif
    this_proj -> modelgl -> coord_win -> win = destroy_this_widget (this_proj -> modelgl -> coord_win -> win);
    this_proj -> modelgl -> coord_win -> win = advanced_coord_properties (this_proj -> modelgl, 0);
#ifdef GTK3
    gtk_window_move (GTK_WINDOW(this_proj -> modelgl -> coord_win -> win), x ,y);
#endif
  }
}

/*
*  void clean_other_window_after_edit (struct project * this_proj)
*
*  Usage:
*
*  struct project * this_proj : the target project
*/
void clean_other_window_after_edit (struct project * this_proj)
{
  int i;
  clean_coord_window (this_proj);
  if (this_proj -> modelgl -> cell_win)
  {
    if (this_proj -> modelgl -> cell_win -> win && ! this_proj -> modelgl -> cell_win -> slab_passivate)
    {
      i = gtk_notebook_get_current_page (GTK_NOTEBOOK (this_proj -> modelgl -> cell_win -> notebook));
#ifdef GTK3
      int x, y;
      gtk_window_get_position (GTK_WINDOW(this_proj -> modelgl -> cell_win -> win), & x, & y);
#endif
      this_proj -> modelgl -> cell_win -> win = destroy_this_widget (this_proj -> modelgl -> cell_win -> win);
      this_proj -> modelgl -> cell_win -> density = NULL;
      if (this_proj -> modelgl -> cell_win -> slab_lot) g_free (this_proj -> modelgl -> cell_win -> slab_lot);
      this_proj -> modelgl -> cell_win -> slab_lot = allocint (this_proj -> nspec);
      this_proj -> modelgl -> cell_win -> slab_info = NULL;
      if (this_proj -> natomes)
      {
        this_proj -> modelgl -> cell_win -> win = create_cell_edition_window (this_proj, & this_proj -> modelgl -> colorp[0][0]);
        gtk_notebook_set_current_page (GTK_NOTEBOOK (this_proj -> modelgl -> cell_win -> notebook), i);
#ifdef GTK3
        gtk_window_move (GTK_WINDOW(this_proj -> modelgl -> cell_win -> win), x ,y);
#endif
        show_the_widgets (this_proj -> modelgl -> cell_win -> win);
        for (i=1; i<6; i++)
        {
          if (i < 3) gtk_widget_hide (this_proj -> modelgl -> cell_win -> slab_hbox[i]);
          gtk_widget_hide (this_proj -> modelgl -> cell_win -> slab_box[i]);
        }
      }
    }
  }
  /*if (this_proj -> modelgl -> spiner)
  {
    gtk_window_get_position (GTK_WINDOW(this_proj -> modelgl -> spiner -> win), & x, & y);
    this_proj -> modelgl -> spiner -> win = destroy_this_widget (this_proj -> modelgl -> spiner -> win);
    g_free (this_proj -> modelgl -> spiner);
    this_proj -> modelgl -> spiner = NULL;
    if (this_proj -> natomes) window_spinner (NULL, this_proj -> modelgl);
    gtk_window_move (GTK_WINDOW(this_proj -> modelgl -> spiner -> win), x ,y);
  }*/
}

/*
*  void clean_atom_win (struct project * this_proj)
*
*  Usage:
*
*  struct project * this_proj : the target project
*/
void clean_atom_win (struct project * this_proj)
{
  this_proj -> modelgl -> anim -> last -> img -> box_axis[AXIS] = this_proj -> modelgl -> atom_win -> old_axis;
  this_proj -> modelgl -> atom_win -> win = destroy_this_widget (this_proj -> modelgl -> atom_win -> win);
  if (! this_proj -> modelgl -> builder_win)
  {
    g_free (this_proj -> modelgl -> atom_win);
    this_proj -> modelgl -> atom_win = NULL;
  }
  else
  {
    this_proj -> modelgl -> atom_win -> visible = FALSE;
  }
  int shaders[6] = {ATOMS, BONDS, POLYS, RINGS, SELEC, VOLMS};
  re_create_md_shaders (6, shaders, this_proj);
  this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
  this_proj -> modelgl -> create_shaders[MAXIS] = TRUE;
  this_proj -> modelgl -> create_shaders[PICKS] = TRUE;
  int i, j;
  for (i=0; i<2; i++)
  {
    if (this_proj -> modelgl -> saved_coord[i] != NULL)
    {
      g_free (this_proj -> modelgl -> saved_coord[i]);
      this_proj -> modelgl -> saved_coord[i] = NULL;
    }
    for (j=0; j<2; j++) this_proj -> modelgl -> rebuild[i][j] = FALSE;
  }
  clean_other_window_after_edit (this_proj);
  update (this_proj -> modelgl);
}

/*
*  G_MODULE_EXPORT void close_edit (GtkButton * but, gpointer data)
*
*  Usage:
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void close_edit (GtkButton * but, gpointer data)
{
  int id = GPOINTER_TO_INT(data);
  struct project * this_proj = get_project_by_id(id);
  gboolean leave = (this_proj -> was_moved) ? ask_yes_no("Leaving without saving ?",
                                            "To preserve atom(s) displacement(s) press the 'Apply' button\n"
                                            "Otherwise initial atom positions will be restored ...\n"
                                            "\t\t\t Are you sure to leave ?" ,
                                            GTK_MESSAGE_QUESTION, this_proj -> modelgl -> atom_win -> win) : TRUE;
  if (leave && this_proj -> modelgl -> atom_win)
  {
    int h;
    for (h=0; h<2; h++)
    {
      reset_coordinates (this_proj, h);
      init_coordinates (this_proj, h, TRUE, FALSE);
    }
    for (h=2; h<7; h++)
    {
      g_free (this_proj -> modelgl -> search_widg[h]);
      this_proj -> modelgl -> search_widg[h]= NULL;
    }
    this_proj -> was_moved = FALSE;
    clean_atom_win (this_proj);
  }
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT gboolean delete_action (GtkWindow * widg, gpointer data)
*
*  Usage:
*
*  GtkWindow * widg :
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean delete_action (GtkWindow * widg, gpointer data)
#else
/*
*  G_MODULE_EXPORT gboolean delete_action (GtkWidget * widg, GdkEvent * event, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  GdkEvent * event : the GdkEvent triggering the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean delete_action (GtkWidget * widg, GdkEvent * event, gpointer data)
#endif
{
  close_edit (NULL, data);
  return TRUE;
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void set_reset_transformation (GtkCheckButton * but, gpointer data)
*
*  Usage:
*
*  GtkCheckButton * but : the GtkCheckButton sending the signal
*  gpointer data        : the associated data pointer
*/
G_MODULE_EXPORT void set_reset_transformation (GtkCheckButton * but, gpointer data)
{
  if (gtk_check_button_get_active (but))
#else
/*
*  G_MODULE_EXPORT void set_reset_transformation (GtkToggleButton * but, gpointer data)
*
*  Usage:
*
*  GtkToggleButton * but : the GtkToggleButton sending the signal
*  gpointer data         : the associated data pointer
*/
G_MODULE_EXPORT void set_reset_transformation (GtkToggleButton * but, gpointer data)
{
  if (gtk_toggle_button_get_active (but))
#endif
  {
    tint * id = (tint *)data;
    struct project * this_proj = get_project_by_id (id -> a);
    if (ask_yes_no("Reset", "Reset and get back to initial coordinates ?\n This will affect all atoms !",
                    GTK_MESSAGE_WARNING, this_proj -> modelgl -> atom_win -> win))
    {
      int h, i, j;
      for (h=0; h<3; h++)
      {
        for (i=0; i<2; i++)
        {
          for (j=0; j<6; j++)
          {
            this_proj -> modelgl -> atom_win -> new_param[h][i][j] = this_proj -> modelgl -> edition_param[h][i][j];
            gtk_range_set_value (GTK_RANGE(this_proj -> modelgl -> atom_win -> edit_scale[j]), this_proj -> modelgl -> atom_win -> new_param[h][i][j]);
            update_range_and_entry (this_proj, h, i, j);
            // update_coordinates (this_proj, h, i, j);
          }
        }
      }
      for (h=0; h<2; h++)
      {
        reset_coordinates (this_proj, h);
        init_coordinates (this_proj, h, TRUE, FALSE);
      }
      init_default_shaders (this_proj -> modelgl);
      update (this_proj -> modelgl);
    }
#ifdef GTK4
    gtk_check_button_set_active (but, FALSE);
#else
    gtk_toggle_button_set_active (but, FALSE);
#endif
  }
}

/*
*  G_MODULE_EXPORT void apply_edit (GtkButton * but, gpointer data)
*
*  Usage:
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void apply_edit (GtkButton * but, gpointer data)
{
  int id = GPOINTER_TO_INT(data);
  struct project * this_proj = get_project_by_id(id);
  int h, i, j;
  i = this_proj -> modelgl -> mode;
  this_proj -> modelgl -> mode = ANALYZE;
  image * last = this_proj -> modelgl -> anim -> last -> img;
  vec4_t q = last -> rotation_quaternion;
  init_camera (this_proj, TRUE);
  last -> rotation_quaternion = q;
  this_proj -> modelgl -> mode = i;
  for (h=0; h<3; h++)
  {
    for (i=0; i<2; i++)
    {
      for (j=0; j<6; j++)
      {
        this_proj -> modelgl -> edition_param[h][i][j] = this_proj -> modelgl -> atom_win -> new_param[h][i][j];
        this_proj -> modelgl -> atom_win -> old_param[h][i][j] = this_proj -> modelgl -> atom_win -> new_param[h][i][j];
      }
    }
  }
  for (h=0; h<2; h++)
  {
    g_free (this_proj -> modelgl -> saved_coord[h]);
    this_proj -> modelgl -> saved_coord[h] = save_coordinates (this_proj, h);
  }
  this_proj -> was_moved = FALSE;
  //clean_atom_win (this_proj);
}

/*
*  GtkWidget * create_atom_notebook (struct project * this_proj, GtkWidget * vbox)
*
*  Usage:
*
*  struct project * this_proj : the target project
*  GtkWidget * vbox           : the GtkWidget sending the signal
*/
GtkWidget * create_atom_notebook (struct project * this_proj, GtkWidget * vbox)
{
  GtkWidget * notebook = gtk_notebook_new ();
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, notebook, FALSE, FALSE, 0);
  gchar * str;
  int i;
  for (i=0; i<5; i++)
  {
    str = g_strdup_printf ("<b>%s</b>", action_name[i]);
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), action_tab(i, this_proj), markup_label(str, -1, -1, 0.0, 0.5));
    g_free (str);
  }

  show_the_widgets (notebook);
  return notebook;
}

/*
*  atom_search * allocate_atom_search (int proj, int action, int searchid, int tsize)
*
*  Usage:
*
*  int proj     :
*  int action   :
*  int searchid :
*  int tsize    :
*/
atom_search * allocate_atom_search (int proj, int action, int searchid, int tsize)
{
  atom_search * asearch = g_malloc0 (sizeof*asearch);
  asearch -> search_digit = -1;
  asearch -> proj = proj;
  asearch -> action = action;
  int i;
  for (i=0; i<6; i++)
  {
    asearch -> pointer[i].a = proj;
    asearch -> pointer[i].b = TOLAB+i;
    asearch -> pointer[i].c = searchid;
  }
  asearch -> update_bonding = (get_project_by_id(proj) -> natomes > 10000) ? FALSE : TRUE;
  if (searchid != 5) allocate_todo (asearch, tsize);
  clean_picked_and_labelled (asearch);
  return asearch;
}

/*
*  GtkWidget * create_edition_window (struct project * this_proj)
*
*  Usage:
*
*  struct project * this_proj : the target project
*/
GtkWidget * create_edition_window (struct project * this_proj)
{
  gchar * str = g_strdup_printf ("Model edition - %s", this_proj -> name);
  this_proj -> was_moved = FALSE;
  GtkWidget * win = create_win (str, this_proj -> modelgl -> win, FALSE, FALSE);
  g_free (str);
  int i, j;
  for (i=2; i<7; i++)
  {
    if (this_proj -> modelgl -> search_widg[i] == NULL)
    {
      this_proj -> modelgl -> search_widg[i] = allocate_atom_search (this_proj -> id, i, i, this_proj -> natomes);
      this_proj -> modelgl -> search_widg[i] -> status = (this_proj -> modelgl -> anim -> last -> img -> selected[0] -> selected) ? 1 : 2;
    }
  }
  this_proj -> modelgl -> atom_win -> vbox = create_vbox (5);
  add_container_child (CONTAINER_WIN, win, this_proj -> modelgl -> atom_win -> vbox);
  gtk_widget_set_size_request (this_proj -> modelgl -> atom_win -> vbox, 760, -1);
  add_gtk_close_event (win, G_CALLBACK(delete_action), GINT_TO_POINTER(this_proj -> id));
  for (i=0; i<2; i++)
  {
    for (j=0; j<2; j++) this_proj -> modelgl -> rebuild[i][j] = TRUE;
  }
  this_proj -> modelgl -> atom_win -> notebook = create_atom_notebook (this_proj, this_proj -> modelgl -> atom_win -> vbox);
  GtkWidget * hbox = create_hbox (5);
  add_box_child_end (this_proj -> modelgl -> atom_win -> vbox, hbox, TRUE, FALSE, 0);
  GtkWidget * but = create_button ("Apply", IMG_STOCK, APPLY, -1, -1, GTK_RELIEF_NORMAL, G_CALLBACK(apply_edit), GINT_TO_POINTER(this_proj -> id));
  add_box_child_end (hbox, but, FALSE, FALSE, 5);
  but = create_button ("Close", IMG_STOCK, FCLOSE, -1, -1, GTK_RELIEF_NORMAL, G_CALLBACK(close_edit), GINT_TO_POINTER(this_proj -> id));
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, but, FALSE, FALSE, 5);
  return win;
}

/*
*  void prepare_atom_edition (gpointer data, gboolean visible)
*
*  Usage:
*
*  gpointer data    : the associated data pointer
*  gboolean visible :
*/
void prepare_atom_edition (gpointer data, gboolean visible)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  int i;
  if (this_proj -> modelgl -> atom_win == NULL)
  {
    this_proj -> modelgl -> atom_win = g_malloc0 (sizeof*this_proj -> modelgl -> atom_win);
    for (i=0; i<2; i++) this_proj -> modelgl -> atom_win -> adv_bonding[i] = this_proj -> modelgl -> adv_bonding[i];
    if (this_proj -> modelgl -> anim)
    {
      this_proj -> modelgl -> atom_win -> old_axis = this_proj -> modelgl -> anim -> last -> img -> box_axis[AXIS];
    }
    for (i=0; i<2; i++)
    {
      init_coordinates (this_proj, i, visible, TRUE);
    }
    this_proj -> modelgl -> atom_win -> msd = allocfloat (this_proj -> natomes);
    this_proj -> modelgl -> atom_win -> msd_all = allocfloat (this_proj -> nspec);
    this_proj -> modelgl -> atom_win -> repeat_move = 1;
  }
  this_proj -> modelgl -> atom_win -> visible = visible;
  if (visible)
  {
    this_proj -> modelgl -> atom_win -> win = create_edition_window (this_proj);
    show_the_widgets (this_proj -> modelgl -> atom_win -> win);
    gtk_notebook_set_current_page (GTK_NOTEBOOK (this_proj -> modelgl -> atom_win -> notebook), id -> b);
    for (i=0; i<5; i++)
    {
      if (i != 3)
      {
        if (this_proj -> modelgl -> search_widg[i+2] -> todo_size < 10000)
        {
          gtk_widget_hide (this_proj -> modelgl -> search_widg[i+2] -> info[1]);
        }
        else
        {
          gtk_widget_hide (this_proj -> modelgl -> search_widg[i+2] -> id_box);
        }
      }
      widget_set_sensitive (gtk_notebook_get_nth_page(GTK_NOTEBOOK (this_proj -> modelgl -> atom_win -> notebook), i), this_proj -> natomes);
    }
    widget_set_sensitive (gtk_notebook_get_nth_page(GTK_NOTEBOOK (this_proj -> modelgl -> atom_win -> notebook), 3), 1);
    if (this_proj -> modelgl -> mode == EDITION)
    {
#ifdef GTK4
      set_mode (NULL, & this_proj -> modelgl -> colorp[0][0]);
#else
      // GTK3 Menu Action To Check
      gtk_check_menu_item_set_active ((GtkCheckMenuItem *)this_proj -> modelgl -> ogl_mode[0], TRUE);
      set_mode (this_proj -> modelgl -> ogl_mode[0], & this_proj -> modelgl -> colorp[0][0]);
#endif
    }
  }
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void action_window (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage:
*
*  GSimpleAction * action :
*  GVariant * parameter   :
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT void action_window (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*
*  G_MODULE_EXPORT void action_window (GtkWidget * widg, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void action_window (GtkWidget * widg, gpointer data)
#endif
{
  prepare_atom_edition (data, TRUE);
}
