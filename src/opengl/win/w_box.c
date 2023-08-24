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
* This file: 'w_box.c'
*
*  Contains:
*

 - The subroutines to create the box properties window

*
*  List of subroutines:

  G_MODULE_EXPORT void set_box_combo_style (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void set_show_box_toggle (GtkCheckButton * but, gpointer data);
  G_MODULE_EXPORT void set_show_box_toggle (GtkToggleButton * but, gpointer data);
  G_MODULE_EXPORT void set_color_box (GtkColorChooser * colob, gpointer data);
  G_MODULE_EXPORT void box_advanced (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void box_advanced (GtkWidget * widg, gpointer data);

*/

#include "global.h"
#include "interface.h"
#include "glview.h"
#include "glwindow.h"

#define BOX_STYLES 2

extern G_MODULE_EXPORT void update_bond_parameter (GtkEntry * res, gpointer data);
extern GtkWidget * styles;
extern GtkWidget * width_box;
extern GtkWidget * radius_box;

gchar * box_style[BOX_STYLES] = {"Wireframe", "Cylinders"};
GtkWidget * box_data;

/*
*  G_MODULE_EXPORT void set_box_combo_style (GtkWidget * widg, gpointer data)
*
*  Usage: set box style callback
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void set_box_combo_style (GtkWidget * widg, gpointer data)
{
  int i = gtk_combo_box_get_active (GTK_COMBO_BOX(widg));
#ifdef GTK4
  opengl_project -> modelgl -> anim -> last -> img -> box_axis[0] = (i < 0) ? (NONE) : (i == 0) ? WIREFRAME : CYLINDERS;
  opengl_project -> modelgl -> create_shaders[MDBOX] = TRUE;
  update (opengl_project -> modelgl);
#endif
  if (i == 1)
  {
    if (is_the_widget_visible(width_box)) gtk_widget_hide (width_box);
    if (! is_the_widget_visible(radius_box)) gtk_widget_show (radius_box);
#ifdef GTK4

#else
    // GTK3 Menu Action To Check
    gtk_check_menu_item_set_active ((GtkCheckMenuItem *)opengl_project -> modelgl -> ogl_box_axis[0][2], TRUE);
#endif
  }
  else if (i == 0)
  {
    if (is_the_widget_visible(radius_box)) gtk_widget_hide (radius_box);
    if (! is_the_widget_visible(width_box)) gtk_widget_show (width_box);
#ifdef GTK3
    // GTK3 Menu Action To Check
    gtk_check_menu_item_set_active ((GtkCheckMenuItem *)opengl_project -> modelgl -> ogl_box_axis[0][1], TRUE);
#endif
  }
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void set_show_box_toggle (GtkCheckButton * but, gpointer data)
*
*  Usage: toggle show / hide box callback GTK4
*
*  GtkCheckButton * but : the GtkCheckButton sending the signal
*  gpointer data        : the associated data pointer
*/
G_MODULE_EXPORT void set_show_box_toggle (GtkCheckButton * but, gpointer data)
#else
/*
*  G_MODULE_EXPORT void set_show_box_toggle (GtkToggleButton * but, gpointer data)
*
*  Usage: toggle show / hide box callback GTK3
*
*  GtkToggleButton * but : the GtkToggleButton sending the signal
*  gpointer data         : the associated data pointer
*/
G_MODULE_EXPORT void set_show_box_toggle (GtkToggleButton * but, gpointer data)
#endif
{
  gboolean val;
#ifdef GTK4
  val = gtk_check_button_get_active (but);
#else
  val = gtk_toggle_button_get_active (but);
#endif
  if (val)
  {
#ifdef GTK3
     // GTK3 Menu Action To Check
    gtk_check_menu_item_set_active ((GtkCheckMenuItem *)opengl_project -> modelgl -> ogl_box_axis[0][0], TRUE);
#endif
    gtk_combo_box_set_active (GTK_COMBO_BOX(styles), WIREFRAME-1);
  }
  else
  {
#ifdef GTK3
    // GTK3 Menu Action To Check
    gtk_check_menu_item_set_active ((GtkCheckMenuItem *)opengl_project -> modelgl -> ogl_box_axis[0][0], FALSE);
#endif
    gtk_combo_box_set_active (GTK_COMBO_BOX(styles), NONE);
  }
  widget_set_sensitive (box_data, val);
}

/*
*  G_MODULE_EXPORT void set_color_box (GtkColorChooser * colob, gpointer data)
*
*  Usage: set box color callback
*
*  GtkColorChooser * colob : the GtkColorChooser sending the signal
*  gpointer data           : the associated data pointer
*/
G_MODULE_EXPORT void set_color_box (GtkColorChooser * colob, gpointer data)
{
  opengl_project -> modelgl -> anim -> last -> img -> box_color = get_button_color (colob);
  opengl_project -> modelgl -> create_shaders[MDBOX] = TRUE;
  update (opengl_project -> modelgl);
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void box_advanced (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage: create the box edition window callback GTK4
*
*  GSimpleAction * action : the GAction sending the signal
*  GVariant * parameter   : GVariant parameter of the GAction
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT void box_advanced (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*
*  G_MODULE_EXPORT void box_advanced (GtkWidget * widg, gpointer data)
*
*  Usage: create the box edition window callback GTK3
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void box_advanced (GtkWidget * widg, gpointer data)
#endif
{
  int i;

  glwin * view = (glwin *)data;
  opengl_project_changed (view -> proj);
  GtkWidget * win = dialogmodal ("Advanced box settings", GTK_WINDOW(view -> win));
  GtkWidget * vbox = dialog_get_content_area (win);
  GtkWidget * box;

  gboolean ac;
  if (view -> anim -> last -> img -> box_axis[BOX] != NONE)
  {
    ac = TRUE;
  }
  else
  {
    ac = FALSE;
  }
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, check_button ("Show / hide box", 100, 40, ac, G_CALLBACK(set_show_box_toggle), (gpointer)data),
                       FALSE, FALSE, 0);
  box_data = create_vbox (BSEP);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, box_data, TRUE, TRUE, 10);
  widget_set_sensitive (box_data, ac);

  GtkWidget * pos_box = create_vbox (BSEP);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box_data, pos_box, TRUE, TRUE, 0);

  box = abox (box_data, "Style: ", 0);
  styles  = create_combo ();
  for (i=0; i<BOX_STYLES; i++)
  {
    combo_text_append (styles, box_style[i]);
  }
  if (view -> anim -> last -> img -> box_axis[BOX] == NONE) i = NONE;
  if (view -> anim -> last -> img -> box_axis[BOX] == WIREFRAME) i = 0;
  if (view -> anim -> last -> img -> box_axis[BOX] == CYLINDERS) i = 1;
  gtk_combo_box_set_active (GTK_COMBO_BOX(styles), i);
  gtk_widget_set_size_request (styles, 100, -1);
  g_signal_connect (G_OBJECT (styles), "changed", G_CALLBACK(set_box_combo_style), data);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, styles, TRUE, TRUE, 0);
  width_box = abox (box_data, "Line width [pts]: ", 0);
  GtkWidget * width  = create_entry (G_CALLBACK(update_bond_parameter), 100, 10, FALSE, (gpointer)GINT_TO_POINTER(-3));
  update_entry_double (GTK_ENTRY(width), view -> anim -> last -> img -> box_axis_line[BOX]);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, width_box, width, FALSE, FALSE, 10);
  radius_box = abox (box_data, "Cylinder radius [Å]: ", 0);
  GtkWidget * radius = create_entry (G_CALLBACK(update_bond_parameter), 100, 10, FALSE, (gpointer)GINT_TO_POINTER(-3));
  update_entry_double (GTK_ENTRY(radius), view -> anim -> last -> img -> box_axis_rad[BOX]);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, radius_box, radius, FALSE, FALSE, 10);

  // Colors
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, abox (box_data, "Color:", 0),
                       color_button(view -> anim -> last -> img -> box_color, TRUE, 100, -1, G_CALLBACK(set_color_box), NULL),
                       FALSE, FALSE, 10);

  g_signal_connect (G_OBJECT(win), "response", G_CALLBACK(run_destroy_dialog), NULL);
  show_the_widgets (win);
   if (view -> anim -> last -> img -> box_axis[BOX] == CYLINDERS)
  {
    gtk_widget_hide (width_box);
  }
  else
  {
    gtk_widget_hide (radius_box);
  }
  dialog_id ++;
  Event_loop[dialog_id] = g_main_loop_new (NULL, FALSE);
  g_main_loop_run (Event_loop[dialog_id]);
#ifdef GTK4
  update_menu_bar (view);
#endif
}
