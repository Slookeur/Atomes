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
#include "interface.h"
#include "project.h"
#include "glwindow.h"
#include "glview.h"

void set_quality (int q, glwin * view)
{
  view -> anim -> last -> img -> quality = q;
  init_default_shaders (view);
  view -> create_shaders[MDBOX] = TRUE;
  view -> create_shaders[MAXIS] = TRUE;
  update (view);
#ifdef GTK3
  gtk_range_set_value (GTK_RANGE(view -> ogl_quality), view -> anim -> last -> img -> quality);
#endif
}

G_MODULE_EXPORT void set_quality_spin (GtkSpinButton * res, gpointer data)
{
  glwin * view = (glwin *)data;
  set_quality (gtk_spin_button_get_value_as_int(res), view);
  update_entry_int (GTK_ENTRY(res), view -> anim -> last -> img -> quality);
}

G_MODULE_EXPORT void window_quality (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  GtkWidget * win =  dialogmodal ("Render Quality", GTK_WINDOW(view -> win));
  GtkWidget * vbox = dialog_get_content_area (win);
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, TRUE, TRUE, 10);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, gtk_label_new ("Quality [2-1000]: "), TRUE, TRUE, 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox,
                      spin_button (G_CALLBACK(set_quality_spin), view -> anim -> last -> img -> quality, 2.0, 1000.0, 1.0, 0, 100, data),
                      FALSE, FALSE, 10);
  add_gtk_close_event (win, G_CALLBACK(destroy_this_window), NULL);
  show_the_widgets (vbox);
  run_this_gtk_dialog (win, G_CALLBACK(run_destroy_dialog), NULL);
#ifdef GTK3
  // GTK3 Menu Action To Check
  gtk_range_set_value (GTK_RANGE(view -> ogl_quality), view -> anim -> last -> img -> quality);
#else
  update_menu_bar (view);
#endif
}

#ifdef GTK3
GtkWidget * menu_quality (glwin * view, int id)
{
  GtkWidget * menuq = gtk_menu_new ();
  GtkWidget * fixed = gtk_fixed_new ();
  if (id == 0)
  {
    view -> ogl_quality = create_hscale (2.0, 1000.0, 1.0, view -> anim -> last -> img -> quality, GTK_POS_RIGHT, 0, 100, NULL, NULL, NULL);
    gtk_fixed_put (GTK_FIXED(fixed), view -> ogl_quality, 0, 0);
  }
  else
  {
    GtkWidget * qscale = create_hscale (2.0, 1000.0, 1.0, view -> anim -> last -> img -> quality, GTK_POS_RIGHT, 0, 100, NULL, NULL, NULL);
    gtk_fixed_put (GTK_FIXED(fixed), qscale, 0, 0);
  }
  GtkWidget * qs = create_menu_item_from_widget (fixed, FALSE, FALSE, FALSE);
  g_signal_connect (G_OBJECT (qs), "activate", G_CALLBACK(window_quality), view);
  add_menu_child (menuq, qs);
  return menuq;
}
#else
G_MODULE_EXPORT void to_window_quality (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  window_quality (NULL, data);
}

GMenu * menu_quality (glwin * view)
{
  GMenu * menu = g_menu_new ();
  gchar * str = g_strdup_printf ("Quality: %d", view -> anim -> last -> img -> quality);
  append_opengl_item (view, menu, str, "quality", 0, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(to_window_quality), (gpointer)view, FALSE, FALSE, FALSE, TRUE);
  g_free (str);
  return menu;
}
#endif
