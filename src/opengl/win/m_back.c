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
#include "color_box.h"
#include "glwindow.h"

#ifdef GTK3
GtkWidget * menu_back (glwin * view)
{
  GtkWidget * menub = gtk_menu_new ();
  GtkWidget * bc = create_menu_item (FALSE, "Color");
  add_menu_child (menub, bc);
  menu_item_set_submenu (bc, color_box(view, -2, 0, 0));
  return menub;
}
#else
GMenu * menu_back (glwin * view)
{
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, "back-color", "back-color", 0, NULL, IMG_NONE, NULL, TRUE, NULL, NULL, FALSE, FALSE, FALSE, FALSE);
  append_opengl_item (view, menu, "More colors ...", "back-color", 0, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(to_run_back_color_window), view, FALSE, FALSE, FALSE, TRUE);
  return menu;
}
#endif
