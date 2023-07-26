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
* This file: 'm_back.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  GtkWidget * menu_back (glwin * view);

  GMenu * menu_back (glwin * view, int popm);

*/

#include "global.h"
#include "color_box.h"
#include "glwindow.h"

#ifdef GTK3
/*
*  GtkWidget * menu_back (glwin * view)
*
*  Usage:
*
*  glwin * view :
*/
GtkWidget * menu_back (glwin * view)
{
  GtkWidget * menub = gtk_menu_new ();
  GtkWidget * bc = create_menu_item (FALSE, "Color");
  gtk_menu_shell_append ((GtkMenuShell *)menub, bc);
  gtk_menu_item_set_submenu ((GtkMenuItem *)bc, color_box(view, -2, 0, 0));
  return menub;
}
#else
/*
*  GMenu * menu_back (glwin * view, int popm)
*
*  Usage:
*
*  glwin * view :
*  int popm     :
*/
GMenu * menu_back (glwin * view, int popm)
{
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, "back-color", "back-color", popm, popm, NULL, IMG_NONE, NULL, TRUE, NULL, NULL, FALSE, FALSE, FALSE, FALSE);
  append_opengl_item (view, menu, "More colors ...", "back-color", popm, popm, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(to_run_back_color_window), view, FALSE, FALSE, FALSE, TRUE);
  return menu;
}
#endif
