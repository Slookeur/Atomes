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
#include "glview.h"
#include "glwindow.h"
#include "submenus.h"

#ifdef GTK3
G_MODULE_EXPORT void show_hide_clones (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  gboolean j;
  j = check_menu_item_get_active ((gpointer)widg);
  if (widg != view -> ogl_clones[0])
  {
    check_menu_item_set_active ((gpointer)view -> ogl_clones[0], j);
  }
  view -> anim -> last -> img -> draw_clones = j;
  widget_set_sensitive (view -> ogl_atoms[5], j);
  widget_set_sensitive (view -> ogl_atoms[7], j);
  widget_set_sensitive (view -> ogl_bonds[9], j);
  widget_set_sensitive (view -> ogl_bonds[11], j);
  widget_set_sensitive (view -> ogl_bonds[13], j);
  widget_set_sensitive (view -> ogl_clones[1], j);
  widget_set_sensitive (view -> ogl_clones[2], j);
  widget_set_sensitive (view -> ogl_clones[3], j);
  widget_set_sensitive (view -> ogl_clones[4], j);
  int i;
  for (i=0; i<get_project_by_id(view -> proj) -> nspec; i++)
  {
    widget_set_sensitive (view -> ogl_spec[1][i], j);
    widget_set_sensitive (view -> ogl_lab[1][i], j);
  }
  init_default_shaders (view);
  update (view);
}

GtkWidget * create_clone_widget (char * name, gboolean clone, GtkWidget * menu, glwin * view)
{
  GtkWidget * clone_widget = gtk3_menu_item (menu, name, IMG_NONE, NULL, G_CALLBACK(show_hide_clones), view, FALSE, 0, 0, TRUE, FALSE, clone);
  widget_set_sensitive (clone_widget, view -> allbonds[1]);
  return clone_widget;
}

GtkWidget * menu_clones (glwin * view, int id)
{
  GtkWidget * menu = gtk_menu_new ();
  if (id == 0)
  {
    view -> ogl_clones[0] = create_clone_widget ("Show/Hide", view -> anim -> last -> img -> draw_clones, menu, view);
  }
  else
  {
    create_clone_widget ("Show/Hide", view -> anim -> last -> img -> draw_clones, menu, view);
  }
  add_menu_child (menu, menu_item_new_with_submenu ("Atom(s)", TRUE, menu_atoms (view, id, 1)));
  if ((id == 0) || (view -> anim -> last -> img -> style != SPHERES && view -> anim -> last -> img -> style != PUNT))
  {
    add_menu_child (menu, menu_item_new_with_submenu ("Bond(s)", TRUE, menu_bonds (view, id, 1)));
  }
  return menu;
}
#else
G_MODULE_EXPORT void show_hide_clones (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  glwin * view = (glwin *)data;
  view -> anim -> last -> img -> draw_clones = ! view -> anim -> last -> img -> draw_clones;
  init_default_shaders (view);
  update (view);
  update_menu_bar (view);
}

GMenu * menu_clones (glwin * view)
{
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, "Show/Hide", "show-clones", 0, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(show_hide_clones), (gpointer)view, TRUE, view -> anim -> last -> img -> draw_clones, FALSE, (view -> allbonds[1]) ? TRUE : FALSE);
  g_menu_append_submenu (menu, "Atom(s)", (GMenuModel*)menu_atoms(view, 1));
  if (view -> anim -> last -> img -> style != SPHERES && view -> anim -> last -> img -> style != PUNT)
  {
    g_menu_append_submenu (menu, "Bonds(s)", (GMenuModel*)menu_bonds(view, 1));
  }
  return menu;
}
#endif
