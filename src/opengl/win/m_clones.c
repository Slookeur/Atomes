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
* This file: 'm_clones.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  G_MODULE_EXPORT void show_hide_clones (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void show_hide_clones (GSimpleAction * action, GVariant * parameter, gpointer data);

  GtkWidget * create_clone_widget (char * name, gboolean clone, GtkWidget * menu, glwin * view);
  GtkWidget * menu_clones (glwin * view, int id);

  GMenu * menu_clones (glwin * view, int popm);

*/

#include "global.h"
#include "glview.h"
#include "glwindow.h"
#include "submenus.h"

#ifdef GTK3
/*
*  G_MODULE_EXPORT void show_hide_clones (GtkWidget * widg, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void show_hide_clones (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  gboolean j;
  j = gtk_check_menu_item_get_active ((GtkCheckMenuItem *)widg);
  if (widg != view -> ogl_clones[0])
  {
    gtk_check_menu_item_set_active ((GtkCheckMenuItem *)view -> ogl_clones[0], j);
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

/*
*  GtkWidget * create_clone_widget (char * name, gboolean clone, GtkWidget * menu, glwin * view)
*
*  Usage:
*
*  char * name      :
*  gboolean clone   :
*  GtkWidget * menu : the GtkWidget sending the signal
*  glwin * view     : the target glwin
*/
GtkWidget * create_clone_widget (char * name, gboolean clone, GtkWidget * menu, glwin * view)
{
  GtkWidget * clone_widget = gtk3_menu_item (menu, name, IMG_NONE, NULL, G_CALLBACK(show_hide_clones), view, FALSE, 0, 0, TRUE, FALSE, clone);
  widget_set_sensitive (clone_widget, view -> allbonds[1]);
  return clone_widget;
}

/*
*  GtkWidget * menu_clones (glwin * view, int id)
*
*  Usage:
*
*  glwin * view : the target glwin
*  int id       : main app (0) or popup (1)
*/
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
  gtk_menu_shell_append ((GtkMenuShell *)menu, menu_item_new_with_submenu ("Atom(s)", TRUE, menu_atoms (view, id, 1)));
  if ((id == 0) || (view -> anim -> last -> img -> style != SPHERES && view -> anim -> last -> img -> style != PUNT))
  {
    gtk_menu_shell_append ((GtkMenuShell *)menu, menu_item_new_with_submenu ("Bond(s)", TRUE, menu_bonds (view, id, 1)));
  }
  return menu;
}
#else
/*
*  G_MODULE_EXPORT void show_hide_clones (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage:
*
*  GSimpleAction * action : the GAction sending the signal
*  GVariant * parameter   : GVariant parameter of the GAction
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT void show_hide_clones (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  glwin * view = (glwin *)data;
  const gchar * name = g_action_get_name ((GAction *)action);
  if (g_strcmp0(name, "set-show-clones.1.1") == 0)
  {
    g_action_group_activate_action ((GActionGroup *)view -> action_group, "set-show-clones.0.0", NULL);
  }
  else
  {
    GVariant * state;
    gboolean show;
    if (action)
    {
      state = g_action_get_state (G_ACTION (action));
      show = ! g_variant_get_boolean (state);
    }
    else
    {
      show = ! view -> anim -> last -> img -> draw_clones;
    }
    view -> anim -> last -> img -> draw_clones = show;
    init_default_shaders (view);
    update (view);
    if (action)
    {
      g_action_change_state (G_ACTION (action), g_variant_new_boolean (show));
      g_variant_unref (state);
    }
  }
}

/*
*  GMenu * menu_clones (glwin * view, int popm)
*
*  Usage:
*
*  glwin * view : the target glwin
*  int popm     : main app (0) or popup (1)
*/
GMenu * menu_clones (glwin * view, int popm)
{
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, "Show/Hide", "show-clones", popm, popm, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(show_hide_clones), (gpointer)view, TRUE, view -> anim -> last -> img -> draw_clones, FALSE, (view -> allbonds[1]) ? TRUE : FALSE);
  append_submenu (menu, "Atom(s)", menu_atoms(view, popm, 1));
  if (view -> anim -> last -> img -> style != SPHERES && view -> anim -> last -> img -> style != PUNT)
  {
    append_submenu (menu, "Bonds(s)", menu_bonds(view, popm, 1));
  }
  return menu;
}
#endif
