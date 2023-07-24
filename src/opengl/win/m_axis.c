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

#ifdef GTK4
extern G_MODULE_EXPORT void axis_advanced (GSimpleAction * action, GVariant * parameter, gpointer data);
#else
extern G_MODULE_EXPORT void axis_advanced (GtkWidget * widg, gpointer data);
#endif
extern GtkWidget * create_layout_widget (gchar * str, GtkWidget * menu, int vl, int vab, gpointer data);

#ifdef GTK3
G_MODULE_EXPORT void set_axis_template_pos (GtkWidget * widg, gpointer data)
{
/* TOP_RIGHT = 0,
   TOP_LEFT = 1,
   BOTTOM_RIGHT = 2,
   BOTTOM_LEFT = 3,
   CENTER = 4 */
  tint * the_data = (tint *)data;
  struct project * this_proj = get_project_by_id(the_data -> a);
  int i = this_proj -> modelgl -> anim -> last -> img -> axispos;
  int j = the_data -> b;
  if (check_menu_item_get_active ((gpointer)widg) && i != j)
  {
    this_proj -> modelgl -> anim -> last -> img -> axispos = NONE;
    if (i != CUSTOM)
    {
      check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_box_axis[1][8+i], FALSE);
    }
    if (widg != this_proj -> modelgl -> ogl_box_axis[1][8+j])
    {
      check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_box_axis[1][8+j], TRUE);
    }
    this_proj -> modelgl -> anim -> last -> img -> axispos = j;
    this_proj -> modelgl -> create_shaders[MAXIS] = TRUE;
    update (this_proj -> modelgl);
  }
  else if (i == j && ! check_menu_item_get_active ((gpointer)widg))
  {
    if (i != CUSTOM)
    {
      check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_box_axis[1][8+i], TRUE);
    }
  }
}

GtkWidget * axis_position_submenu (glwin * view, int id)
{
  int i, j, k;
  GtkWidget * menup = gtk_menu_new ();
  GtkWidget * ud, * udm, * widg;
  gchar * udlab[2] = {"Top", "Bottom"};
  gchar * lrlab[2] = {"Right Corner", "Left Corner"};

  i = 0;
  for (j=0; j<2; j++)
  {
    ud = create_menu_item (FALSE, udlab[j]);
    add_menu_child (menup, ud);
    udm = gtk_menu_new ();
    menu_item_set_submenu (ud, udm);
    for (k=0; k<2; k++)
    {
      if (id == 0)
      {
        view -> ogl_box_axis[1][8+i] = gtk3_menu_item (udm, lrlab[k], IMG_NONE, NULL, G_CALLBACK(set_axis_template_pos), & view -> colorp[i][0],
                                                       FALSE, 0, 0, TRUE, TRUE, (view -> anim -> last -> img -> axispos == i) ? TRUE: FALSE);
        if (view -> anim -> last -> img -> box_axis[AXIS] == NONE) widget_set_sensitive (view -> ogl_box_axis[1][8+i], 0);
      }
      else
      {
        widg = gtk3_menu_item (udm, lrlab[k], IMG_NONE, NULL, G_CALLBACK(set_axis_template_pos), & view -> colorp[i][0],
                               FALSE, 0, 0, TRUE, TRUE, (view -> anim -> last -> img -> axispos == i) ? TRUE: FALSE);
        if (view -> anim -> last -> img -> box_axis[AXIS] == NONE) widget_set_sensitive (widg, 0);
      }
      i += 1;
    }
  }
  if (id == 0)
  {
    view -> ogl_box_axis[1][8+i] = gtk3_menu_item (menup, "Center", IMG_NONE, NULL, G_CALLBACK(set_axis_template_pos), & view -> colorp[i][0],
                                                   FALSE, 0, 0, TRUE, TRUE, (view -> anim -> last -> img -> axispos == i) ? TRUE: FALSE);
    if (view -> anim -> last -> img -> box_axis[AXIS] == NONE) widget_set_sensitive (view -> ogl_box_axis[1][8+i], 0);
  }
  else
  {
    widg = gtk3_menu_item (menup, "Center", IMG_NONE, NULL, G_CALLBACK(set_axis_template_pos), & view -> colorp[i][0],
                           FALSE, 0, 0, TRUE, TRUE, (view -> anim -> last -> img -> axispos == i) ? TRUE: FALSE);
    if (view -> anim -> last -> img -> box_axis[AXIS] == NONE) widget_set_sensitive (widg, 0);
  }
  return menup;
}

void menu_axis (GtkWidget * menu_ab, glwin * view, int id)
{
  GtkWidget * widg = create_menu_item (FALSE, "Length");
  add_menu_child (menu_ab, widg);
  GtkWidget * menul = gtk_menu_new ();
  menu_item_set_submenu (widg, menul);

  gchar * str = g_strdup_printf (" Length [ %f Å ]", view -> anim -> last -> img -> axis_length);
  if (id == 0)
  {
    view -> ogl_box_axis[1][7] = create_layout_widget (str, menul, 1, view -> anim -> last -> img -> box_axis[AXIS], & view -> colorp[0][AXIS]);
    if (view -> anim -> last -> img -> box_axis[AXIS] == NONE)
    {
      widget_set_sensitive (view -> ogl_box_axis[1][7], 0);
    }
  }
  else
  {
    widg = create_layout_widget (str, menul, 1, view -> anim -> last -> img -> box_axis[AXIS], & view -> colorp[0][AXIS]);
    if (view -> anim -> last -> img -> box_axis[AXIS] == NONE)
    {
      widget_set_sensitive (widg, 0);
    }
  }
  g_free (str);
  add_menu_child (menu_ab, menu_item_new_with_submenu ("Position", TRUE, axis_position_submenu(view, id)));
  add_advanced_item (menu_ab, G_CALLBACK(axis_advanced), (gpointer)view, FALSE, 0, 0);
}
#else
G_MODULE_EXPORT void change_axis_pos_radio (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  /* TOP_RIGHT = 0,
     TOP_LEFT = 1,
     BOTTOM_RIGHT = 2,
     BOTTOM_LEFT = 3,
     CENTER = 4 */
  tint * the_data = (tint *)data;
  glwin * view = get_project_by_id(the_data -> a) -> modelgl;
  const gchar * pos = g_variant_get_string (parameter, NULL);
  int lgt = strlen (pos);
  gchar * name = g_strdup_printf ("%c%c", pos[lgt-2], pos[lgt-1]);
  if (g_strcmp0(name, ".1") == 0)
  {
    g_free (name);
    name = g_strdup_printf ("%.*s.0", lgt-2, pos);
    g_action_group_activate_action ((GActionGroup *)view -> action_group, "set-axis-pos", g_variant_new_string((const gchar *)name));
    g_free (name);
  }
  else
  {
    int i;
    gchar * pos_name = NULL;
    for (i=0; i<5; i++)
    {
      pos_name = g_strdup_printf ("set-axis-pos.%d.0", i);
      if (g_strcmp0(pos, (const gchar *)pos_name) == 0)
      {
        view -> anim -> last -> img -> axispos = i;
        g_free (pos_name);
        pos_name = NULL;
        break;
      }
      g_free (pos_name);
      pos_name = NULL;
    }
    g_action_change_state (G_ACTION (action), parameter);
    view -> create_shaders[MAXIS] = TRUE;
    update (view);
    g_action_change_state (G_ACTION (action), parameter);
  }
}

GMenu * position_submenu (glwin * view, int popm, int pos)
{
  GMenu * menu = g_menu_new ();
  gchar * lrlab[2] = {"Right Corner", "Left Corner"};
  int i, j;
  i = pos*2;
  for (j=0; j<2; j++)
  {
    append_opengl_item (view, menu, lrlab[j], "axis-pos", popm, i, NULL, IMG_NONE, NULL, FALSE,
                        G_CALLBACK(change_axis_pos_radio), & view -> colorp[i][0],
                        FALSE, (view -> anim -> last -> img -> axispos == i) ? TRUE: FALSE, TRUE, TRUE);
    i ++;
  }
  return menu;
}

GMenu * axis_position_submenu (glwin * view, int popm)
{
  int i;
  GMenu * menu = g_menu_new ();
  gchar * udlab[2] = {"Top", "Bottom"};
  for (i=0; i<2; i++)
  {
    append_submenu (menu, udlab[i], position_submenu(view, popm, i));
  }
  append_opengl_item (view, menu, "Center", "axis-pos", popm, 4, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(change_axis_pos_radio), & view -> colorp[4][0],
                      FALSE, (view -> anim -> last -> img -> axispos == 4) ? TRUE: FALSE, TRUE, TRUE);
  return menu;
}

void menu_axis (GMenu * menu_ab, glwin * view, int popm)
{
  GMenuItem * item = g_menu_item_new ("Length", (view -> anim -> last -> img -> box_axis[AXIS]) != NONE ? NULL : "None");
  g_menu_item_set_attribute (item, "custom", "s", "axis-length", NULL);
  g_menu_item_set_submenu (item, (GMenuModel *)axis_box_param (view, popm, AXIS, NONE));
  g_menu_append_item (menu_ab, item);
  append_submenu (menu_ab, "Position", axis_position_submenu(view, popm));
  append_opengl_item (view, menu_ab, "Advanced", "axis-advanced", popm, popm, NULL, IMG_STOCK, DPROPERTIES, FALSE,
                      G_CALLBACK(axis_advanced), (gpointer)view, FALSE, FALSE, FALSE, TRUE);
}
#endif
