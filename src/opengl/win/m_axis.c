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
  const gchar * style = g_variant_get_string (parameter, NULL);
  gchar * str = NULL;
  g_debug ("style= %s", style);
  /* TOP_RIGHT = 0,
  TOP_LEFT = 1,
  BOTTOM_RIGHT = 2,
  BOTTOM_LEFT = 3,
  CENTER = 4 */
  tint * the_data = (tint *)data;
  glwin * view = get_project_by_id(the_data -> a) -> modelgl;
  int i;
  for (i=0; i<5; i++)
  {
    str = g_strdup_printf ("set-axis-pos.%d", i);
    if (g_strcmp0(style, (const gchar *)str) == 0)
    {
      view -> anim -> last -> img -> axispos = i;
      g_free (str);
      str = NULL;
      break;
    }
    g_free (str);
    str = NULL;
  }
  g_action_change_state (G_ACTION (action), parameter);
  view -> create_shaders[MAXIS] = TRUE;
  update (view);
  update_menu_bar (view);
}

GMenu * position_submenu (glwin * view, int pos)
{
  GMenu * menu = g_menu_new ();
  gchar * lrlab[2] = {"Right Corner", "Left Corner"};
  int i, j;
  i = pos*2;
  for (j=0; j<2; j++)
  {
    append_opengl_item (view, menu, lrlab[j], "axis-pos", i, NULL, IMG_NONE, NULL, FALSE,
                        G_CALLBACK(change_axis_pos_radio), & view -> colorp[i][0],
                        FALSE, (view -> anim -> last -> img -> axispos == i) ? TRUE: FALSE, TRUE, TRUE);
    i ++;
  }
  return menu;
}

GMenu * axis_position_submenu (glwin * view)
{
  int i;
  GMenu * menu = g_menu_new ();
  gchar * udlab[2] = {"Top", "Bottom"};
  for (i=0; i<2; i++)
  {
    g_menu_append_submenu (menu, udlab[i], (GMenuModel *)position_submenu(view, i));
  }
  append_opengl_item (view, menu, "Center", "axis-pos", 4, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(change_axis_pos_radio), & view -> colorp[4][0],
                      FALSE, (view -> anim -> last -> img -> axispos == 4) ? TRUE: FALSE, TRUE, TRUE);
  return menu;
}

void menu_axis (GMenu * menu_ab, glwin * view)
{
  GMenuItem * item = g_menu_item_new ("Length", (view -> anim -> last -> img -> box_axis[AXIS]) != NONE ? NULL : "None");
  g_menu_item_set_attribute (item, "custom", "s", "axis-length", NULL);
  g_menu_item_set_submenu (item, (GMenuModel *)axis_box_param (view, AXIS, NONE));
  g_menu_append_item (menu_ab, item);
  g_menu_append_submenu (menu_ab, "Position", (GMenuModel *)axis_position_submenu(view));
  append_opengl_item (view, menu_ab, "Advanced", "axis-advanced", 0, NULL, IMG_STOCK, DPROPERTIES, FALSE,
                      G_CALLBACK(axis_advanced), (gpointer)view, FALSE, FALSE, FALSE, TRUE);
}
#endif
