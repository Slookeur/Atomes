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
#include "color_box.h"
#include "submenus.h"

#ifdef GTK4
G_MODULE_EXPORT void box_advanced (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void window_bonds (GSimpleAction * action, GVariant * parameter, gpointer data);
#else
G_MODULE_EXPORT void box_advanced (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void window_bonds (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void window_lines (GtkWidget * widg, gpointer data);
#endif

#ifdef GTK3
// GTK3 Menu Action To Check
G_MODULE_EXPORT void set_box_axis_style (GtkWidget * widg, gpointer data)
{
  tint * the_data = (tint *)data;
  int j, k, o;
  j = the_data -> b;
  o = the_data -> c;
  struct project * this_proj = get_project_by_id(the_data -> a);
  k = this_proj -> modelgl -> anim -> last -> img -> box_axis[o];
  int dim[2]={OGL_BOX, OGL_AXIS};
  int i, m, l;
  l = (k == NONE) ? 2 : (j == WIREFRAME) ? 2 : 1;
  m = (k == NONE) ? 0 : (j == WIREFRAME) ? 1 : 2;
  if (k != j && check_menu_item_get_active ((gpointer)widg))
  {
    this_proj -> modelgl -> anim -> last -> img -> box_axis[o] = NONE - 1;
    check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_box_axis[o][l], FALSE);
    gtk_widget_show (this_proj -> modelgl -> ogl_box_axis[o][3+2*(m-1)]);
    gtk_widget_hide (this_proj -> modelgl -> ogl_box_axis[o][3+2*(l-1)]);
    if (widg != this_proj -> modelgl -> ogl_box_axis[o][(k == NONE) ? 1 : m])
    {
      check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_box_axis[o][(k == NONE) ? 1 : m], TRUE);
    }
    for (i=1; i<dim[o]; i++) widget_set_sensitive (this_proj -> modelgl -> ogl_box_axis[o][i], 1);
    this_proj -> modelgl -> anim -> last -> img -> box_axis[o] = (k == NONE) ? WIREFRAME : j;
  }
  else if (k == j && ! check_menu_item_get_active ((gpointer)widg))
  {
    check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_box_axis[o][(k == NONE) ? 1 : m], TRUE);
  }
  else if (j == 0)
  {
    for (i=1; i<3; i++)
    {
      check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_box_axis[o][i], FALSE);
      widget_set_sensitive (this_proj -> modelgl -> ogl_box_axis[o][i], 0);
      widget_set_sensitive (this_proj -> modelgl -> ogl_box_axis[o][i], 0);
    }
    widget_set_sensitive (this_proj -> modelgl -> ogl_box_axis[o][4], 0);
    for (i=6; i<dim[o]; i++)
    {
      widget_set_sensitive (this_proj -> modelgl -> ogl_box_axis[o][i], 0);
    }
    if (widg != this_proj -> modelgl -> ogl_box_axis[o][0]) check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_box_axis[o][0], FALSE);
    this_proj -> modelgl -> anim -> last -> img -> box_axis[o] = NONE;
  }
  this_proj -> modelgl -> create_shaders[o+MDBOX] = TRUE;
  update (this_proj -> modelgl);
}

GtkWidget * create_box_axis_menu (char * name, int val, int box, GtkWidget * menu, tint * data)
{
  GtkWidget * box_widget = gtk3_menu_item (menu, name, IMG_NONE, NULL, G_CALLBACK(set_box_axis_style), data,
                                           FALSE, 0, 0, TRUE, (data -> b != 0) ? TRUE : FALSE, (box == val && val != NONE) ? TRUE : FALSE);
  if (data -> b != 0)
  {
    if (val == NONE)
    {
      widget_set_sensitive (box_widget, 0);
    }
  }
  return box_widget;
}

GtkWidget * create_color_widget (GtkWidget * widg, glwin * view, int va, int vb)
{
  GtkWidget * color_widget = color_box(view, -1, 0, 0);
  menu_item_set_submenu (widg, color_widget);
  if (va == vb)
  {
    widget_set_sensitive (color_widget, 0);
  }
  return color_widget;
}

GtkWidget * create_layout_widget (gchar * str, GtkWidget * menu, int vl, int vab, gpointer data)
{
  GtkWidget * layout = create_menu_item (TRUE, str);
  add_menu_child (menu, layout);
  g_signal_connect (G_OBJECT (layout), "activate", G_CALLBACK(window_bonds), data);
  if (vab == NONE) widget_set_sensitive (layout, 0);
  return layout;
}

GtkWidget * menu_box_axis (glwin * view, int id, int ab)
{
  GtkWidget * widg;
  gchar * menu_title[2]={"Box", "Axis"};
  GtkWidget * ab_menu;
  GtkWidget * menu_ab = gtk_menu_new ();
  if (id == 0 && ab == 0)
  {
    view -> ogl_box[0] = create_menu_item (TRUE, menu_title[ab]);
    menu_item_set_submenu (view -> ogl_box[0], menu_ab);
    widget_set_sensitive (view -> ogl_box[0], get_project_by_id(view -> proj) -> cell.ltype);
  }
  else
  {
    ab_menu = create_menu_item (TRUE, menu_title[ab]);
    menu_item_set_submenu (ab_menu, menu_ab);
    if (ab == 0) widget_set_sensitive (ab_menu, get_project_by_id(view -> proj) -> cell.ltype);
  }

  GtkWidget * menul;
  int i = view -> anim -> last -> img -> box_axis[ab];
  if (id == 0)
  {
    view -> ogl_box_axis[ab][0] = create_box_axis_menu ("Show/_Hide", i, i, menu_ab, & view -> colorp[0][ab]);
  }
  else
  {
    widg = create_box_axis_menu ("Show/Hide", i, i, menu_ab, & view -> colorp[0][ab]);
  }
  widg = create_menu_item (FALSE, "Style");
  add_menu_child (menu_ab, widg);
  GtkWidget * menus = gtk_menu_new ();
  menu_item_set_submenu (widg, menus);
  gchar * str;
  str = g_strdup_printf ("Width [ %f pts ]", view -> anim -> last -> img -> box_axis_line[ab]);
  if (id == 0)
  {
    view -> ogl_box_axis[ab][1] = create_box_axis_menu (text_styles[1], i, 1, menus, & view -> colorp[1][ab]);
    view -> ogl_box_axis[ab][2] = create_box_axis_menu (text_styles[4], i, 4, menus, & view -> colorp[4][ab]);
    view -> ogl_box_axis[ab][3] = create_menu_item (TRUE, "_Lines");
    add_menu_child (menu_ab, view -> ogl_box_axis[ab][3]);
    menul = gtk_menu_new ();
    menu_item_set_submenu (view -> ogl_box_axis[ab][3], menul);
    view -> ogl_box_axis[ab][4] = create_layout_widget (str, menul, 0, view -> anim -> last -> img -> box_axis[ab], & view -> colorp[WIREFRAME][ab]);
  }
  else
  {
    create_box_axis_menu (text_styles[1], i, 1, menus, & view -> colorp[1][ab]);
    create_box_axis_menu (text_styles[4], i, 4, menus, & view -> colorp[4][ab]);
    if (i == WIREFRAME)
    {
      widg = create_menu_item (FALSE, "Lines");
      add_menu_child (menu_ab, widg);
      menul = gtk_menu_new ();
      menu_item_set_submenu (widg, menul);
      widg = create_layout_widget (str, menul, 0, view -> anim -> last -> img -> box_axis[ab], & view -> colorp[WIREFRAME][ab]);
    }
  }
  g_free (str);

  str = g_strdup_printf ("Radius [ %f Å ]", view -> anim -> last -> img -> box_axis_rad[ab]);
  if (id == 0)
  {
    view -> ogl_box_axis[ab][5] = create_menu_item (FALSE, "Cylinders");
    add_menu_child (menu_ab, view -> ogl_box_axis[ab][5]);
    menul = gtk_menu_new ();
    menu_item_set_submenu (view -> ogl_box_axis[ab][5], menul);
    view -> ogl_box_axis[ab][6] = create_layout_widget (str, menul, 1, view -> anim -> last -> img -> box_axis[ab], & view -> colorp[CYLINDERS][ab]);
  }
  else if (i == CYLINDERS)
  {
    widg = create_menu_item (FALSE, "Cylinders");
    add_menu_child (menu_ab, widg);
    menul = gtk_menu_new ();
    menu_item_set_submenu (widg, menul);
    widg = create_layout_widget (str, menul, 1, view -> anim -> last -> img -> box_axis[ab], & view -> colorp[CYLINDERS][ab]);
  }
  g_free (str);

  if (ab == 0)
  {
    widg = create_menu_item (FALSE, "Color");
    add_menu_child (menu_ab, widg);
    if (id == 0)
    {
      view -> ogl_box_axis[ab][7] = create_color_widget (widg, view, i, NONE);
    }
    else
    {
      widg = create_color_widget (widg, view, i, NONE);
    }
    add_advanced_item (menu_ab, G_CALLBACK(box_advanced), (gpointer)view, FALSE, 0, 0);
  }
  else
  {
    menu_axis (menu_ab, view, id);
  }

  if (id == 0 && ab == 0)
  {
    return view -> ogl_box[0];
  }
  else
  {
    return ab_menu;
  }
}
#else
G_MODULE_EXPORT void show_hide_box_axis (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  tint * the_data = (tint *)data;
  glwin * view = get_project_by_id(the_data -> a) -> modelgl;
  if (view -> anim -> last -> img -> box_axis[the_data -> c] != NONE)
  {
    view -> anim -> last -> img -> box_axis[the_data -> c] = NONE;
  }
  else
  {
    view -> anim -> last -> img -> box_axis[the_data -> c] = WIREFRAME;
  }
  view -> create_shaders[the_data -> c+MDBOX] = TRUE;
  update (view);
  update_menu_bar (view);
}

G_MODULE_EXPORT void change_box_axis_radio (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  tint * the_data = (tint *)data;
  int i, j;
  i = the_data -> c;
  glwin * view = get_project_by_id (the_data -> a) -> modelgl;
  const gchar * style = g_variant_get_string (parameter, NULL);
  gchar * str = g_strdup_printf ("set-%s-style", (i) ? "axis" : "box");
  gchar * style_name = NULL;
  for (j=0; j<2; j++)
  {
    style_name = g_strdup_printf ("%s.%d", str, j);
    if (g_strcmp0(style, (const gchar *)style_name) == 0)
    {
      view -> anim -> last -> img -> box_axis[i] = (j == 0) ? WIREFRAME : CYLINDERS;
      g_free (style_name);
      style_name = NULL;
      break;
    }
    if (style_name)
    {
      g_free (style_name);
      style_name = NULL;
    }
  }
  g_action_change_state (G_ACTION (action), parameter);
  view -> create_shaders[i+MDBOX] = TRUE;
  update (view);
  update_menu_bar (view);
}

GMenu * axis_box_style (glwin * view, int ab, int abs)
{
  GMenu * menu = g_menu_new ();
  gchar * str = g_strdup_printf ("%s-style", (ab) ? "axis" : "box");
  append_opengl_item (view, menu, text_styles[1], str, 0, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(change_box_axis_radio), & view -> colorp[1][ab], FALSE, (abs == WIREFRAME) ? TRUE : FALSE,
                      TRUE, (abs != NONE) ? TRUE : FALSE);
  g_free (str);
  str = g_strdup_printf ("%s-style", (ab) ? "axis" : "box");
  append_opengl_item (view, menu, text_styles[4], str, 1, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(change_box_axis_radio), & view -> colorp[4][ab], FALSE, (abs == CYLINDERS) ? TRUE : FALSE,
                      TRUE, (abs != NONE) ? TRUE : FALSE);
  g_free (str);
  return menu;
}

GMenu * axis_box_param (glwin * view, int ab, int style)
{
  gchar * str, * key;
  if (style == WIREFRAME)
  {
    str = g_strdup_printf ("Width [ %f pts ]", view -> anim -> last -> img -> box_axis_line[ab]);
    key = g_strdup_printf ("%s-width", (ab) ? "axis" : "box");
  }
  else if (style == CYLINDERS)
  {
    str = g_strdup_printf ("Radius [ %f Å ]", view -> anim -> last -> img -> box_axis_rad[ab]);
    key = g_strdup_printf ("%s-radius", (ab) ? "axis" : "box");
  }
  else
  {
    str = g_strdup_printf (" length [ %f Å ]", view -> anim -> last -> img -> axis_length);
    key = g_strdup_printf ("axis-legnth");
  }
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, str, key, 0, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(window_bonds), & view -> colorp[style][ab], FALSE, FALSE, FALSE, TRUE);
  g_free (str);
  g_free (key);
  return menu;
}

GMenuItem * menu_box_axis (glwin * view, int ab)
{
  GMenuItem * ab_item = g_menu_item_new ((ab) ? "Axis" : "Box", (ab) ? NULL : (get_project_by_id(view -> proj) -> cell.ltype) ? NULL : "None");
  int i = view -> anim -> last -> img -> box_axis[ab];
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, "Show/Hide", (ab) ? "show-axis" : "show-box", 0, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(show_hide_box_axis), & view -> colorp[0][ab], TRUE, (i != NONE) ? TRUE : FALSE, FALSE, (ab) ? TRUE : get_project_by_id(view -> proj) -> cell.ltype);

  g_menu_append_submenu (menu, "Style", (GMenuModel*)axis_box_style (view, ab, i));
  GMenuItem * item ;
  if (i == WIREFRAME)
  {
    item = g_menu_item_new ("Lines", (view -> anim -> last -> img -> box_axis[ab]) != NONE ? NULL : "None");
    g_menu_item_set_attribute (item, "custom", "s", (ab) ? "axis-lines" : "box-lines", NULL);
    g_menu_item_set_submenu (item, (GMenuModel *)axis_box_param (view, ab, WIREFRAME));
    g_menu_append_item (menu, item);
  }
  if (i == CYLINDERS)
  {
    item = g_menu_item_new ("Cylinders", (view -> anim -> last -> img -> box_axis[ab]) != NONE ? NULL : "None");
    g_menu_item_set_attribute (item, "custom", "s", (ab) ? "axis-cylinders" : "box-cylinders", NULL);
    g_menu_item_set_submenu (item, (GMenuModel *)axis_box_param (view, ab, CYLINDERS));
    g_menu_append_item (menu, item);
  }

  if (ab == 0)
  {
    GMenu * menuc = g_menu_new ();
    append_opengl_item (view, menu, "box-color", "box-color", 0, NULL, IMG_NONE, NULL, TRUE, NULL, NULL, FALSE, FALSE, FALSE, FALSE);
    append_opengl_item (view, menu, "More colors ...", "box-color", 0, NULL, IMG_NONE, NULL, FALSE,
                        G_CALLBACK(to_run_box_color_window), view, FALSE, FALSE, FALSE, get_project_by_id(view -> proj) -> cell.ltype);
    g_menu_append_submenu (menu, "Color", (GMenuModel*)menuc);
    g_object_unref (menuc);
    append_opengl_item (view, menu, "Advanced", "box-advanced", 0, NULL, IMG_STOCK, DPROPERTIES, FALSE,
                      G_CALLBACK(box_advanced), (gpointer)view, FALSE, FALSE, FALSE, TRUE);
  }
  else
  {
    menu_axis (menu, view);
  }
  g_menu_item_set_submenu (ab_item, (GMenuModel *)menu);
  return ab_item;
}
#endif
