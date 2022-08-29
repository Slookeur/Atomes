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
#include "glview.h"
#include "glwindow.h"

extern G_MODULE_EXPORT void coord_properties (GtkWidget * widg, gpointer data);

#ifdef GTK4
extern G_MODULE_EXPORT void to_coord_properties (GSimpleAction * action, GVariant * parameter, gpointer data);

G_MODULE_EXPORT void show_hide_poly (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void show_hide_poly (GtkWidget * widg, gpointer data)
#endif
{
  qint * obj = (qint *)data;
  int i, j;
  gboolean show;
  struct project * this_proj = get_project_by_id(obj -> a);
  int s = obj -> b;
  int c = obj -> c;
  int g = obj -> d;
#ifdef DEBUG
  g_debug ("SHOW_HIDE_POLY:: p= %d, s= %d, c= %d, g= %d", this_proj -> id, s, c, g);
#endif
#ifdef GTK4
  GVariant * state = g_action_get_state (G_ACTION (action));
  show = ! g_variant_get_boolean (state);
#else
  show = check_menu_item_get_active ((gpointer)widg);
#endif
  j = c;
  if (g < 2)
  {
    for (i=0; i<s; i++)
    {
      j += this_proj -> coord -> ntg[g][i];
    }
  }
#ifdef GTK3
  // GTK3 Menu Action To Check
  if (is_coord_in_menu(g, this_proj))
  {
    for (i=0; i<2; i++)
    {
      if (widg != this_proj -> modelgl -> ogl_poly[i][g][j])
      {
        if (GTK_IS_WIDGET(this_proj -> modelgl -> ogl_poly[i][g][j]))
        {
          if (check_menu_item_get_active ((gpointer)this_proj -> modelgl -> ogl_poly[i][g][j]) != show)
          {
            check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_poly[i][g][j], show);
          }
        }
      }
    }
  }
#endif
  g_debug ("show= %d", show);
  this_proj -> modelgl -> anim -> last -> img -> show_poly[g][j] = show;
  int shaders[2] = {POLYS, RINGS};
  re_create_md_shaders (2, shaders, this_proj);
  update (this_proj -> modelgl);
#ifdef GTK4
  g_action_change_state (G_ACTION (action), g_variant_new_boolean (show));
  g_variant_unref (state);
#endif
}

#ifdef GTK4
G_MODULE_EXPORT void cloned_poly (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void cloned_poly (GtkWidget * widg, gpointer data)
#endif
{
  glwin * view = (glwin *)data;
  gboolean show;
#ifdef GTK4
  GVariant * state = g_action_get_state (G_ACTION (action));
  show = ! g_variant_get_boolean (state);
#else
  show = check_menu_item_get_active ((gpointer)widg);
  if (widg != view -> ogl_clones[5]) check_menu_item_set_active ((gpointer)view -> ogl_clones[6], show);
#endif
  view -> anim -> last -> img -> cloned_poly = show;
  int shaders[2] = {POLYS, RINGS};
  re_create_md_shaders (2, shaders, get_project_by_id(view -> proj));
  update (view);
#ifdef GTK4
  g_action_change_state (G_ACTION (action), g_variant_new_boolean (show));
  g_variant_unref (state);
#endif
}


#ifdef GTK3
GtkWidget * mpoly (glwin * view, int jd, int id)
{
  int i, j;
  struct project * this_proj = get_project_by_id(view -> proj);
  GtkWidget * menup = gtk_menu_new ();
  if (is_coord_in_menu(id, this_proj))
  {
    GtkWidget * pshow = create_menu_item (FALSE, "Show/Hide");
    GtkWidget * widg;
    add_menu_child (menup, pshow);
    GtkWidget * menus = gtk_menu_new ();
    menu_item_set_submenu (pshow, menus);
    j = (id < 2) ? this_proj -> nspec : this_proj -> coord -> totcoord[id];
    for (i=0; i<j; i++)
    {
      if (id < 2)
      {
        if (GTK_IS_WIDGET(view -> oglmpv[jd][id][i]))
        {
          widg =  gtk_widget_get_parent (view -> oglmpv[jd][id][i]);
          if (GTK_IS_WIDGET(widg))
          {
            g_object_ref (view -> oglmpv[jd][id][i]);
            gtk_container_remove (GTK_CONTAINER(widg), view -> oglmpv[jd][id][i]);
          }
          add_menu_child (menus, view -> oglmpv[jd][id][i]);
        }
      }
      else
      {
        if (GTK_IS_WIDGET(view -> ogl_poly[jd][id][i]))
        {
          widg =  gtk_widget_get_parent (view -> ogl_poly[jd][id][i]);
          if (GTK_IS_WIDGET(widg))
          {
            g_object_ref (view -> ogl_poly[jd][id][i]);
            gtk_container_remove (GTK_CONTAINER(widg), view -> ogl_poly[jd][id][i]);
          }
          add_menu_child (menus, view -> ogl_poly[jd][id][i]);
        }
      }
    }
  }
  add_advanced_item (menup, G_CALLBACK(coord_properties), (gpointer)& view -> colorp[id][0], FALSE, 0, 0);
  return menup;
}

GtkWidget * menupoly (glwin * view, int jd, int id, int hd, gchar * poln)
{
  int i;
  GtkWidget * item;
  if (id > 1)
  {
    GtkWidget * menui = gtk_menu_new ();
    if (view -> rings)
    {
      for (i=0; i<5; i++)
      {
        if (view -> ring_max[i])
        {
          if (jd == 0)
          {
            view -> ogl_rings[7+i] = create_menu_item (TRUE, rings_type[i]);
            menu_item_set_submenu (view -> ogl_rings[7+i], mpoly(view, jd, 4+i));
            add_menu_child (menui, view -> ogl_rings[7+i]);
          }
          else
          {
            item = create_menu_item (TRUE, rings_type[i]);
            add_menu_child (menui, item);
            menu_item_set_submenu (item, mpoly(view, jd, 4+i));
          }
        }
      }
    }
    return menui;
  }
  else
  {
    GtkWidget * poly = create_menu_item (TRUE, poln);
    menu_item_set_submenu (poly, mpoly (view, jd, id));
    return poly;
  }
}

GtkWidget * menu_poly (glwin * view, int id)
{
  GtkWidget * menup = gtk_menu_new ();
  add_menu_child (menup, menupoly(view, id, 0, 0, "Total Coordination(s)"));
  add_menu_child (menup, menupoly(view, id, 1, 0, "Partial Coordination(s)"));
  if (id == 0)
  {
    add_menu_child (menup, view -> ogl_rings[6]);
    widget_set_sensitive (view -> ogl_rings[6], view -> rings);
    view -> ogl_clones[5] = gtk3_menu_item (menup, "Cloned Polyhedra", IMG_NONE, NULL, G_CALLBACK(cloned_poly), view, FALSE, 0, 0, TRUE, FALSE, view -> anim -> last -> img -> cloned_poly);
  }
  else
  {
    GtkWidget * item = create_menu_item (FALSE, "Ring(s)");
    menu_item_set_submenu (item, menupoly(view, id, 2, 0, NULL));
    add_menu_child (menup, item);
    widget_set_sensitive (item, view -> rings);
    GtkWidget * cloned_p =  gtk3_menu_item (menup, "Cloned Polyhedra", IMG_NONE, NULL, G_CALLBACK(cloned_poly), view, FALSE, 0, 0, TRUE, FALSE, view -> anim -> last -> img -> cloned_poly);
    widget_set_sensitive ((cloned_p), get_project_by_id(view -> proj) -> cell.pbc);
  }
  return menup;
}
#else
GMenu * menu_show_coord_poly (glwin * view, int id)
{
  GMenu * menu = g_menu_new ();
  GMenu * menus;
  struct project * this_proj = get_project_by_id (view -> proj);
  gchar * stra,  * strb;
  int i, j, k;
  for (i=0; i<this_proj -> nspec; i++)
  {
    j = 0;
    menus = g_menu_new ();
    if (this_proj -> coord)
    {
      if (this_proj -> coord -> ntg[id])
      {
        for (k=0; k<i; k++)
        {
          j += this_proj -> coord -> ntg[id][k];
        }
        for (k=0; k < this_proj -> coord -> ntg[id][i]; k++)
        {
          if (id)
          {
            stra = exact_name (env_name (this_proj, k, i, 1, NULL));
          }
          else
          {
            stra = g_strdup_printf ("%d", this_proj -> coord -> geolist[id][i][k]);
          }
          strb = g_strdup_printf ("%s-%d-0", stra, id);
          append_opengl_item (view, menus, stra, strb, k+j, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(show_hide_poly), & view -> gcid[id][k+j][id],
                              TRUE, view -> anim -> last -> img -> show_poly[id][k+j], FALSE, TRUE);
          g_free (stra);
          g_free (strb);
        }
      }
    }
    g_menu_append_submenu (menu, this_proj -> chemistry -> label[i], (GMenuModel*)menus);
    g_object_unref (menus);
  }
  return menu;
}

GMenu * menu_show_rings_poly (glwin * view, int id)
{
  GMenu * menu = g_menu_new ();
  struct project * this_proj = get_project_by_id (view -> proj);
  if (this_proj -> coord)
  {
    int i;
    gchar * stra,  * strb;
    for (i=0; i<this_proj -> coord -> totcoord[id]; i++)
    {
      stra = g_strdup_printf ("%d", this_proj -> coord -> geolist[id][0][i]);
      strb = g_strdup_printf ("%s-0", stra);
      append_opengl_item (view, menu, stra, strb, i, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(show_hide_poly), & view -> gcid[id][i][id],
                          TRUE, view -> anim -> last -> img -> show_poly[id][i], FALSE, TRUE);
      g_free (stra);
      g_free (strb);
    }
  }
  return menu;
}

GMenu * add_menu_poly (glwin * view, int id)
{
  GMenu * menu = g_menu_new ();
  if (id < 2)
  {
    g_menu_append_submenu (menu, "Show/Hide", (GMenuModel*)menu_show_coord_poly (view, id));
  }
  else
  {
    g_menu_append_submenu (menu, "Show/Hide", (GMenuModel*)menu_show_rings_poly (view, id));
  }
  append_opengl_item (view, menu, "Advanced", "adv-p", id, NULL, IMG_STOCK, (gpointer)DPROPERTIES, FALSE,
                      G_CALLBACK(to_coord_properties), & view -> colorp[id][0], FALSE, FALSE, FALSE, TRUE);
  return menu;
}

GMenu * menu_poly_rings (glwin * view)
{
  GMenu * menu = g_menu_new ();
  int i;
  for (i=0; i<5; i++)
  {
    if (view -> ring_max[i])
    {
       g_menu_append_submenu (menu, rings_type[i], (GMenuModel*)add_menu_poly(view, 4+i));
    }
  }
  return menu;
}

GMenu * menu_poly (glwin * view)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Total Coordination(s)", (GMenuModel*)add_menu_poly (view, 0));
  g_menu_append_submenu (menu, "Partial Coordination(s)", (GMenuModel*)add_menu_poly (view, 1));
  if (view -> rings)
  {
    g_menu_append_submenu (menu, "Rings(s)", (GMenuModel*)menu_poly_rings (view));
  }
  else
  {
    append_menu_item (menu, "Ring(s)", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  }
  append_opengl_item (view, menu, "Cloned Polyhedra", "cloned-poly", 0, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(cloned_poly), view, TRUE, view -> anim -> last -> img -> cloned_poly, FALSE, view -> allbonds[1]);
  return menu;
}
#endif
