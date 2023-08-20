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
* This file: 'm_poly.c'
*
*  Contains:
*

 - The subroutines to create the 'Chemistry -> Polyhedra' submenus

*
*  List of subroutines:

  G_MODULE_EXPORT void show_hide_poly (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void show_hide_poly (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void cloned_poly (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void cloned_poly (GtkWidget * widg, gpointer data);

  GtkWidget * mpoly (glwin * view, int jd, int id);
  GtkWidget * menupoly (glwin * view, int jd, int id, gchar * poln);
  GtkWidget * menu_poly (glwin * view, int id);

  GMenu * menu_show_coord_poly (glwin * view, int popm, int id);
  GMenu * menu_show_rings_poly (glwin * view, int popm, int id);
  GMenu * add_menu_poly (glwin * view, int popm, int aid);
  GMenu * menu_poly_rings (glwin * view, int popm);
  GMenu * menu_poly (glwin * view, int popm);

*/

#include "global.h"
#include "interface.h"
#include "glview.h"
#include "glwindow.h"

extern G_MODULE_EXPORT void coord_properties (GtkWidget * widg, gpointer data);

#ifdef GTK4
extern G_MODULE_EXPORT void to_coord_properties (GSimpleAction * action, GVariant * parameter, gpointer data);

/*
*  G_MODULE_EXPORT void show_hide_poly (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage: show / hide polyhedra callback - GTK4
*
*  GSimpleAction * action : the GAction sending the signal
*  GVariant * parameter   : GVariant parameter of the GAction
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT void show_hide_poly (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*
*  G_MODULE_EXPORT void show_hide_poly (GtkWidget * widg, gpointer data)
*
*  Usage: show / hide polyhedra callback - GTK3
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void show_hide_poly (GtkWidget * widg, gpointer data)
#endif
{
  qint * obj = (qint *)data;
  int i, j;
  gboolean doit = TRUE;
  gboolean show;
  struct project * this_proj = get_project_by_id(obj -> a);
  int s = obj -> b;
  int c = obj -> c;
  int g = obj -> d;
  j = c;
  if (g < 2)
  {
    for (i=0; i<s; i++)
    {
      j += this_proj -> coord -> ntg[g][i];
    }
  }
#ifdef DEBUG
  g_debug ("SHOW_HIDE_POLY:: p= %d, s= %d, c= %d, g= %d", this_proj -> id, s, c, g);
#endif
#ifdef GTK4
  GVariant * state;
  if (action)
  {
    state = g_action_get_state (G_ACTION (action));
    show = ! g_variant_get_boolean (state);
    const gchar * poly = g_action_get_name ((GAction *)action);
    int lgt = strlen (poly);
    gchar * name = g_strdup_printf ("%c%c", poly[lgt-2], poly[lgt-1]);
    if (g_strcmp0(name, ".1") == 0)
    {
      g_free (name);
      name = g_strdup_printf ("%.*s.0", lgt-2, poly);
      g_action_group_activate_action ((GActionGroup *)this_proj -> modelgl -> action_group, (const gchar *)name, NULL);
      g_free (name);
      doit = FALSE;
    }
  }
  else
  {
    show = this_proj -> modelgl -> anim -> last -> img -> show_poly[g][j];
  }
#else
  show = gtk_check_menu_item_get_active ((GtkCheckMenuItem *)widg);
#endif
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
          if (gtk_check_menu_item_get_active ((GtkCheckMenuItem *)this_proj -> modelgl -> ogl_poly[i][g][j]) != show)
          {
            gtk_check_menu_item_set_active ((GtkCheckMenuItem *)this_proj -> modelgl -> ogl_poly[i][g][j], show);
          }
        }
      }
    }
  }
#endif
  if (doit)
  {
    this_proj -> modelgl -> anim -> last -> img -> show_poly[g][j] = show;
    int shaders[2] = {POLYS, RINGS};
    re_create_md_shaders (2, shaders, this_proj);
    update (this_proj -> modelgl);
#ifdef GTK4
    if (action)
    {
      g_action_change_state (G_ACTION (action), g_variant_new_boolean (show));
      g_variant_unref (state);
    }
#endif
  }
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void cloned_poly (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage: cloned polyehdra callback - GTK4
*
*  GSimpleAction * action : the GAction sending the signal
*  GVariant * parameter   : GVariant parameter of the GAction
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT void cloned_poly (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*
*  G_MODULE_EXPORT void cloned_poly (GtkWidget * widg, gpointer data)
*
*  Usage: cloned polyehdra callback - GTK3
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void cloned_poly (GtkWidget * widg, gpointer data)
#endif
{
  glwin * view = (glwin *)data;
  gboolean doit = TRUE;
  gboolean show;
#ifdef GTK4
  const gchar * name = g_action_get_name ((GAction *)action);
  if (g_strcmp0(name, "set-cloned-poly.1.1") == 0)
  {
    g_action_group_activate_action ((GActionGroup *)view -> action_group, "set-cloned-poly.0.0", NULL);
    doit = FALSE;
  }
#endif
  if (doit)
  {
#ifdef GTK4
    GVariant * state;
    state = g_action_get_state (G_ACTION (action));
    show = ! g_variant_get_boolean (state);
#else
    show = gtk_check_menu_item_get_active ((GtkCheckMenuItem *)widg);
    if (widg != view -> ogl_clones[5]) gtk_check_menu_item_set_active ((GtkCheckMenuItem *)view -> ogl_clones[5], show);
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
}


#ifdef GTK3
/*
*  GtkWidget * mpoly (glwin * view, int jd, int id)
*
*  Usage: update the 'Polyhedra -> * -> Show/Hide' subemnus - GTK3
*
*  glwin * view : the target glwin
*  int jd       : main app (0) or popup (1)
*  int id       : the type of coordination
*/
GtkWidget * mpoly (glwin * view, int jd, int id)
{
  int i, j;
  struct project * this_proj = get_project_by_id(view -> proj);
  GtkWidget * menup = gtk_menu_new ();
  if (is_coord_in_menu(id, this_proj))
  {
    GtkWidget * pshow = create_menu_item (FALSE, "Show/Hide");
    GtkWidget * widg;
    gtk_menu_shell_append ((GtkMenuShell *)menup, pshow);
    GtkWidget * menus = gtk_menu_new ();
    gtk_menu_item_set_submenu ((GtkMenuItem *)pshow, menus);
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
          gtk_menu_shell_append ((GtkMenuShell *)menus, view -> oglmpv[jd][id][i]);
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
          gtk_menu_shell_append ((GtkMenuShell *)menus, view -> ogl_poly[jd][id][i]);
        }
      }
    }
  }
  add_advanced_item (menup, G_CALLBACK(coord_properties), (gpointer)& view -> colorp[id][0], FALSE, 0, 0);
  return menup;
}

/*
*  GtkWidget * menupoly (glwin * view, int jd, int id, gchar * poln)
*
*  Usage: update the polyhedra submenus - GTK3
*
*  glwin * view : the target glwin
*  int jd       : main app (0) or popup (1)
*  int id       : the type of coordination: total (0), partial (1), rings (2)
*  gchar * poln : the label of the menu item
*/
GtkWidget * menupoly (glwin * view, int jd, int id, gchar * poln)
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
            gtk_menu_item_set_submenu ((GtkMenuItem *)view -> ogl_rings[7+i], mpoly(view, jd, 4+i));
            gtk_menu_shell_append ((GtkMenuShell *)menui, view -> ogl_rings[7+i]);
          }
          else
          {
            item = create_menu_item (TRUE, rings_type[i]);
            gtk_menu_shell_append ((GtkMenuShell *)menui, item);
            gtk_menu_item_set_submenu ((GtkMenuItem *)item, mpoly(view, jd, 4+i));
          }
        }
      }
    }
    return menui;
  }
  else
  {
    GtkWidget * poly = create_menu_item (TRUE, poln);
    gtk_menu_item_set_submenu ((GtkMenuItem *)poly, mpoly (view, jd, id));
    return poly;
  }
}

/*
*  GtkWidget * menu_poly (glwin * view, int id)
*
*  Usage: create the 'Chemistry -> Polyehdra' submenu - GTK3
*
*  glwin * view : the target glwin
*  int id       : main app (0) or popup (1)
*/
GtkWidget * menu_poly (glwin * view, int id)
{
  GtkWidget * menup = gtk_menu_new ();
  gtk_menu_shell_append ((GtkMenuShell *)menup, menupoly(view, id, 0, "Total Coordination(s)"));
  gtk_menu_shell_append ((GtkMenuShell *)menup, menupoly(view, id, 1, "Partial Coordination(s)"));
  if (id == 0)
  {
    gtk_menu_shell_append ((GtkMenuShell *)menup, view -> ogl_rings[6]);
    widget_set_sensitive (view -> ogl_rings[6], view -> rings);
    view -> ogl_clones[5] = gtk3_menu_item (menup, "Cloned Polyhedra", IMG_NONE, NULL, G_CALLBACK(cloned_poly), view, FALSE, 0, 0, TRUE, FALSE, view -> anim -> last -> img -> cloned_poly);
  }
  else
  {
    GtkWidget * item = create_menu_item (FALSE, "Ring(s)");
    gtk_menu_item_set_submenu ((GtkMenuItem *)item, menupoly(view, id, 2, NULL));
    gtk_menu_shell_append ((GtkMenuShell *)menup, item);
    widget_set_sensitive (item, view -> rings);
    GtkWidget * cloned_p =  gtk3_menu_item (menup, "Cloned Polyhedra", IMG_NONE, NULL, G_CALLBACK(cloned_poly), view, FALSE, 0, 0, TRUE, FALSE, view -> anim -> last -> img -> cloned_poly);
    widget_set_sensitive ((cloned_p), get_project_by_id(view -> proj) -> cell.pbc);
  }
  return menup;
}
#else
/*
*  GMenu * menu_show_coord_poly (glwin * view, int popm, int id)
*
*  Usage: create the 'Polyedra -> Coordination -> Show/Hide' submenus - GTK4
*
*  glwin * view : the target glwin
*  int popm     : main app (0) or popup (1)
*  int id       : the coordination type: total (0) or partial (1)
*/
GMenu * menu_show_coord_poly (glwin * view, int popm, int id)
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
          strb = g_strdup_printf ("%s-%d-p", stra, id);
          append_opengl_item (view, menus, stra, strb, popm, k+j, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(show_hide_poly), & view -> gcid[id][k+j][id],
                              TRUE, view -> anim -> last -> img -> show_poly[id][k+j], FALSE, TRUE);
          g_free (stra);
          g_free (strb);
        }
      }
    }
    append_submenu (menu, this_proj -> chemistry -> label[i], menus);
    g_object_unref (menus);
  }
  return menu;
}

/*
*  GMenu * menu_show_rings_poly (glwin * view, int popm, int id)
*
*  Usage: create the 'Rings(s) -> Show/Hide' subemnus - GTK4
*
*  glwin * view : the target glwin
*  int popm     : main app (0) or popup (1)
*  int id       : the ring(s) type
*/
GMenu * menu_show_rings_poly (glwin * view, int popm, int id)
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
      strb = g_strdup_printf ("%s-p", stra);
      append_opengl_item (view, menu, stra, strb, popm, i, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(show_hide_poly), & view -> gcid[id][i][id],
                          TRUE, view -> anim -> last -> img -> show_poly[id][i], FALSE, TRUE);
      g_free (stra);
      g_free (strb);
    }
  }
  return menu;
}

/*
*  GMenu * add_menu_poly (glwin * view, int popm, int aid)
*
*  Usage: create the 'Show/Hide' polyhedra subemnus - GTK4
*
*  glwin * view : the target glwin
*  int popm     : main app (0) or popup (1)
*  int aid      : coordination id
*/
GMenu * add_menu_poly (glwin * view, int popm, int aid)
{
  GMenu * menu = g_menu_new ();
  if (aid < 2)
  {
    append_submenu (menu, "Show/Hide", menu_show_coord_poly (view, popm, aid));
  }
  else
  {
    append_submenu (menu, "Show/Hide", menu_show_rings_poly (view, popm, aid));
  }
  append_opengl_item (view, menu, "Advanced", "adv-p", popm, aid, NULL, IMG_STOCK, (gpointer)DPROPERTIES, FALSE,
                      G_CALLBACK(to_coord_properties), & view -> colorp[aid][0], FALSE, FALSE, FALSE, TRUE);
  return menu;
}

/*
*  GMenu * menu_poly_rings (glwin * view, int popm)
*
*  Usage: create the 'Polyhedra -> Ring(s)' submenu - GTK4
*
*  glwin * view : the target glwin
*  int popm     : main app (0) or popup (1)
*/
GMenu * menu_poly_rings (glwin * view, int popm)
{
  GMenu * menu = g_menu_new ();
  int i;
  for (i=0; i<5; i++)
  {
    if (view -> ring_max[i])
    {
       append_submenu (menu, rings_type[i], add_menu_poly(view, popm, 4+i));
    }
  }
  return menu;
}

/*
*  GMenu * menu_poly (glwin * view, int popm)
*
*  Usage: create the 'Chemistry -> Polyehdra' submenu - GTK4
*
*  glwin * view : the target glwin
*  int popm     : main app (0) or popup (1)
*/
GMenu * menu_poly (glwin * view, int popm)
{
  GMenu * menu = g_menu_new ();
  append_submenu (menu, "Total Coordination(s)", add_menu_poly (view, popm, 0));
  append_submenu (menu, "Partial Coordination(s)", add_menu_poly (view, popm, 1));
  if (view -> rings)
  {
    append_submenu (menu, "Rings(s)", menu_poly_rings (view, popm));
  }
  else
  {
    append_menu_item (menu, "Ring(s)", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  }
  append_opengl_item (view, menu, "Cloned Polyhedra", "cloned-poly", popm, popm, NULL, IMG_NONE, NULL, FALSE,
                      G_CALLBACK(cloned_poly), view, TRUE, view -> anim -> last -> img -> cloned_poly, FALSE, view -> allbonds[1]);
  return menu;
}
#endif
