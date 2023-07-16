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
extern G_MODULE_EXPORT void window_color_coord (GSimpleAction * action, GVariant * parameter, gpointer data);
extern GtkWidget * color_palette (glwin * view, int ideo, int spec, int geo);
#endif

gboolean is_coord_in_menu (int id, struct project * this_proj)
{
  if (((id == 2 || id == 3) && this_proj -> coord -> totcoord[id] <= COORD_MAX_MENU) || id < 2 || id > 3)
  {
    return TRUE;
  }
  else
  {
    return FALSE;
  }
}

#ifdef GTK4
G_MODULE_EXPORT void show_hide_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void show_hide_coord (GtkWidget * widg, gpointer data)
#endif
{
  qint * cid = (qint *)data;
  int i, j, k;
  int c, g, s;
  gboolean show;
  struct project * this_proj = get_project_by_id(cid -> a);
  s = cid -> b;
  c = cid -> c;
  g = cid -> d;
  j = 0;
  if (g < 2)
  {
    for (i=0; i<s; i++)
    {
      j += this_proj -> coord -> ntg[g][i];
    }
  }
  j += c;
#ifdef GTK3
  show = check_menu_item_get_active ((gpointer)widg);
#else
  GVariant * state;
  if (action)
  {
    state = g_action_get_state (G_ACTION (action));
    show = ! g_variant_get_boolean (state);
  }
  else
  {
    show = this_proj -> modelgl -> anim -> last -> img -> show_coord[g][j];
  }
#endif
#ifdef DEBUG
  g_debug ("SHOW_HIDE_COORD:: p= %d, s= %d, c= %d, g= %d, show= %d", this_proj -> id, s, c, g, show);
#endif

#ifdef GTK3
  // GTK3 Menu Action To Check
  if (is_coord_in_menu(g, this_proj))
  {
    for (i=0; i<2; i++)
    {
      if (widg != this_proj -> modelgl -> ogl_geom[i][g][j])
      {
        if (GTK_IS_WIDGET(this_proj -> modelgl -> ogl_geom[i][g][j]))
        {
          if (check_menu_item_get_active ((gpointer)this_proj -> modelgl -> ogl_geom[i][g][j]) != show)
          {
            check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_geom[i][g][j], show);
          }
        }
      }
    }
  }
#endif
  this_proj -> modelgl -> anim -> last -> img -> show_coord[g][j] = show;
  for (i=0; i<this_proj -> steps; i++)
  {
    for (j=0; j<this_proj -> natomes; j++)
    {
      if (g < 2)
      {
        if (this_proj -> atoms[i][j].sp == s && this_proj -> atoms[i][j].coord[g] == c)
        {
          this_proj -> atoms[i][j].show[0] = this_proj -> atoms[i][j].show[1] = show;
        }
      }
      else if (g < 4)
      {
        if (this_proj -> atoms[i][j].coord[g] == c)
        {
          this_proj -> atoms[i][j].show[0] = this_proj -> atoms[i][j].show[1] = show;
        }
      }
      else if (g < 9)
      {
        k = this_proj -> coord -> geolist[g][0][c] - 1;
        if (this_proj -> atoms[i][j].rings[s][k] != NULL)
        {
          if (this_proj -> atoms[i][j].rings[s][k][0])
          {
            this_proj -> atoms[i][j].show[0] = this_proj -> atoms[i][j].show[1] = show;
          }
        }
      }
      else
      {
        k = this_proj -> coord -> geolist[g][0][c] - 1;
        if (this_proj -> atoms[i][j].chain[k] != NULL)
        {
          if (this_proj -> atoms[i][j].chain[k][0])
          {
            this_proj -> atoms[i][j].show[0] = this_proj -> atoms[i][j].show[1] = show;
          }
        }
      }
    }
  }
  init_default_shaders (this_proj -> modelgl);
#ifdef GTK4
  if (action)
  {
    g_action_change_state (G_ACTION (action), g_variant_new_boolean (show));
    g_variant_unref (state);
  }
#endif
}

#ifdef GTK3
void detach_frag_mol_menu (glwin * view, int id, int jd)
{
  GtkWidget * widg;
  if (view -> oglmc[id][jd] != NULL)
  {
    if (view -> oglmc[id][jd][0] != NULL)
    {
      if (GTK_IS_WIDGET(view -> oglmc[id][jd][0]))
      {
        widg =  gtk_menu_get_attach_widget (GTK_MENU(view -> oglmc[id][jd][0]));
        if (GTK_IS_WIDGET(widg))
        {
          g_object_ref (view -> oglmc[id][jd][0]);
          gtk_menu_detach (GTK_MENU(view -> oglmc[id][jd][0]));
        }
      }
    }
  }
  if (view -> oglmv[id][jd] != NULL)
  {
    if (view -> oglmv[id][jd][0] != NULL)
    {
      if (GTK_IS_WIDGET(view -> oglmv[id][jd][0]))
      {
        widg =  gtk_menu_get_attach_widget (GTK_MENU(view -> oglmv[id][jd][0]));
        if (GTK_IS_WIDGET(widg))
        {
          g_object_ref (view -> oglmv[id][jd][0]);
          gtk_menu_detach (GTK_MENU(view -> oglmv[id][jd][0]));
        }
      }
    }
  }
}

GtkWidget * add_menu_coord (glwin * view, int id, int jd)
{
  int i, j;
  GtkWidget * widg;
  GtkWidget * menucts;
  struct project * this_proj = get_project_by_id (view -> proj);
  GtkWidget * menuct = gtk_menu_new ();
  if (is_coord_in_menu(jd, this_proj))
  {
    if (jd == 2 || jd == 3) detach_frag_mol_menu(view, id, jd);
    GtkWidget * colt = create_menu_item (TRUE, "_Show/Hide");
    add_menu_child (menuct, colt);
    if (jd < 2 || jd > 3)
    {
      menucts = gtk_menu_new ();
      menu_item_set_submenu (colt, menucts);
      if (jd > 3)
      {
        if (jd == 9)
        {
          colt = create_menu_item (FALSE, "Atoms in chain(s) of size");
        }
        else
        {
          colt = create_menu_item (FALSE, "Atoms in ring(s) of size");
        }
        add_menu_child (menucts, colt);
        menucts = gtk_menu_new ();
        menu_item_set_submenu (colt, menucts);
      }
      j = (jd < 2) ? this_proj -> nspec : this_proj -> coord -> totcoord[jd];
      for (i=0; i<j; i++)
      {
        if (jd < 2)
        {
          if (view -> oglmv[id][jd])
          {
            if (GTK_IS_WIDGET(view -> oglmv[id][jd][i]))
            {
              widg = gtk_widget_get_parent (view -> oglmv[id][jd][i]);
              if (GTK_IS_WIDGET(widg))
              {
                g_object_ref (view -> oglmv[id][jd][i]);
                gtk_container_remove (GTK_CONTAINER(widg), view -> oglmv[id][jd][i]);
              }
              add_menu_child (menucts, view -> oglmv[id][jd][i]);
            }
          }
        }
        else
        {
          if (view -> ogl_geom[id][jd])
          {
            if (GTK_IS_WIDGET(view -> ogl_geom[id][jd][i]))
            {
              widg =  gtk_widget_get_parent (view -> ogl_geom[id][jd][i]);
              if (GTK_IS_WIDGET(widg))
              {
                g_object_ref (view -> ogl_geom[id][jd][i]);
                gtk_container_remove (GTK_CONTAINER(widg), view -> ogl_geom[id][jd][i]);
              }
              add_menu_child (menucts, view -> ogl_geom[id][jd][i]);
            }
          }
        }
      }
    }
    else if ((jd == 2 && view -> adv_bonding[0]) || (jd == 3 && view -> adv_bonding[1]))
    {
      if (view -> oglmv[id][jd])
      {
        menu_item_set_submenu (colt, view -> oglmv[id][jd][0]);
      }
    }
    if (jd != 9)
    {
      colt = create_menu_item (FALSE, "Color(s)");
      add_menu_child (menuct, colt);
    }
    if (jd < 2)
    {
      menucts = gtk_menu_new ();
      menu_item_set_submenu (colt, menucts);
      if (view -> oglmc[id][jd])
      {
        for (i=0; i<this_proj -> nspec; i++)
        {
          if (GTK_IS_WIDGET(view -> oglmc[id][jd][i]))
          {
            widg = gtk_widget_get_parent (view -> oglmc[id][jd][i]);
            if (GTK_IS_WIDGET(widg))
            {
              g_object_ref (view -> oglmc[id][jd][i]);
              gtk_container_remove (GTK_CONTAINER(widg), view -> oglmc[id][jd][i]);
            }
            add_menu_child (menucts, view -> oglmc[id][jd][i]);
          }
        }
      }
    }
    else if ((jd == 2 && view -> adv_bonding[0]) || (jd == 3 && view -> adv_bonding[1]))
    {
      if (view -> oglmc[id][jd])
      {
        if (GTK_IS_WIDGET(view -> oglmc[id][jd][0]))
        {
          menu_item_set_submenu (colt, view -> oglmc[id][jd][0]);
        }
      }
    }
    else if ((jd > 3 && jd < 9) && view -> rings)
    {
      if (view -> oglmc[id][jd])
      {
        if (GTK_IS_WIDGET(view -> oglmc[id][jd][0]))
        {
          widg =  gtk_menu_get_attach_widget (GTK_MENU(view -> oglmc[id][jd][0]));
          if (GTK_IS_WIDGET(widg))
          {
            g_object_ref (view -> oglmc[id][jd][0]);
            gtk_menu_detach (GTK_MENU(view -> oglmc[id][jd][0]));
          }
          menu_item_set_submenu (colt, view -> oglmc[id][jd][0]);
        }
      }
    }
  }
  add_advanced_item (menuct, G_CALLBACK(coord_properties), (gpointer)& view -> colorp[jd][1], FALSE, 0, 0);
  return menuct;
}

GtkWidget * menu_coord (glwin * view, int id)
{
  GtkWidget * menuco = create_menu_item (FALSE, "Coordination");
  GtkWidget * menuc = gtk_menu_new ();
  menu_item_set_submenu (menuco, menuc);

  add_menu_child (menuc, menu_item_new_with_submenu ("Total(s)", TRUE, add_menu_coord(view, id, 0)));
  add_menu_child (menuc, menu_item_new_with_submenu ("Partial(s)", TRUE, add_menu_coord(view, id, 1)));
  return menuco;
}

GtkWidget * menu_rings (glwin * view, int id, int jd)
{
  GtkWidget * menuco = gtk_menu_new ();
  if (view -> rings)
  {
    int i;
    for (i=0; i<5; i++)
    {
      if (view -> ring_max[i])
      {
        if (id == 0)
        {
          view -> ogl_rings[i+1] = menu_item_new_with_submenu (rings_type[i], TRUE, add_menu_coord (view, id, 4+i));
          add_menu_child (menuco, view -> ogl_rings[i+1]);
        }
        else
        {
          add_menu_child (menuco, menu_item_new_with_submenu (rings_type[i], TRUE, add_menu_coord (view, id, 4+i)));
        }
      }
    }
  }
  return menuco;
}
#else
GMenu * color_item (glwin * view, gchar * act, int id, GCallback handler, gpointer data)
{
  GMenu *  menu = g_menu_new ();
  append_opengl_item (view, menu, act, act, id, NULL, IMG_NONE, NULL, TRUE, NULL, NULL, FALSE, FALSE, FALSE, FALSE);
  append_opengl_item (view, menu, "More colors ...", act, id, NULL, IMG_NONE, NULL, FALSE, handler, data, FALSE, FALSE, FALSE, TRUE);
  return menu;
}

GMenu * menu_show_coord (glwin * view, int id, int mid)
{
  GMenu * menu = g_menu_new ();
  GMenu * menus;
  struct project * this_proj = get_project_by_id (view -> proj);
  gchar * stra,  * strb;
  int i, j, k;
  for (i=0; i<this_proj -> nspec; i++)
  {
    menus = g_menu_new ();
    if (this_proj -> coord)
    {
      if (this_proj -> coord -> ntg[id])
      {
        j = 0;
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
          if (! mid)
          {
            strb = g_strdup_printf ("%s-s", stra);
            append_opengl_item (view, menus, stra, strb, k+j, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(show_hide_coord), & view -> gcid[id][k+j][id],
                                TRUE, view -> anim -> last -> img -> show_coord[id][k+j], FALSE, TRUE);
          }
          else
          {
            strb = g_strdup_printf ("%s-c", stra);
            g_menu_append_submenu (menus, stra, (GMenuModel*)color_item(view, strb, k+j, G_CALLBACK(window_color_coord), & view -> gcid[id][k+j][id]));
          }
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

G_MODULE_EXPORT void to_coord_properties (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  coord_properties (NULL, data);
}

GMenu * menu_show_frag_mol (glwin * view, int id, int mid)
{
  GMenu * menu = g_menu_new ();
  struct project * this_proj = get_project_by_id (view -> proj);
  if (active_coord -> totcoord[id] <= COORD_MAX_MENU)
  {
    gchar * stra, * strb;
    int i;
    for (i=0; i < this_proj -> coord -> totcoord[id]; i++)
    {
      stra = g_strdup_printf ("%s N°%d", (id == 2) ? "Fragment" : "Molecule", i+1);
      strb = g_strdup_printf ("%s-%d", (id == 2) ? "frag" : "mol", i+1);
      if (! mid)
      {
        append_opengl_item (view, menu, stra, strb, i, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(show_hide_coord), & view -> gcid[id][i][id],
                            TRUE, view -> anim -> last -> img -> show_coord[id][i], FALSE, TRUE);
      }
      else
      {
        g_menu_append_submenu (menu, stra, (GMenuModel*)color_item(view, (id == 2) ? "fcol": "mcol", i, G_CALLBACK(window_color_coord), & view -> gcid[id][i][id]));
      }
      g_free (stra);
      g_free (strb);
    }
  }
  // append_opengl_item (view, menu, "All", "all-fm", id, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(to_coord_properties), & view -> colorp[id][1], FALSE, FALSE, FALSE, TRUE);
  return menu;
}

GMenu * menu_show_rings (glwin * view, int id, int mid)
{
  GMenu * menu = g_menu_new ();
  GMenu * menus;
  if (! mid)
  {
    menus = g_menu_new ();
    g_menu_append_submenu (menu, (id < 9) ? "Atoms In Ring(s) of Size " : "Atoms In Chain(s) of Size ", (GMenuModel*)menus);
  }
  struct project * this_proj = get_project_by_id (view -> proj);
  gchar * rin = g_strdup_printf ("rcol-%d", id);
  gchar * str;
  int i;
  for (i=0; i < this_proj -> coord -> totcoord[id]; i++)
  {
    str = g_strdup_printf ("%d", this_proj -> coord -> geolist[id][0][i]);
    if (! mid)
    {
      append_opengl_item (view, menus, str, str, i, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(show_hide_coord), & view -> gcid[id][i][id],
                          TRUE, view -> anim -> last -> img -> show_coord[id][i], FALSE, TRUE);
    }
    else
    {
      g_menu_append_submenu (menu, str, (GMenuModel*)color_item(view, rin, i, G_CALLBACK(window_color_coord), & view -> gcid[id][i][id]));
    }
    g_free (str);
  }
  g_free (rin);
  if (! mid) g_object_unref (menus);
  return menu;
}

GMenu * add_menu_coord (glwin * view, int id)
{
  GMenu * menu = g_menu_new ();
  struct project * this_proj = get_project_by_id (view -> proj);
  gchar * menu_name[2] = {"Show/Hide", "Color"};
  if (is_coord_in_menu(id, this_proj))
  {
    int i;
    for (i=0; i<2; i++)
    {
      if (id < 2)
      {
        g_menu_append_submenu (menu, menu_name[i], (GMenuModel*) menu_show_coord(view, id, i));
      }
      else if (id < 4)
      {
        g_menu_append_submenu (menu, menu_name[i], (GMenuModel*) menu_show_frag_mol(view, id, i));
      }
      else if (id < 10)
      {
        g_menu_append_submenu (menu, menu_name[i], (GMenuModel*) menu_show_rings(view, id, i));
      }
    }
    append_opengl_item (view, menu, "Advanced", "adv-c", id, NULL, IMG_STOCK, (gpointer)DPROPERTIES, FALSE,
                        G_CALLBACK(to_coord_properties), & view -> colorp[id][1], FALSE, FALSE, FALSE, TRUE);
  }
  return menu;
}

GMenu * menu_coord (glwin * view, int id)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Total(s)", (GMenuModel*)add_menu_coord(view, 0));
  g_menu_append_submenu (menu, "Partial(s)", (GMenuModel*)add_menu_coord(view, 1));
  return menu;
}

GMenu * menu_rings (glwin * view, int id)
{
  GMenu * menu = g_menu_new ();
  if (view -> rings)
  {
    int i;
    for (i=0; i<5; i++)
    {
      if (view -> ring_max[i])
      {
        g_menu_append_submenu (menu, rings_type[i], (GMenuModel*)add_menu_coord (view, 4+i));
      }
    }
  }
  return menu;
}
#endif
