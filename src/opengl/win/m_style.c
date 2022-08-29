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
#include "bind.h"
#include "project.h"
#include "glview.h"
#include "glwindow.h"

extern gchar * label_atpts (struct project * this_proj, glwin * view, int id);

void clean_atom_style (struct project * this_proj)
{
  int i, j;
  for (i=0; i<this_proj -> steps; i++)
  {
    for (j=0; j<this_proj -> natomes; j++)
    {
      this_proj -> atoms[i][j].style = NONE;
    }
  }
}

#ifdef GTK3
// GTK3 Menu Action To Check
void update_menus (glwin * view)
{
  int i, j;
  int s = view -> anim -> last -> img -> style;
  gchar * str;
  for (i=0; i<2; i++)
  {
    for (j=0; j<6; j+=2)
    {
      gtk_widget_hide (view -> ogl_bonds[j+8*i]);
      if (j<4) gtk_widget_hide (view -> ogl_atoms[4*i+j]);
    }
  }
  switch (s)
  {
    case CYLINDERS:
      for (i=0; i<2; i++) gtk_widget_show (view -> ogl_bonds[8*i]);
      break;
    case WIREFRAME:
      for (i=0; i<2; i++)
      {
        gtk_widget_show (view -> ogl_bonds[4+8*i]);
        str = label_atpts (get_project_by_id(view -> proj), view, 1+2*i);
        gtk_menu_item_set_label (GTK_MENU_ITEM(view -> ogl_atoms[4*i+1]), str);
        g_free (str);
        gtk_widget_show (view -> ogl_atoms[4*i+2]);
      }
      break;
    case SPACEFILL:
      for (i=0; i<2; i++)
      {
        str = label_atpts (get_project_by_id(view -> proj), view, 4);
        gtk_menu_item_set_label (GTK_MENU_ITEM(view -> ogl_atoms[4*i+1]), str);
        g_free (str);
        gtk_widget_show (view -> ogl_atoms[4*i]);
      }
      break;
    case PUNT:
      for (i=0; i<2; i++)
      {
        str = label_atpts (get_project_by_id(view -> proj), view, 2*i+1);
        gtk_menu_item_set_label (GTK_MENU_ITEM(view -> ogl_atoms[4*i+1]), str);
        g_free (str);
        gtk_widget_show (view -> ogl_atoms[4*i+2]);
      }
      break;
    case SPHERES:
      for (i=0; i<2; i++)
      {
        str = label_atpts (get_project_by_id(view -> proj), view, 2*i);
        gtk_menu_item_set_label (GTK_MENU_ITEM(view -> ogl_atoms[4*i+1]), str);
        g_free (str);
        gtk_widget_show (view -> ogl_atoms[4*i]);
      }
      break;
    default:
      for (i=0; i<2; i++)
      {
        gtk_widget_show (view -> ogl_bonds[2+8*i]);
        str = label_atpts (get_project_by_id(view -> proj), view, 2*i);
        gtk_menu_item_set_label (GTK_MENU_ITEM(view -> ogl_atoms[4*i+1]), str);
        g_free (str);
        gtk_widget_show (view -> ogl_atoms[4*i]);
      }
      break;
  }
}
#endif

G_MODULE_EXPORT void set_style (GtkWidget * widg, gpointer data)
{
  tint * the_data = (tint *)data;
  struct project * this_proj = get_project_by_id(the_data -> a);
  int s = the_data -> b;
  int st = (s >= OGL_STYLES) ? SPACEFILL : s;
  int ft = (s >= OGL_STYLES) ? s - OGL_STYLES * (s/OGL_STYLES) : NONE;
  int old_style = this_proj -> modelgl -> anim -> last -> img -> style;
  int old_filled = this_proj -> modelgl -> anim -> last -> img -> filled_type;
  int i, j, k;
#ifdef GTK3
  if ((old_style != st || old_filled != ft) && check_menu_item_get_active ((gpointer)widg))
#else
  if (old_style != st || old_filled != ft)
#endif
  {
    i = old_style;
    j = old_filled;
    this_proj -> modelgl -> anim -> last -> img -> style = NONE;
    this_proj -> modelgl -> anim -> last -> img -> filled_type = NONE;
#ifdef GTK3
    if (old_style == SPACEFILL)
    {
      check_menu_item_set_active ((gpointer)this_proj -> modelgl -> filled_styles[j], FALSE);
    }
    else
    {
      check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_styles[i], FALSE);
    }
#endif
    if (s >= OGL_STYLES)
    {
      j = this_proj -> nspec;
      for (i=0; i<j; i++)
      {
        k = (int)this_proj -> chemistry -> chem_prop[CHEM_Z][i];
        this_proj -> modelgl -> anim -> last -> img -> atomicrad[i] = this_proj -> modelgl -> anim -> last -> img -> atomicrad[i+j] = set_radius_ (& k, & ft);
      }
#ifdef GTK3
      if (widg != this_proj -> modelgl -> filled_styles[ft])
      {
        check_menu_item_set_active ((gpointer)this_proj -> modelgl -> filled_styles[ft], TRUE);
      }
#endif
      this_proj -> modelgl -> anim -> last -> img -> filled_type = ft;
      this_proj -> modelgl -> anim -> last -> img -> style = SPACEFILL;
    }
    else
    {
#ifdef GTK3
      if (widg != this_proj -> modelgl -> ogl_styles[st])
      {
        check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_styles[st], TRUE);
      }
#endif
      this_proj -> modelgl -> anim -> last -> img -> style = st;
    }
    clean_atom_style (this_proj);
    if (st == WIREFRAME || st == PUNT)
    {
      j = 0;
    }
    else
    {
      j = 1;
    }
#ifdef GTK3
    for (i=0; i<OGL_RENDERS; i++)
    {
      widget_set_sensitive (this_proj -> modelgl -> ogl_render[i], j);
    }
    update_menus (this_proj -> modelgl);
#else

#endif
    init_default_shaders (this_proj -> modelgl);
  }
  /*else if (st != SPACEFILL && old_style != NONE && ! check_menu_item_get_active ((gpointer)widg))
  {
    check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_styles[st], TRUE);
  }
  else if (old_style != NONE && old_filled != NONE && ! check_menu_item_get_active ((gpointer)widg))
  {
    check_menu_item_set_active ((gpointer)this_proj -> modelgl -> filled_styles[ft], TRUE);
  }*/
}

#ifdef GTK3
GtkWidget * create_style_menu (char * name, int val, int style, int vbl, int filled, GtkWidget * menu, tint * data)
{
  GtkWidget * style_widget = gtk3_menu_item (menu, name, IMG_NONE, NULL, G_CALLBACK(set_style), data, FALSE, 0, 0, TRUE, TRUE, (style == val && filled == vbl) ? TRUE : FALSE);
  return style_widget;
}

GtkWidget * menu_style (glwin * view, int id)
{
  int i, j;
  GtkWidget * widg;
  GtkWidget * menus = gtk_menu_new ();
  if (id == 0)
  {
    for (i=0; i<OGL_STYLES; i++)
    {
      if (i != SPACEFILL)
      {
        view -> ogl_styles[i] = create_style_menu (text_styles[i],
                                                   i,
                                                   view -> anim -> last -> img -> style,
                                                   i,
                                                   i,
                                                   menus,
                                                   & view -> colorp[i][0]);
      }
      else
      {
        widg = create_menu_item (FALSE, "Spacefilled");
        add_menu_child (menus, widg);
        GtkWidget * menuf = gtk_menu_new ();
        menu_item_set_submenu (widg, menuf);
        for (j=0; j < FILLED_STYLES; j++)
        {
           view -> filled_styles[j] = create_style_menu (text_filled[j],
                                                         SPACEFILL,
                                                         view -> anim -> last -> img -> style,
                                                         j,
                                                         view -> anim -> last -> img -> filled_type,
                                                         menuf,
                                                         & view -> colorp[OGL_STYLES + j][0]);
        }
      }
    }
  }
  else
  {
    for (i=0; i<OGL_STYLES; i++)
    {
      if (i != SPACEFILL)
      {
        widg = create_style_menu (text_styles[i],
               i,
               view -> anim -> last -> img -> style,
               i,
               i,
               menus,
               & view -> colorp[i][0]);
      }
      else
      {
        widg = create_menu_item (FALSE, "Spacefilled");
        add_menu_child (menus, widg);
        GtkWidget * menuf = gtk_menu_new ();
        menu_item_set_submenu (widg, menuf);
        for (j=0; j < FILLED_STYLES; j++)
        {
           widg = create_style_menu (text_filled[j],
                                     SPACEFILL,
                                     view -> anim -> last -> img -> style,
                                     j,
                                     view -> anim -> last -> img -> filled_type,
                                     menuf,
                                     & view -> colorp[OGL_STYLES + j][0]);
        }

      }
    }
  }
  return menus;
}
#else
G_MODULE_EXPORT void change_style_radio (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  glwin * view = (glwin *)data;
  const gchar * style = g_variant_get_string (parameter, NULL);
  gchar * style_name = NULL;
  int i, j;
  for (i=0; i<OGL_STYLES; i++)
  {
    if (i != SPACEFILL)
    {
      style_name = g_strdup_printf ("set-style.%d", i);
      if (g_strcmp0(style, (const gchar *)style_name) == 0)
      {
        set_style (NULL, & view -> colorp[i][0]);
        break;
      }
      g_free (style_name);
      style_name = NULL;
    }
    else
    {
      for (j=0; j < FILLED_STYLES; j++)
      {
        style_name = g_strdup_printf ("set-style.%d", OGL_STYLES+j);
        if (g_strcmp0(style, (const gchar *)style_name) == 0)
        {
          set_style (NULL, & view -> colorp[OGL_STYLES+j][0]);
          break;
        }
        g_free (style_name);
        style_name = NULL;
      }
    }
    if (style_name)
    {
      g_free (style_name);
      break;
    }
  }
  g_action_change_state (G_ACTION (action), parameter);
}

GMenu * menu_style (glwin * view)
{
  int i, j;
  GMenu * menu = g_menu_new ();
  for (i=0; i<OGL_STYLES; i++)
  {
    if (i != SPACEFILL)
    {
      append_opengl_item (view, menu, text_styles[i], "style", i,
                          NULL, IMG_NONE, NULL,
                          FALSE, G_CALLBACK(change_style_radio), (gpointer)view,
                          FALSE, (view -> anim -> last -> img -> style == i) ? TRUE : FALSE, TRUE, TRUE);
    }
    else
    {
      GMenu * menus = g_menu_new ();
      for (j=0; j < FILLED_STYLES; j++)
      {
        append_opengl_item (view, menus, text_filled[j], "style", OGL_STYLES+j,
                            NULL, IMG_NONE, NULL,
                            FALSE, G_CALLBACK(change_style_radio), (gpointer)view,
                            FALSE, (view -> anim -> last -> img -> style == SPACEFILL && view -> anim -> last -> img -> filled_type == j) ? TRUE : FALSE, TRUE, TRUE);
      }
      g_menu_append_submenu (menu, "Spacefilled", (GMenuModel*)menus);
    }
  }
  return menu;
}
#endif
