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

#ifdef GTK4
extern G_MODULE_EXPORT void window_bonds (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void window_cuts (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void set_bond_parameter (GSimpleAction * action, GVariant * parameter, gpointer data);
#else
extern G_MODULE_EXPORT void window_bonds (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void window_cuts (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void set_bond_parameter (GtkWidget * widg, gpointer data);
#endif

gchar * label_cutrab (struct project * this_proj, glwin * view, int id)
{
  int i, j, k, m;
  gchar * mot;
  gchar * tmp;
  gchar * str;
  chemical_data * chem = this_proj -> chemistry;

  k = 0;
  m = this_proj -> nspec;
  for (i=0; i < m; i++)
  {
    if (id == 0)
    {
      str = g_strdup_printf ("%s-%s [ %f Å ]", chem -> label[i], chem -> label[i], chem -> cutoffs[i][i]);
    }
    else
    {
      if (id == 1)
      {
        str = g_strdup_printf ("%s-%s [ %f Å ]", chem -> label[i], chem -> label[i], view -> anim -> last -> img -> bondrad[i][i]);
      }
      else if (id == 2)
      {
        str = g_strdup_printf ("%s-%s [ %f Å ]", chem -> label[i], chem -> label[i], view -> anim -> last -> img -> bondrad[i+m][i+m]);
      }
      else if (id == 3)
      {
        str = g_strdup_printf ("%s-%s [ %f pts ]", chem -> label[i], chem -> label[i], view -> anim -> last -> img -> linerad[i][i]);
      }
      else
      {
        str = g_strdup_printf ("%s-%s [ %f pts ]", chem -> label[i], chem -> label[i], view -> anim -> last -> img -> linerad[i+m][i+m]);
      }
    }
    if (i == 0)
    {
      mot = g_strdup_printf ("%s", str);
    }
    else
    {
      tmp = g_strdup_printf ("%s", mot);
      g_free (mot);
      mot = g_strdup_printf ("%s\n%s", tmp, str);
      g_free (tmp);
    }
    g_free (str);
    k+=1;
  }
  for (i=0; i < m-1; i++)
  {
    for (j=i+1; j < m; j++)
    {
      if (id == 0)
      {
        str = g_strdup_printf ("%s-%s [ %f Å ]", chem -> label[i], chem -> label[j], chem -> cutoffs[i][j]);
      }
      else
      {
        if (id == 1)
        {
          str = g_strdup_printf ("%s-%s [ %f Å ]", chem -> label[i], chem -> label[j], view -> anim -> last -> img -> bondrad[i][j]);
        }
        else if (id == 2)
        {
          str = g_strdup_printf ("%s-%s [ %f Å ]", chem -> label[i], chem -> label[j], view -> anim -> last -> img -> bondrad[i+m][j+m]);
        }
        else if (id == 3)
        {
          str = g_strdup_printf ("%s-%s [ %f pts ]", chem -> label[i], chem -> label[j], view -> anim -> last -> img -> linerad[i][j]);
        }
        else
        {
          str = g_strdup_printf ("%s-%s [ %f pts ]", chem -> label[i], chem -> label[j], view -> anim -> last -> img -> linerad[i+m][j+m]);
        }
      }

      if (k == 0)
      {
        mot = g_strdup_printf ("%s", str);
      }
      else
      {
        tmp = g_strdup_printf ("%s", mot);
        g_free (mot);
        mot = g_strdup_printf ("%s\n%s", tmp, str);
        g_free (tmp);
      }
      g_free (str);
      k += 1;
    }
  }
  if (id == 0)
  {
    str = g_strdup_printf ("Total [ %f Å ]", chem -> grtotcutoff);
    tmp = g_strdup_printf ("%s", mot);
    g_free (mot);
    mot = g_strdup_printf ("%s\n%s", tmp, str);
    g_free (tmp);
    g_free (str);
  }
  return mot;
}

#ifdef GTK3
GtkWidget * create_bond_layout_menu (gchar * str, GtkWidget * menu)
{
  GtkWidget * layout = create_menu_item (FALSE, str);
  add_menu_child (menu, layout);
  return  layout;
}

GtkWidget * create_bond_layout_widget (gchar * str, GtkWidget * widg, int va, tint * data)
{
  GtkWidget * menu = gtk_menu_new ();
  menu_item_set_submenu (widg, menu);
  GtkWidget * layout = create_menu_item (FALSE, str);
  add_menu_child (menu, layout);
  switch (va)
  {
    case 0:
      g_signal_connect (G_OBJECT (layout), "activate", G_CALLBACK(window_bonds), data);
      break;
    case 1:
      g_signal_connect (G_OBJECT (layout), "activate", G_CALLBACK(set_bond_parameter), data);
      break;
    default:
      g_signal_connect (G_OBJECT (layout), "activate", G_CALLBACK(window_cuts), data);
      break;
  }
  return layout;
}

GtkWidget * menu_bonds (glwin * view, int id, int at)
{
  GtkWidget * widg;
  gchar * str;
  int i;

  GtkWidget * menub = gtk_menu_new ();
  i = view -> anim -> last -> img -> style;

  if (id == 0)
  {
    if (at == 0)
    {
      str = g_strdup_printf ("Cylinder(s) [ %f Å ]", view -> anim -> last -> img -> radall[0]);
    }
    else
    {
      str = g_strdup_printf ("Cylinder(s) [ %f Å ]", view -> anim -> last -> img -> radall[1]);
    }
    view -> ogl_bonds[8*at] = create_bond_layout_menu ("Radius", menub);
    view -> ogl_bonds[1+8*at] = create_bond_layout_widget (str, view -> ogl_bonds[8*at], 0, & view -> colorp[2+at][0]);
    g_free (str);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (view -> ogl_bonds[1+8*at], 0);
    }
  }
  else if (i == CYLINDERS)
  {
    if (at == 0)
    {
      str = g_strdup_printf ("Cylinder(s) [ %f Å ]", view -> anim -> last -> img -> radall[0]);
    }
    else
    {
      str = g_strdup_printf ("Cylinder(s) [ %f Å ]", view -> anim -> last -> img -> radall[1]);
    }
    widg = create_bond_layout_menu ("Cylinder Radius(ii)", menub);
    widg = create_bond_layout_widget (str, widg, 0, & view -> colorp[2+at][0]);
    g_free (str);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (widg, 0);
    }
  }

  if (id == 0)
  {
    str = label_cutrab (get_project_by_id(view -> proj), view, 1+at);
    view -> ogl_bonds[2+8*at] = create_bond_layout_menu ("Radius(ii)", menub);
    view -> ogl_bonds[3+8*at] = create_bond_layout_widget (str, view -> ogl_bonds[2+8*at], 1, & view -> colorp[at][0]);
    g_free (str);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (view -> ogl_bonds[3+8*at], 0);
    }
  }
  else if (i == BALL_AND_STICK)
  {
    str = label_cutrab (get_project_by_id(view -> proj), view, 1+at);
    widg = create_bond_layout_menu ("Radius(ii)", menub);
    widg = create_bond_layout_widget (str, widg, 1, & view -> colorp[at][0]);
    g_free (str);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (widg, 0);
    }
  }

  if (id == 0)
  {
    str = label_cutrab (get_project_by_id(view -> proj), view, 3+at);
    view -> ogl_bonds[4+8*at] = create_bond_layout_menu ("Line Width(s)", menub);
    view -> ogl_bonds[5+8*at] = create_bond_layout_widget (str, view -> ogl_bonds[4+8*at], 1, & view -> colorp[at][1]);
    g_free (str);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (view -> ogl_bonds[5+8*at], 0);
    }
  }
  else if (i == WIREFRAME)
  {
    str = label_cutrab (get_project_by_id(view -> proj), view, 3+at);
    widg = create_bond_layout_menu ("Line Width(s)", menub);;
    widg = create_bond_layout_widget (str, widg, 1, & view -> colorp[at][1]);
    g_free (str);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (widg, 0);
    }
  }

  if (at == 0)
  {
    str = label_cutrab (get_project_by_id(view -> proj), view, 0);
    if (id == 0)
    {
      view -> ogl_bonds[6] = create_bond_layout_menu ("Cutoff(s)", menub);
      view -> ogl_bonds[7] = create_bond_layout_widget (str, view -> ogl_bonds[6], 2, & view -> colorp[at][0]);
    }
    else
    {
      widg = create_bond_layout_menu ("Cutoff(s)", menub);
      widg = create_bond_layout_widget (str, widg, 2, & view -> colorp[at][0]);
    }
    g_free (str);
  }
  return menub;
}
#else
GMenu * create_bond_layout_section (glwin * view, gchar * str, gchar * key, int id, GCallback handler, gpointer data, gboolean sensitive)
{
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, str, key, id, NULL,  IMG_NONE, NULL, FALSE, handler, data, FALSE, FALSE, FALSE, sensitive);
  return  menu;
}

GMenu * menu_bonds (glwin * view, int at)
{
  gchar * str;
  gboolean sensitive = (at == 1 && ! view -> anim -> last -> img -> draw_clones) ? FALSE : TRUE;
  GMenu * menu = g_menu_new ();
  switch (view -> anim -> last -> img -> style)
  {
    case CYLINDERS:
      if (at == 0)
      {
        str = g_strdup_printf ("Cylinder(s) [ %f Å ]", view -> anim -> last -> img -> radall[0]);
      }
      else
      {
        str = g_strdup_printf ("Cylinder(s) [ %f Å ]", view -> anim -> last -> img -> radall[1]);
      }
      g_menu_append_submenu (menu, "Cylinder Radius(ii)", (GMenuModel *)create_bond_layout_section (view, str, (at) ? "clone-cyl-rad" : "atom-cyl-rad", 0, G_CALLBACK(window_bonds), & view -> colorp[2+at][0], sensitive));
      g_free (str);
      break;
    case BALL_AND_STICK:
      str = label_cutrab (get_project_by_id(view -> proj), view, 1+at);
      g_menu_append_submenu (menu, "Radius(ii)", (GMenuModel *)create_bond_layout_section (view, str, (at) ? "clone-rad" : "atom-rad", 1, G_CALLBACK(set_bond_parameter), & view -> colorp[at][0], sensitive));
      g_free (str);
      break;
    case WIREFRAME:
      str = label_cutrab (get_project_by_id(view -> proj), view, 3+at);
      g_menu_append_submenu (menu, "Line Width(s)", (GMenuModel *)create_bond_layout_section (view, str, (at) ? "clone-line" : "atom-line", 2, G_CALLBACK(set_bond_parameter), & view -> colorp[at][1], sensitive));
      g_free (str);
      break;
  }

  if (! at)
  {
    str = label_cutrab (get_project_by_id(view -> proj), view, 0);
    g_menu_append_submenu (menu, "Cutoff(s)", (GMenuModel *)create_bond_layout_section (view, str, "bond-cutoffs", 3, G_CALLBACK(window_cuts), & view -> colorp[at][0], sensitive));
    g_free (str);
  }
  return menu;
}
#endif
