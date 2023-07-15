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
#include "project.h"
#include "calc.h"
#include "glview.h"
#include "initcoord.h"
#include "submenus.h"
#include "color_box.h"

#ifdef GTK4
extern GtkWidget * color_palette (glwin * view, int ideo, int spec, int geo);
extern G_MODULE_EXPORT void opengl_advanced (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void render_gl_image (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void to_coord_properties (GSimpleAction * action, GVariant * parameter, gpointer data);
GSimpleActionGroup * view_pop_actions;

G_MODULE_EXPORT void to_opengl_advanced (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  opengl_advanced (NULL, data);
}

G_MODULE_EXPORT void to_render_gl_image (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  render_gl_image (NULL, data);
}

void append_opengl_item (glwin * view, GMenu * menu, const gchar * name, const gchar * key, int item_id,
                         gchar * accel, int image_format, gpointer icon,
                         gboolean custom, GCallback handler, gpointer data,
                         gboolean check, gboolean status, gboolean radio, gboolean sensitive)
{
  gchar * str_a, * str_b, * str_c;
  str_a = g_strdup_printf ("set-%s", key);
  str_b = g_strdup_printf ("%s.%d", str_a, item_id);
  str_c = (sensitive) ? g_strdup_printf ("gl-%d.%s", view -> action_id, (radio) ? str_a : str_b) : g_strdup_printf ("None");
  append_menu_item (menu, name, (const gchar *) str_c, accel, (custom) ? (const gchar *) str_b : NULL, image_format, icon, check, status, radio, (radio) ? (const gchar *)str_b : NULL);
  if (handler)
  {
    if (! radio || (radio && status))
    {
      if (! opengl_project)
      {
        widget_add_action (view -> action_group, (radio) ? (const gchar *)str_a : (const gchar *)str_b, handler, data,
                           check, status, radio, (const gchar *)str_b);
      }
      else
      {
        widget_add_action (view_pop_actions, (radio) ? (const gchar *)str_a : (const gchar *)str_b, handler, data,
                           check, status, radio, (const gchar *)str_b);
      }
    }
  }
  g_free (str_a);
  g_free (str_b);
  g_free (str_c);
}

GMenu * prepare_opengl_menu (glwin * view)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Style", (GMenuModel*)menu_style(view));
  GMenuItem * item = g_menu_item_new ("Color Scheme(s)", (get_project_by_id(view -> proj) -> nspec) ? NULL : "None");
  g_menu_item_set_submenu (item, (GMenuModel*)menu_map(view));
  g_menu_append_item (menu, item);
  g_menu_append_submenu (menu, "Render", (GMenuModel*)menu_render(view));
  g_menu_append_submenu (menu, "Quality", (GMenuModel*)menu_quality(view));
  append_opengl_item (view, menu, "Material And Lights", "material", 0, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(to_opengl_advanced), (gpointer)view, FALSE, FALSE, FALSE, TRUE);
  append_opengl_item (view, menu, "Render Image", "image", 0, "<CTRL>I", IMG_FILE, PACKAGE_IMG, FALSE, G_CALLBACK(to_render_gl_image), (gpointer)view, FALSE, FALSE, FALSE, TRUE);
  return menu;
}

GMenu * prepare_model_menu (glwin * view)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Atom(s)", (GMenuModel*)menu_atoms(view, 0));
  g_menu_append_submenu (menu, "Bond(s)", (GMenuModel*)menu_bonds(view, 0));
  g_menu_append_submenu (menu, "Clone(s)", (GMenuModel*)menu_clones(view));
  g_menu_append_item (menu, menu_box_axis (view, 0));
  return menu;
}

GMenu * prepare_coord_menu (glwin * view)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Coordination", (GMenuModel*)menu_coord (view));
  g_menu_append_submenu (menu, "Polyhedra", (GMenuModel*)menu_poly (view));
  if (view -> rings)
  {
    g_menu_append_submenu (menu, "Rings(s)", (GMenuModel*)menu_rings (view));
  }
  else
  {
    append_menu_item (menu, "Ring(s)", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  }
  if (view -> chains)
  {
    g_menu_append_submenu (menu, "Chain(s)", (GMenuModel*)add_menu_coord (view, 9));
  }
  else
  {
    append_menu_item (menu, "Chain(s)", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  }
  if (view -> adv_bonding[0])
  {
    g_menu_append_submenu (menu, "Fragment(s)", (GMenuModel*)add_menu_coord (view, 2));
  }
  else
  {
    append_menu_item (menu, "Fragment(s)", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  }
  if (view -> adv_bonding[1])
  {
    g_menu_append_submenu (menu, "Molecule(s)", (GMenuModel*)add_menu_coord (view, 3));
  }
  else
  {
    append_menu_item (menu, "Molecule(s)", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  }
  append_opengl_item (view, menu, "Advanced", "adv-all", 0, "<CTRL>E", IMG_STOCK, (gpointer)DPROPERTIES, FALSE, G_CALLBACK(to_coord_properties), & view -> colorp[30][0], FALSE, FALSE, FALSE, TRUE);
  return menu;
}

GMenu * opengl_menu_bar (glwin * view, gchar * str)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "OpenGL", (GMenuModel*)prepare_opengl_menu(view));
  if (get_project_by_id(view -> proj) -> natomes)
  {
    g_menu_append_submenu (menu, "Model", (GMenuModel*)prepare_model_menu(view));
    g_menu_append_submenu (menu, "Chemistry", (GMenuModel*)prepare_coord_menu(view));
  }
  else
  {
    append_menu_item (menu, "Model", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
    append_menu_item (menu, "Chemistry", "None", NULL, NULL, IMG_NONE, NULL, FALSE, FALSE, FALSE, NULL);
  }
  g_menu_append_submenu (menu, "Tools", (GMenuModel*)menu_tools(view, 0));
  g_menu_append_submenu (menu, "View", (GMenuModel*)menu_view(view, 0));
  g_menu_append_submenu (menu, "Animate", (GMenuModel*)menu_anim(view));
  return menu;
}

void menu_bar_attach_color_palettes (glwin * view, GtkWidget * menu_bar)
{
  /* Here we need to attached color palettes for:
    - Box
    - Atoms
    - Clones
    - Total coordination(s)
    - Partial coordination(s)
    - Fragment(s) and molecule(s)
    - Ring(s)
    - Background
  This is a work in progress since the 'gtk_popover_menu_bar_add_child' to use to insert widget is bugged:
  https://gitlab.gnome.org/GNOME/gtk/-/issues/5955
  */
  int i, j, k, l, m;
  gchar * str;
  struct project * this_proj = get_project_by_id (view -> proj);
  // Box
  if (! gtk_popover_menu_bar_add_child ((GtkPopoverMenuBar *)menu_bar, color_palette (view, -1, -1, -1), "set-box-color.0"))
  {
    g_debug ("Color palette error: box - custom= set-box-color.0");
  }
  // Atom(s) and clone(s)
  for (i=0; i<2; i++)
  {
    for (j=0; j<this_proj -> nspec; j++)
    {
      str = g_strdup_printf ("set-%s.%d", (! i) ? "atom-color" : "clone-color", j);
      if (! gtk_popover_menu_bar_add_child ((GtkPopoverMenuBar *)menu_bar, color_palette (view, i*this_proj -> nspec+j, -1, -1), (const gchar *)str))
      {
        g_debug ("Color palette error: %s - %d - custom= %s", (! i) ? "atom-color" : "clone-color", j+1, str);
      }
      g_free (str);
    }
  }
  // Coordinations
  for (i=0; i<2; i++)
  {
    if (this_proj -> coord -> ntg[i])
    {
      for (j=0; j<this_proj -> nspec; j++)
      {
        for (k=0; k<this_proj -> coord -> ntg[i][j]; k++)
        {
          m = 0;
          for (l=0; l<j; l++)
          {
            m += this_proj -> coord -> ntg[i][l];
          }
          if (i)
          {
            str = g_strdup_printf ("set-%s-c.%d", exact_name (env_name (this_proj, k, j, 1, NULL)), m);
          }
          else
          {
            str = g_strdup_printf ("set-%d-c.%d", this_proj -> coord -> geolist[i][j][k], m);
          }
          m += (i) ? this_proj -> coord -> totcoord[0] : 0;
          if (! gtk_popover_menu_bar_add_child ((GtkPopoverMenuBar *)menu_bar, color_palette (view, 2*this_proj -> nspec+m, -1, -1), (const gchar *)str))
          {
            g_debug ("Color palette error: %s - spec= %d - coord= %d, custom= %s", (! i) ? "total-coord" : "partial-coord", j+1, k+1, str);
          }
          g_free (str);
        }
      }
    }
  }
  // Fragment(s) and molecule(s)
  for (i=2; i<4; i++)
  {
    for (j=0; j<this_proj -> coord -> totcoord[i]; j++)
    {
      str = g_strdup_printf ("set-%s-%d", (i == 2) ? "fcol" : "mcol", j);
      k = 2*this_proj -> nspec + this_proj -> coord -> totcoord[0] + this_proj -> coord -> totcoord[1] + j;
      if (i == 3) k += this_proj -> coord -> totcoord[2];
      if (! gtk_popover_menu_bar_add_child ((GtkPopoverMenuBar *)menu_bar, color_palette (view, k, i, 0), (const gchar *)str))
      {
        g_debug ("Color palette error: %s - %d, custom= %s", (i == 2) ? "fragment" : "molecule", j+1, str);
      }
      g_free (str);
    }
  }
  // Rings
  for (i=4; i<9; i++)
  {
    for (j=0; j<this_proj -> coord -> totcoord[i]; j++)
    {
      str = g_strdup_printf ("set-rcol-%d-%d", i, j);
      if (! gtk_popover_menu_bar_add_child ((GtkPopoverMenuBar *)menu_bar, color_palette (view, -3, i-4, 0), (const gchar *)str))
      {
        g_debug ("Color palette error: rings - %d - %d, custom= %s", i, j+1, str);
      }
      g_free (str);
    }
  }
  // Background
  if (! gtk_popover_menu_bar_add_child ((GtkPopoverMenuBar *)menu_bar, color_palette (view, -2, -1, -1), "set-back-color.0"))
  {
    g_debug ("Color palette error: background - custom= set-back-color.0");
  }
}

GtkWidget * opengl_window_create_menu_bar (glwin * view)
{
  view -> menu_bar = destroy_this_widget (view -> menu_bar);
  gchar * str = g_strdup_printf ("gl-%d", view -> action_id);
  if (view -> action_group) g_object_unref (view -> action_group);
  view -> action_group = g_simple_action_group_new ();
  opengl_project = NULL;
  GtkWidget * menu_bar = gtk_popover_menu_bar_new_from_model ((GMenuModel *)opengl_menu_bar(view, str));

  menu_bar_attach_color_palettes (view, menu_bar);

  opengl_project_changed (activev);
  gtk_widget_insert_action_group (menu_bar, str, G_ACTION_GROUP(view -> action_group));
  g_free (str);


  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, view -> menu_box, menu_bar, FALSE, FALSE, 0);

  show_the_widgets (menu_bar);
  return menu_bar;
}

void update_menu_bar (glwin * view)
{
  view -> menu_bar = opengl_window_create_menu_bar (view);
}
#endif
