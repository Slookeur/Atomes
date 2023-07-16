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

extern G_MODULE_EXPORT void set_atom_parameter (GtkWidget * widg, gpointer data);

gchar * label_atpts (struct project * this_proj, glwin * view, int id)
{
  int i;
  gchar * mot;
  gchar * tmp;
  gchar * str;

  for (i=0; i < this_proj -> nspec; i++)
  {
    switch (id)
    {
      case 0:
        str = g_strdup_printf ("%s [ %f Å ]", this_proj -> chemistry -> label[i], view -> anim -> last -> img -> sphererad[i]);
        break;
      case 1:
        str = g_strdup_printf ("%s [ %f pts ]", this_proj -> chemistry -> label[i], view -> anim -> last -> img -> pointrad[i]);
        break;
      case 2:
        str = g_strdup_printf ("%s [ %f pts ]", this_proj -> chemistry -> label[i], view -> anim -> last -> img -> sphererad[i+this_proj -> nspec]);
        break;
      case 3:
        str = g_strdup_printf ("%s [ %f pts ]", this_proj -> chemistry -> label[i], view -> anim -> last -> img -> pointrad[i+this_proj -> nspec]);
        break;
      default:
        str = g_strdup_printf ("%s [ %f Å ]", this_proj -> chemistry -> label[i], view -> anim -> last -> img ->  atomicrad[i]);
        break;
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
  }
  return mot;
}

#ifdef GTK3
extern G_MODULE_EXPORT void set_show_hide_all_atom_labels (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void atom_properties (GtkWidget * widg, gpointer data);

G_MODULE_EXPORT void show_hide_atoms (GtkWidget * widg, gpointer data)
{
  tint * the_data = (tint *) data;
  int j = the_data -> b;
  int k = the_data -> c;
  int l, m;
  struct project * this_proj = get_project_by_id(the_data -> a);
  gboolean v = check_menu_item_get_active ((gpointer)widg);
  if (widg != this_proj -> modelgl -> ogl_spec[j][k])
  {
    check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_spec[j][k], v);
  }
  for (l=0; l<this_proj -> steps; l++)
  {
    for (m=0; m<this_proj -> natomes; m++)
    {
      if (this_proj -> atoms[l][m].sp == k) this_proj -> atoms[l][m].show[j] = v;
    }
  }
  this_proj -> modelgl -> anim -> last -> img -> show_atom[j][k] = v;
  init_default_shaders (this_proj -> modelgl);
}

G_MODULE_EXPORT void show_hide_labels (GtkWidget * widg, gpointer data)
{
  int l, m;
  tint * id = (tint *) data;
  int j = id -> b;
  int k = id -> c;
  struct project * this_proj = get_project_by_id(id -> a);
  gboolean v = check_menu_item_get_active ((gpointer)widg);
  if (widg != this_proj -> modelgl -> ogl_lab[j][k])
  {
    check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_lab[j][k], v);
  }
  for (l=0; l<this_proj -> steps; l++)
  {
    for (m=0; m<this_proj -> natomes; m++)
    {
      if (this_proj -> atoms[l][m].sp == k)
      {
        if (this_proj -> atoms[l][m].label[j] != v)
        {
          this_proj -> atoms[l][m].label[j] = v;
        }
      }
    }
  }
  this_proj -> modelgl -> labelled = check_label_numbers (this_proj, j);
  this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT void show_hide_all_atom_labels (GtkWidget * widg, gpointer data)
{
  tint * id = (tint *) data;
  int j = id -> b;
  int k;
  gboolean show = TRUE;
  struct project * this_proj = get_project_by_id(id -> a);
  for (k=0; k<this_proj -> nspec; k++)
  {
    if (plot -> show_label[j][k]) show = FALSE;
  }
  for (k=0; k<this_proj -> nspec; k++)
  {
    check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_lab[j][k], show);
  }
}

GtkWidget * create_spec_menu (char * name, gboolean va, gboolean vb, GtkWidget * menu, GCallback handler, tint * data)
{
  GtkWidget * spec_widget = gtk3_menu_item (menu, name, IMG_NONE, NULL, handler, data, FALSE, 0, 0, TRUE, FALSE, va);
  widget_set_sensitive (spec_widget, vb);
  return spec_widget;
}

GtkWidget * create_atom_layout_widget (gchar * str, GtkWidget * widg, int val, int vb, tint * data)
{
  GtkWidget * menu = gtk_menu_new ();
  menu_item_set_submenu (widg, menu);
  GtkWidget * layout = create_menu_item (TRUE, str);
  add_menu_child (menu, layout);
  g_signal_connect (G_OBJECT (layout), "activate", G_CALLBACK(set_atom_parameter), data);
  return layout;
}

GtkWidget * show_atoms_submenu (glwin * view, int id, int at)
{
  GtkWidget * mshow = gtk_menu_new ();
  gchar * str;
  gboolean sensitive = (! at) ? TRUE : view -> anim -> last -> img -> draw_clones;
  struct project * this_proj = get_project_by_id (view -> proj);
  if (id == 0)
  {
    view -> ogl_spec[at] = g_malloc (this_proj -> nspec*sizeof*view -> ogl_spec[at]);
  }
  int i;
  for (i=0; i< this_proj -> nspec; i++)
  {
    if (at == 0)
    {
      str = g_strdup_printf ("%s", this_proj -> chemistry -> label[i]);
    }
    else
    {
      str = g_strdup_printf ("%s*", this_proj -> chemistry -> label[i]);
    }
    if (id == 0)
    {
      view -> ogl_spec[at][i] = create_spec_menu (str,
                                                  view -> anim -> last -> img -> show_atom[at][i],
                                                  sensitive,
                                                  mshow,
                                                  G_CALLBACK(show_hide_atoms),
                                                  & view -> colorp[at][i]);

    }
    else
    {
      create_spec_menu (str, view -> anim -> last -> img -> show_atom[at][i],
                        sensitive,
                        mshow,
                        G_CALLBACK(show_hide_atoms),
                        & view -> colorp[at][i]);
    }
    g_free (str);
  }
  return mshow;
}

GtkWidget * color_atoms_submenu (glwin * view, int id, int at)
{
  GtkWidget * menuc = gtk_menu_new ();
  struct project * this_proj = get_project_by_id (view -> proj);
  gchar * str;
  GtkWidget * sp;
  int i, j;
  if (at == 0)
  {
    j = 0;
  }
  else
  {
    j = this_proj -> nspec;
  }
  for (i=0; i< this_proj -> nspec; i++)
  {
    if (at == 0)
    {
      str = g_strdup_printf ("%s", this_proj -> chemistry -> label[i]);
    }
    else
    {
      str = g_strdup_printf ("%s*", this_proj -> chemistry -> label[i]);
    }
    sp = create_menu_item (FALSE, str);
    g_free (str);
    add_menu_child (menuc, sp);
    menu_item_set_submenu (sp, color_box(view, i+j, 0, 0));
  }
  if (at == 1) widget_set_sensitive (menuc, view -> anim -> last -> img -> draw_clones);
  return menuc;
}

GtkWidget * label_atoms_submenu (glwin * view, int id, int at)
{
  GtkWidget * menul = gtk_menu_new ();
  GtkWidget * all = create_menu_item (FALSE, "Show/Hide all");
  g_signal_connect (G_OBJECT (all), "activate", G_CALLBACK(show_hide_all_atom_labels), & view -> colorp[at][0]);
  add_menu_child (menul, all);
  GtkWidget * l_show = create_menu_item (FALSE, "Show");
  add_menu_child (menul, l_show);
  GtkWidget * mshow = gtk_menu_new ();
  menu_item_set_submenu (l_show, mshow);
  struct project * this_proj = get_project_by_id (view -> proj);
  gchar * str;
  gboolean sensitive = (! at) ? TRUE : view -> anim -> last -> img -> draw_clones;
  if (id == 0)
  {
    view -> ogl_lab[at] = g_malloc (this_proj -> nspec*sizeof*view -> ogl_lab[at]);
  }
  int i;
  for (i=0; i< this_proj -> nspec; i++)
  {
    if (at == 0)
    {
      str = g_strdup_printf ("%s", this_proj -> chemistry -> label[i]);
    }
    else
    {
      str = g_strdup_printf ("%s*", this_proj -> chemistry -> label[i]);
    }
    if (id == 0)
    {
      view -> ogl_lab[at][i] = create_spec_menu (str,
                                                 view -> anim -> last -> img -> show_label[at][i],
                                                 sensitive,
                                                 mshow,
                                                 G_CALLBACK(show_hide_labels),
                                                 & view -> colorp[at][i]);
    }
    else
    {
      create_spec_menu (str,
                        view -> anim -> last -> img -> show_label[at][i],
                        sensitive,
                        mshow,
                        G_CALLBACK(show_hide_labels),
                        & view -> colorp[at][i]);
    }
  }
  if (at == 0 || id == 1)
  {
    GtkWidget * l_sel = create_menu_item (FALSE, "Select atom(s)");
    add_menu_child (menul, l_sel);
    g_signal_connect (G_OBJECT (l_sel), "activate",G_CALLBACK(atom_properties), & view -> colorp[at][2]);
    if (id == 1) widget_set_sensitive (l_sel, sensitive);
    GtkWidget * l_adv = add_advanced_item (menul, G_CALLBACK(atom_properties), (gpointer)& view -> colorp[at][1], FALSE, 0, 0);
    if (id == 1) widget_set_sensitive (l_adv, sensitive);
  }
  else
  {
    view -> ogl_clones[2] = create_menu_item (FALSE, "Select atom(s)");
    add_menu_child (menul, view -> ogl_clones[2]);
    g_signal_connect (G_OBJECT (view -> ogl_clones[2]), "activate",G_CALLBACK(atom_properties), & view -> colorp[at][2]);
    widget_set_sensitive (view -> ogl_clones[2], sensitive);
    view -> ogl_clones[3] = add_advanced_item (menul, G_CALLBACK(atom_properties), (gpointer)& view -> colorp[at][1], FALSE, 0, 0);
    widget_set_sensitive (view -> ogl_clones[3], sensitive);
  }
  return menul;
}

GtkWidget * menu_atoms (glwin * view, int id, int at)
{
  int i;
  GtkWidget * widg;
  gchar * str;
  struct project * this_proj = get_project_by_id(view -> proj);
  GtkWidget * menua = gtk_menu_new ();
  add_menu_child (menua, menu_item_new_with_submenu ("Show", TRUE, show_atoms_submenu (view, id, at)));
  if (at == 1 && id == 0)
  {
    view -> ogl_clones[1] = color_atoms_submenu (view, id, at);
    add_menu_child (menua, menu_item_new_with_submenu ("Color(s)", ! (view -> anim -> last -> img -> color_map[0] != 0), view -> ogl_clones[1]));
  }
  else
  {
    add_menu_child (menua, menu_item_new_with_submenu ("Color(s)", ! (view -> anim -> last -> img -> color_map[0] != 0), color_atoms_submenu (view, id, at)));
  }
  add_menu_child (menua, menu_item_new_with_submenu ("Label(s)", TRUE, label_atoms_submenu (view, id, at)));

  i = view -> anim -> last -> img -> style;
  str = label_atpts (this_proj, view, 2*at);
  if (id == 0)
  {
    view -> ogl_atoms[4*at] = create_menu_item(FALSE, "Radius(ii)");
    add_menu_child (menua, view -> ogl_atoms[4*at]);
    view -> ogl_atoms[4*at+1] = create_atom_layout_widget (str, view -> ogl_atoms[4*at], 0, i, & view -> colorp[at][0]);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (view -> ogl_atoms[4*at+1], 0);
    }
  }
  else if (i == SPHERES || i == BALL_AND_STICK)
  {
    widg = create_menu_item (FALSE, "Radius(ii)");
    add_menu_child (menua, widg);
    widg = create_atom_layout_widget (str, widg, 0, i, & view -> colorp[at][0]);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (widg, 0);
    }
  }
  g_free (str);

  str = label_atpts (this_proj, view, 1+2*at);
  if (id == 0)
  {
    view -> ogl_atoms[4*at+2] = create_menu_item(FALSE, "Point size(s)");
    add_menu_child (menua, view -> ogl_atoms[4*at+2]);
    view -> ogl_atoms[4*at+3] = create_atom_layout_widget (str, view -> ogl_atoms[4*at+2], 1, i, & view -> colorp[at][1]);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (view -> ogl_atoms[4*at+3], 0);
    }
  }
  else if (i == WIREFRAME || i == PUNT)
  {
    widg = create_menu_item (FALSE, "Point size(s)");
    add_menu_child (menua, widg);
    widg = create_atom_layout_widget (str, widg, 1, i, & view -> colorp[at][1]);
    if (at == 1 && ! view -> anim -> last -> img -> draw_clones)
    {
      widget_set_sensitive (widg, 0);
    }
  }
  g_free (str);
  if (id == 0 && at == 1)
  {
    view -> ogl_clones[4] = add_advanced_item (menua, G_CALLBACK(atom_properties), (gpointer)& view -> colorp[at][0], FALSE, 0, 0);
    if (at == 1)
    {
      widget_set_sensitive (view -> ogl_clones[4], view -> anim -> last -> img -> draw_clones);
    }
  }
  else
  {
    GtkWidget * advanced = add_advanced_item (menua, G_CALLBACK(atom_properties), (gpointer)& view -> colorp[at][0], FALSE, 0, 0);
    if (at == 1)
    {
      widget_set_sensitive (advanced, view -> anim -> last -> img -> draw_clones);
    }
  }
  return menua;
}
#else

extern G_MODULE_EXPORT void atom_properties (GSimpleAction * action, GVariant * state, gpointer data);

G_MODULE_EXPORT void show_hide_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  tint * the_data = (tint *) data;
  int j = the_data -> b;
  int k = the_data -> c;
  int l, m;
  struct project * this_proj = get_project_by_id(the_data -> a);
  GVariant * state;
  gboolean show;
  if (action)
  {
    state = g_action_get_state (G_ACTION (action));
    show = ! g_variant_get_boolean (state);
  }
  else
  {
    show = this_proj -> modelgl -> anim -> last -> img -> show_atom[j][k];
  }

  for (l=0; l<this_proj -> steps; l++)
  {
    for (m=0; m<this_proj -> natomes; m++)
    {
      if (this_proj -> atoms[l][m].sp == k) this_proj -> atoms[l][m].show[j] = show;
    }
  }
  this_proj -> modelgl -> anim -> last -> img -> show_atom[j][k] = show;
  init_default_shaders (this_proj -> modelgl);
  if (action)
  {
    g_action_change_state (G_ACTION (action), g_variant_new_boolean (show));
    g_variant_unref (state);
  }
}

G_MODULE_EXPORT void show_hide_labels (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  int j, k, l, m;
  tint * id = (tint *) data;
  j = id -> b;
  k = id -> c;
  gboolean show;
  GVariant * state;
  struct project * this_proj = get_project_by_id(id -> a);
  if (action)
  {
    state = g_action_get_state (G_ACTION (action));
    show = ! g_variant_get_boolean (state);
  }
  else
  {
    show = ! this_proj -> modelgl -> anim -> last -> img -> show_label[j][k];
  }
  for (l=0; l<this_proj -> steps; l++)
  {
    for (m=0; m<this_proj -> natomes; m++)
    {
      if (this_proj -> atoms[l][m].sp == k)
      {
        if (this_proj -> atoms[l][m].label[j] != show)
        {
          this_proj -> atoms[l][m].label[j] = show;
        }
      }
    }
  }
  this_proj -> modelgl -> labelled = check_label_numbers (this_proj, j);
  this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  update (this_proj -> modelgl);
  if (action)
  {
    g_action_change_state (G_ACTION (action), g_variant_new_boolean (show));
    g_variant_unref (state);
  }
}

G_MODULE_EXPORT void show_hide_all_atom_labels (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  tint * id = (tint *) data;
  int i, j, k;
  struct project * this_proj = get_project_by_id(id -> a);
  i = id -> b;
  gboolean show = ! this_proj -> atoms[0][0].label[i];
  for (j=0; j<this_proj -> steps; j++)
  {
    for (k=0; k<this_proj -> natomes; k++)
    {
      this_proj -> atoms[j][k].label[i] = show;
    }
  }
  this_proj -> modelgl -> labelled = check_label_numbers (this_proj, i);
  this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  update (this_proj -> modelgl);
  update_menu_bar (this_proj -> modelgl);
}

GMenu * label_atoms_submenu (glwin * view, int at, gboolean sensitive)
{
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, "Show/Hide All", (at) ? "clones-labels-all" : "atoms-labels-all", 0, NULL, IMG_NONE, NULL,
                      FALSE, G_CALLBACK(show_hide_all_atom_labels), & view -> colorp[at][0], FALSE, FALSE, FALSE, sensitive);
  GMenu * smenu = g_menu_new ();
  struct project * this_proj = get_project_by_id (view -> proj);
  gchar * str;
  int i;
  for (i=0; i< this_proj -> nspec; i++)
  {
    if (at == 0)
    {
      str = g_strdup_printf ("%s", this_proj -> chemistry -> label[i]);
    }
    else
    {
      str = g_strdup_printf ("%s*", this_proj -> chemistry -> label[i]);
    }
    append_opengl_item (view, smenu, str, (! at) ? "atom-label" : "clone-label", i, NULL, IMG_NONE, NULL,
                        FALSE, G_CALLBACK(show_hide_labels), & view -> colorp[at][i],
                        TRUE, view -> anim -> last -> img -> show_label[at][i], FALSE, sensitive);
  }
  g_menu_append_submenu (menu, "Show", (GMenuModel*)smenu);
  append_opengl_item (view, menu, "Select atom(s)", (! at) ? "atom-select" : "clone-select", 0, NULL, IMG_NONE, NULL,
                      FALSE, G_CALLBACK(atom_properties), & view -> colorp[at][2], FALSE, FALSE, FALSE, sensitive);
  append_opengl_item (view, menu, "Advanced", (! at) ? "atom-lab-adv" : "clone-lab-adv", 0, NULL, IMG_STOCK, DPROPERTIES,
                      FALSE, G_CALLBACK(atom_properties), & view -> colorp[at][1], FALSE, FALSE, FALSE, sensitive);
  return menu;
}

GMenu * color_atoms_submenu (glwin * view, int at, gboolean sensitive)
{
  GMenu * menu = g_menu_new ();
  GMenu * menuc;
  struct project * this_proj = get_project_by_id (view -> proj);
  gchar * stra, * strb;
  int i;
  for (i=0; i< this_proj -> nspec; i++)
  {
    if (at == 0)
    {
      stra = g_strdup_printf ("%s", this_proj -> chemistry -> label[i]);
    }
    else
    {
      stra = g_strdup_printf ("%s*", this_proj -> chemistry -> label[i]);
    }
    strb = g_strdup_printf ("%s", (! at) ? "atom-color" : "clone-color");
    menuc = g_menu_new ();
    append_opengl_item (view, menuc, strb, strb, i, NULL, IMG_NONE, NULL, TRUE, NULL, NULL, FALSE, FALSE, FALSE, FALSE);
    append_opengl_item (view, menuc, "More colors ...", strb, i, NULL, IMG_NONE, NULL,
                        FALSE, G_CALLBACK(to_run_atom_color_window), & view -> colorp[0][i+at*this_proj -> nspec], FALSE, FALSE, FALSE, sensitive);
    g_menu_append_submenu (menu, stra, (GMenuModel*)menuc);
    g_free (stra);
    g_free (strb);
    g_object_unref (menuc);
  }
  return menu;
}

GMenu * show_atoms_submenu (glwin * view, int at, gboolean sensitive)
{
  GMenu * menu = g_menu_new ();
  gchar * str;
  struct project * this_proj = get_project_by_id (view -> proj);
  int i;
  for (i=0; i< this_proj -> nspec; i++)
  {
    if (at == 0)
    {
      str = g_strdup_printf ("%s", this_proj -> chemistry -> label[i]);
    }
    else
    {
      str = g_strdup_printf ("%s*", this_proj -> chemistry -> label[i]);
    }
    append_opengl_item (view, menu, str, (! at) ? "show-atom" : "show-clone", i, NULL, IMG_NONE, NULL,
                        FALSE, G_CALLBACK(show_hide_atoms), & view -> colorp[at][i],
                        TRUE, view -> anim -> last -> img -> show_atom[at][i], FALSE, sensitive);
    g_free (str);
  }
  return menu;
}

GMenu * menu_atoms (glwin * view, int at)
{
  int i = view -> anim -> last -> img -> style;
  gboolean sensitive = (at) ? view -> anim -> last -> img -> draw_clones : TRUE;

  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Show", (GMenuModel*) show_atoms_submenu(view, at, sensitive));
  g_menu_append_submenu (menu, "Color(s)", (GMenuModel*) color_atoms_submenu (view, at, sensitive));
  g_menu_append_submenu (menu, "Label(s)", (GMenuModel*) label_atoms_submenu (view, at, sensitive));
  GMenuItem * item;
  if (i == SPHERES || i == BALL_AND_STICK)
  {
    item = g_menu_item_new ("Radius(ii)", (sensitive) ? NULL : "None");
    g_menu_item_set_attribute (item, "custom", "s", (at) ? "clone-radii" : "atom-radii", NULL);
    g_menu_append_item (menu, item);
  }
  if (i == WIREFRAME || i == PUNT)
  {
    item = g_menu_item_new ("Point size(s)", (sensitive) ? NULL : "None");
    g_menu_item_set_attribute (item, "custom", "s", (at) ? "clone-pts" : "atom-pts", NULL);
    g_menu_append_item (menu, item);
  }
  append_opengl_item (view, menu, "Advanced", (! at) ? "atom-advanced" : "clone-advanced", 0, NULL, IMG_STOCK, DPROPERTIES,
                      FALSE, G_CALLBACK(atom_properties), & view -> colorp[at][0],
                      FALSE, FALSE, FALSE, sensitive);
  return menu;
}
#endif
