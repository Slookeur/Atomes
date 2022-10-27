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
#include "project.h"
#include "calc.h"
#include "glview.h"
#include "initcoord.h"
#include "submenus.h"
#include "color_box.h"

extern G_MODULE_EXPORT void opengl_advanced (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void coord_properties (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void set_style (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void set_render (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void set_mode (GtkWidget * widg, gpointer data);
extern void set_sensitive_coord_menu (glwin * view, gboolean status);
extern void set_color_map_sensitive (glwin * view);

extern gboolean spin (gpointer data);
extern G_MODULE_EXPORT void spin_stop (GtkButton * but, gpointer data);
extern G_MODULE_EXPORT void spin_go (GtkWidget * widg, gpointer data);
extern void prep_all_coord_menus (glwin * view);
extern void update_menus (glwin * view);
extern G_MODULE_EXPORT void set_box_axis_style (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void window_measures (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void window_recorder (GtkWidget * widg, gpointer data);
extern void window_encode (glwin * view, gboolean video);
extern GtkWidget * menupoly (glwin * view, int jd, int id, int hd, gchar * poln);
extern G_MODULE_EXPORT void set_color_map (GtkWidget * widg, gpointer data);
#ifdef GTK4
extern void update_menu_bar (glwin * view);
extern G_MODULE_EXPORT void set_full_screen (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void add_object (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void label_unlabel_atoms (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void select_unselect_atoms (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void edit_in_new_project (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void remove_the_atoms (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void copy_the_atoms (GSimpleAction * action, GVariant * parameter, gpointer data);
#else
extern G_MODULE_EXPORT void set_full_screen (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void to_reset_view (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void add_object (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void label_unlabel_atoms (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void select_unselect_atoms (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void remove_the_atoms (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void copy_the_atoms (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void edit_in_new_project (GtkWidget * widg, gpointer data);
#endif
extern void save_rotation_quaternion (glwin * view);
extern void rotate_x_y (glwin * view, double angle_x, double angle_y);
extern void translate (struct project * this_proj, int status, int axis, vec3_t trans);
extern int selected_aspec;
extern int is_selected;
extern int is_labelled;
extern G_MODULE_EXPORT void on_create_new_project (GtkWidget * widg, gpointer data);
extern gchar * action_atoms[3];
extern int get_measure_type (glwin * view);

#ifdef GTK3
GtkWidget * prep_rings_menu (glwin * view, int id, int ri)
{
  if (id == 0)
  {
    return menu_rings (view, 0, ri);
  }
  else
  {
    return menupoly (view, 0, 2, ri, NULL);
  }
}

GtkWidget * coord_menu (glwin * view)
{
  int i, j, k;
  GtkWidget * widg;
  for (j=4; j<10; j++)
  {
    if (j<9)
    {
      if (view -> ogl_poly[0][j] != NULL)
      {
        for (k=0; k<get_project_by_id(view -> proj) -> coord -> totcoord[j]; k++)
        {
          if (view -> ogl_poly[0][j][k] != NULL)
          {
            if (GTK_IS_WIDGET(view -> ogl_poly[0][j][k]))
            {
              widg = gtk_widget_get_parent (view -> ogl_poly[0][j][k]);
              if (GTK_IS_WIDGET(widg))
              {
                g_object_ref (view -> ogl_poly[0][j][k]);
                gtk_container_remove (GTK_CONTAINER(widg), view -> ogl_poly[0][j][k]);
              }
            }
          }
        }
      }
    }
    if (view -> ogl_geom[0][j] != NULL)
    {
      for (k=0; k<get_project_by_id(view -> proj) -> coord -> totcoord[j]; k++)
      {
        if (view -> ogl_geom[0][j][k] != NULL)
        {
          if (GTK_IS_WIDGET(view -> ogl_geom[0][j][k]))
          {
            widg =  gtk_widget_get_parent (view -> ogl_geom[0][j][k]);
            if (GTK_IS_WIDGET(widg))
            {
              g_object_ref (view -> ogl_geom[0][j][k]);
              gtk_container_remove (GTK_CONTAINER(widg), view -> ogl_geom[0][j][k]);
            }
          }
        }
      }
    }
  }
  if (view -> ogl_chains[0]) view -> ogl_chains[0] = destroy_this_widget (view -> ogl_chains[0]);
  for (i=0; i<2; i++)
  {
    for (j=2; j<4; j++)
    {
      detach_frag_mol_menu (view, i, j);
    }
  }
  for (i=1; i<OGL_COORDS; i++)
  {
    view -> ogl_coord[i] = destroy_this_widget (view -> ogl_coord[i]);
  }
  for (i=0; i<2; i++)
  {
    view -> ogl_rings[i*6] = menu_item_new_with_submenu ("Ring(s)", view -> rings, prep_rings_menu (view, i*6, 0));
  }
  view -> ogl_chains[0] = menu_item_new_with_submenu ("Chain(s)", view -> chains, add_menu_coord (view, 0, 9));
  view -> ogl_coord[1] = menu_coord (view, 0);
  view -> ogl_coord[2] = menu_item_new_with_submenu ("Polyhedra", TRUE, menu_poly (view, 0));
  view -> ogl_coord[3] = menu_item_new_with_submenu ("Fragment(s)", get_project_by_id(view -> proj) -> coord -> totcoord[2], add_menu_coord (view, 0, 2));
  view -> ogl_coord[4] = menu_item_new_with_submenu ("Molecule(s)", get_project_by_id(view -> proj) -> coord -> totcoord[3], add_menu_coord (view, 0, 3));
  GtkWidget * menu = gtk_menu_new ();
  add_menu_child (menu, view -> ogl_coord[1]);
  add_menu_child (menu, view -> ogl_coord[2]);
  add_menu_child (menu, view -> ogl_rings[0]);
  add_menu_child (menu, view -> ogl_chains[0]);
  add_menu_child (menu, view -> ogl_coord[3]);
  add_menu_child (menu, view -> ogl_coord[4]);
#ifdef MENU_ICONS
  add_menu_child (menu, gtk3_image_menu_item ("Advanced", IMG_STOCK, (gpointer)DPROPERTIES, G_CALLBACK(coord_properties), (gpointer)& view -> colorp[30][0], "Ctrl+E", FALSE, FALSE, FALSE));
#else
  add_advanced_item (menu, G_CALLBACK(coord_properties), (gpointer)& view -> colorp[30][0], TRUE, GDK_KEY_e, GDK_CONTROL_MASK);
#endif
  return menu;
}
#endif

void update_all_menus (glwin * view, int nats)
{
#ifdef GTK3
  int i, j, k, l;
  i = view -> anim -> last -> img -> style;
  j = (nats <= 1000) ? BALL_AND_STICK : DEFAULT_STYLE;
  if (i != j)
  {
    check_menu_item_set_active ((gpointer)view -> ogl_styles[j], FALSE);
    if (i != SPACEFILL)
    {
      check_menu_item_set_active ((gpointer)view -> ogl_styles[i], TRUE);
      set_style (view -> ogl_styles[i], & view -> colorp[i][0]);
    }
    else
    {
      i = view -> anim -> last -> img -> filled_type;
      check_menu_item_set_active ((gpointer)view -> filled_styles[i], TRUE);
      set_style (view -> filled_styles[i], & view -> colorp[OGL_STYLES+i][0]);
    }
  }
  i = view -> anim -> last -> img -> render;
  j = FILL;
  if (i != j)
  {
    check_menu_item_set_active ((gpointer)view -> ogl_render[j], FALSE);
    check_menu_item_set_active ((gpointer)view -> ogl_render[i], TRUE);
    set_render (view -> ogl_render[i], & view -> colorp[i][0]);
  }

  update_menus (view);
  for (i=0; i<2; i++)
  {
    widget_set_sensitive (view -> ogl_box[i], active_cell -> ltype);
    if (view -> anim -> last -> img -> box_axis[i] == NONE)
    {
      check_menu_item_set_active ((gpointer)view -> ogl_box_axis[i][0], FALSE);
      set_box_axis_style (view -> ogl_box_axis[i][0], & view -> colorp[0][i]);
    }
    else
    {
      j = (view -> anim -> last -> img -> box_axis[i] == WIREFRAME) ? 1 : 2;
      k = j*j;
      l = (view -> anim -> last -> img -> box_axis[i] == WIREFRAME) ? CYLINDERS : WIREFRAME;
      check_menu_item_set_active ((gpointer)view -> ogl_box_axis[i][0], TRUE);
      set_box_axis_style (view -> ogl_box_axis[i][0], & view -> colorp[0][i]);
      view -> anim -> last -> img -> box_axis[i] = l;
      check_menu_item_set_active ((gpointer)view -> ogl_box_axis[i][j], TRUE);
      set_box_axis_style (view -> ogl_box_axis[i][j], & view -> colorp[k][i]);
    }
  }
  check_menu_item_set_active ((gpointer)view -> ogl_rep[view -> anim -> last -> img -> rep], TRUE);
  check_menu_item_set_active ((gpointer)view -> ogl_rep[! view -> anim -> last -> img -> rep], FALSE);
  for (i=0; i<5; i++) check_menu_item_set_active ((gpointer)view -> ogl_box_axis[1][8+i], FALSE);
  if (view -> anim -> last -> img -> axispos != CUSTOM)
  {
    check_menu_item_set_active ((gpointer)view -> ogl_box_axis[1][8+view -> anim -> last -> img -> axispos], TRUE);
  }
  set_advanced_bonding_menus (view);
  widget_set_sensitive (view -> ogl_clones[0], view -> allbonds[1]);
  update_rings_menus (view);
  update_chains_menus (view);
  check_menu_item_set_active ((gpointer)view -> ogl_clones[0], view -> anim -> last -> img -> draw_clones);
  check_menu_item_set_active ((gpointer)view -> ogl_clones[5], view -> anim -> last -> img -> cloned_poly);
  int * cmap = save_color_map (view);
  set_color_map_sensitive (view);
  if (view -> color_styles[0])
  {
    check_menu_item_set_active ((gpointer)view -> color_styles[0], TRUE);
    set_color_map (view -> color_styles[0], & view  -> colorp[0][0]);
  }
  if (view -> color_styles[ATOM_MAPS])
  {
    check_menu_item_set_active ((gpointer)view -> color_styles[ATOM_MAPS], TRUE);
    set_color_map (view -> color_styles[ATOM_MAPS], & view  -> colorp[ATOM_MAPS][0]);
  }
  restore_color_map (view, cmap);
  g_free (cmap);
  gtk_range_set_value (GTK_RANGE(view -> ogl_quality), view -> anim -> last -> img -> quality);
#else
  update_menu_bar (view);
#endif
}

G_MODULE_EXPORT void render_gl_image (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *) data;
  window_encode (view, FALSE);
}

#ifdef GTK3

void menu_items_opengl (GtkWidget * menu, glwin * view, int pop)
{
#ifdef MENU_ICONS
  GtkWidget * style = gtk3_menu_item (menu, "Style", IMG_FILE, (gpointer)PACKAGE_MOL, NULL, NULL, FALSE, 0, 0, FALSE, FALSE, get_project_by_id(view -> proj) -> nspec);
  menu_item_set_submenu (style, menu_style(view, pop));
#else
  add_menu_child (menu, menu_item_new_with_submenu ("Style", get_project_by_id(view -> proj) -> nspec, menu_style(view, pop)));
#endif
  add_menu_child (menu, menu_item_new_with_submenu ("Color Scheme(s)", get_project_by_id(view -> proj) -> nspec, menu_map(view, pop)));
  add_menu_child (menu, menu_item_new_with_submenu ("Render", get_project_by_id(view -> proj) -> nspec, menu_render(view, pop)));
  add_menu_child (menu, menu_item_new_with_submenu ("Quality", get_project_by_id(view -> proj) -> nspec, menu_quality(view, pop)));
  gtk3_menu_item (menu, "Material And Light(s)", IMG_NONE, NULL, G_CALLBACK(opengl_advanced), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
  gtk3_menu_item (menu, "Render Image", IMG_FILE, (gpointer)PACKAGE_IMG, G_CALLBACK(render_gl_image), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
}

GtkWidget * menu_opengl (glwin * view, int pop)
{
  GtkWidget * menu = gtk_menu_new ();
  menu_items_opengl (menu, view, pop);
  return menu;
}

void menu_items_model (GtkWidget * menu, glwin * view, int pop)
{
  if (get_project_by_id(view -> proj) -> nspec)
  {
    add_menu_child (menu, menu_item_new_with_submenu ("Atom(s)", TRUE, menu_atoms (view, pop, 0)));
    add_menu_child (menu, menu_item_new_with_submenu ("Bond(s)", TRUE, menu_bonds (view, pop, 0)));
    add_menu_child (menu, menu_item_new_with_submenu ("Clone(s)", TRUE, menu_clones (view, pop)));
    add_menu_child (menu, menu_box_axis (view, 0, 0));
  }
}

GtkWidget * menu_model (glwin * view, int pop)
{
  GtkWidget * menu = gtk_menu_new ();
  menu_items_model (menu, view, pop);
  return menu;
}

void menu_items_view (GtkWidget * menu, glwin * view, int popm)
{
  add_menu_child (menu, menu_item_new_with_submenu ("Representation", TRUE, menu_rep (view, popm)));
  add_menu_child (menu, menu_item_new_with_submenu ("Projection", TRUE, menu_proj (view)));
  add_menu_child (menu, menu_item_new_with_submenu ("Background", TRUE, menu_back (view)));
  if (get_project_by_id(view -> proj) -> nspec) add_menu_child (menu, menu_box_axis (view, popm, 1));
  if (! popm)
  {
    add_menu_separator (menu);
    gtk3_menu_item (menu, "Reset view", IMG_STOCK, (gpointer)FITBEST, G_CALLBACK(to_reset_view), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
    gtk3_menu_item (menu, "Center molecule", IMG_STOCK, (gpointer)FITBEST, G_CALLBACK(to_center_this_molecule), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
    add_menu_separator (menu);
#ifdef MENU_ICONS
    add_menu_child (menu, gtk3_image_menu_item ("Fullscreen", IMG_STOCK, (gpointer)FULLSCREEN, G_CALLBACK(set_full_screen), (gpointer)view, "Ctrl+F", FALSE, FALSE, FALSE));
#else
    gtk3_menu_item (menu, "Fullscreen", IMG_STOCK, (gpointer)FULLSCREEN, G_CALLBACK(set_full_screen), (gpointer)view, TRUE, GDK_KEY_f, GDK_CONTROL_MASK, FALSE, FALSE, FALSE);
#endif
  }
}

GtkWidget * menu_view (glwin * view, int popm)
{
  GtkWidget * menu = gtk_menu_new ();
  menu_items_view (menu, view, popm);
  return menu;
}
#endif

void prepare_opengl_menu_bar (glwin * view)
{
#ifdef GTK3
  view -> ogl_coord[0] = destroy_this_widget (view -> ogl_coord[0]);
#endif
  view -> menu_bar = destroy_this_widget (view -> menu_bar);
#ifdef GTK3
  view -> menu_bar = gtk_menu_bar_new ();
  add_menu_child (view -> menu_bar, menu_item_new_with_submenu ("OpenGL", TRUE, menu_opengl(view, 0)));
  struct project * this_proj = get_project_by_id (view -> proj);
  add_menu_child (view -> menu_bar, menu_item_new_with_submenu ("Model", this_proj -> nspec, menu_model(view, 0)));
  view -> ogl_coord[0] = create_menu_item (FALSE, "Chemistry");
  add_menu_child (view -> menu_bar, view -> ogl_coord[0]);
  widget_set_sensitive (view -> ogl_coord[0], this_proj -> nspec);
  if (this_proj -> nspec)
  {
    menu_item_set_submenu (view -> ogl_coord[0], coord_menu (view));
  }
  add_menu_child (view -> menu_bar, menu_item_new_with_submenu ("Tools", TRUE, menu_tools(view, 0)));
  add_menu_child (view -> menu_bar, menu_item_new_with_submenu ("View", TRUE, menu_view(view, 0)));
  add_menu_child (view -> menu_bar, menu_anim (view, 0));
  show_the_widgets (view -> menu_bar);

  if (this_proj -> nspec) update_all_menus (view, this_proj -> natomes);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, view -> menu_box, view -> menu_bar, TRUE, TRUE, 0);
  show_the_widgets (view -> menu_bar);
#else
  update_menu_bar (view);
#endif
}

void change_color_map (glwin * view, int col)
{
#ifdef GTK3
  int i, j;
  i = ATOM_MAPS-1;
  if (view -> custom_map) i++;
  if (view -> anim -> last -> img -> color_map[col] < i-1)
  {
    if (view -> anim -> last -> img -> color_map[col] < 2)
    {
      j = col*ATOM_MAPS + view -> anim -> last -> img -> color_map[col] + 1;
    }
    else if (view -> anim -> last -> img -> color_map[col] == 2 && view -> adv_bonding[0])
    {
      j = col*ATOM_MAPS + view -> anim -> last -> img -> color_map[col] + 1;
    }
    else if (view -> anim -> last -> img -> color_map[col] == 3 && view -> adv_bonding[1])
    {
      j = col*ATOM_MAPS + view -> anim -> last -> img -> color_map[col] + 1;
    }
    else if (view -> anim -> last -> img -> color_map[col] == 4 && get_project_by_id(view -> proj) -> force_field[0])
    {
      j = col*ATOM_MAPS + view -> anim -> last -> img -> color_map[col] + 1;
    }
    else
    {
      j = col*ATOM_MAPS + ((view -> custom_map) ? 6 : 0);
    }
  }
  else
  {
    j = col*ATOM_MAPS;
  }
  gboolean was_input = reading_input;
  reading_input = TRUE;
  check_menu_item_set_active ((gpointer)view -> color_styles[j], TRUE);
  set_color_map (view -> color_styles[j], & view -> colorp[j][0]);
  reading_input = was_input;
#endif
}

void set_motion (glwin * view, int axis, int da, int db, gboolean UpDown, GdkModifierType state)
{
  if (state & GDK_CONTROL_MASK)
  {
    if (view -> mode == EDITION)
    {
      vec3_t trans;
      if (axis)
      {
        trans = vec3(view -> anim -> last -> img -> p_depth*(double) da / view -> pixels[axis], 0.0, 0.0);
      }
      else
      {
        trans = vec3(0.0, view -> anim -> last -> img -> p_depth*(double) da / view -> pixels[axis], 0.0);
      }
      translate (get_project_by_id(view -> proj), 1, 1, trans);
      init_default_shaders (view);
      update (view);
    }
    else if (state & GDK_SHIFT_MASK)
    {
       spin_go (NULL, & view -> colorp[db][axis]);
    }
    else
    {
      view -> anim -> last -> img -> c_shift[! axis] += (double) da / view -> pixels[axis];
      if (view -> camera_widg[! axis + 5])
      {
        if (GTK_IS_WIDGET(view -> camera_widg[! axis + 5]))
        {
          gtk_spin_button_set_value ((GtkSpinButton *)view -> camera_widg[! axis + 5], - view -> anim -> last -> img -> c_shift[! axis]);
        }
      }
      update (view);
    }
  }
  else if ((state & GDK_SHIFT_MASK) && UpDown)
  {
    if (db == 1 && view -> anim -> last -> img -> zoom > ZOOM_MAX) zoom (view, -1);
    if (db == 3) zoom (view, 1);
    update (view);
  }
  else if (state != GDK_SHIFT_MASK)
  {
    save_rotation_quaternion (view);
    double cameraAngle[2] = {0.0, 0.0};
    cameraAngle[axis] = 0.5*da;
    rotate_x_y (view, cameraAngle[0], cameraAngle[1]);
    update (view);
  }
}

mat4_t insert_projection (glwin * view)
{
  GLdouble w, h;
  GLdouble dw, dh;
  double gleft, gright, gbottom, gtop;
  double zoom = view -> anim -> last -> img -> zoom;
  zoom *= (view -> anim -> last -> img -> p_depth /  view -> anim -> last -> img -> gnear);
  dw = view -> anim -> last -> img -> c_shift[0]*2.0*zoom;
  dh = view -> anim -> last -> img -> c_shift[1]*2.0*zoom;
  double aspect = (double)view -> pixels[0] / (double)view -> pixels[1];
  if (aspect > 1.0)
  {
    w = zoom * aspect;
    h = zoom;
  }
  else
  {
    w = zoom;
    h = zoom / aspect;
  }
  gleft = -w + dw;
  gright = w + dw;
  gbottom = -h + dh;
  gtop = h + dh;
  return m4_ortho (gleft, gright, gbottom, gtop, -view -> anim -> last -> img -> gfar, view -> anim -> last -> img -> gfar);
}

vec3_t get_insertion_coordinates (glwin * view)
{
  vec3_t pos;
  mat4_t insert_pmv_matrix;
  insert_pmv_matrix = m4_mul (insert_projection (view), view -> model_view_matrix);
  pos = vec3 (view -> mouseX, view -> mouseY, 0.75);
  return v3_un_project (pos, view -> view_port, insert_pmv_matrix);
}

void glwin_key_pressed (guint keyval, GdkModifierType state, gpointer data)
{
  glwin * view = (glwin *)data;
  int i;
  switch (keyval)
  {
    case GDK_KEY_Escape:
      if (view -> fullscreen)
      {
#ifdef GTK4
        set_full_screen (NULL, NULL, view);
#else
        set_full_screen (NULL, view);
#endif
      }
      break;
    case GDK_KEY_Delete:
      if (get_project_by_id(view -> proj) -> natomes && ! is_atom_win_active(view) && get_project_by_id(view -> proj) -> steps == 1 && view -> mode == EDITION)
      {
        opengl_project_changed (view -> proj);
        is_selected = 1;
        selected_aspec = -1;
#ifdef GTK4
        remove_the_atoms (NULL, NULL, & cut_sel);
#else
        remove_the_atoms (NULL, & cut_sel);
#endif
      }
      break;
    case GDK_KEY_space:
      if (get_project_by_id(view -> proj) -> natomes)
      {
        if (view -> spin[0] || view -> spin[1])
        {
          for (i=0; i<2; i++)
          {
            view -> spin[i+2] = view -> spin[i];
            view -> spin_speed[i+2] = view -> spin_speed[i];
            view -> spin[i] = FALSE;
          }
        }
        else if (view -> spin[2] || view -> spin[3])
        {
          for (i=0; i<2; i++)
          {
            view -> spin[i] = view -> spin[i+2];
            view -> spin_speed[i] = view -> spin_speed[i+2];
            g_timeout_add (REFRESH, (GSourceFunc) spin, & view -> colorp[0][i]);
            view -> spin[i+2] = FALSE;
            view -> spin_speed[i+2] = 0;
          }
        }
      }
      break;
    case GDK_KEY_a:
      if ((state & GDK_CONTROL_MASK) && get_project_by_id(view -> proj) -> natomes)
      {
        opengl_project_changed (view -> proj);
        selected_aspec = -1;
        i = get_measure_type (view);
        if (view -> anim -> last -> img -> selected[i] -> selected == opengl_project -> natomes)
        {
#ifdef GTK4
          select_unselect_atoms (NULL, NULL, & view -> colorp[0][0]);
#else
          select_unselect_atoms (NULL, & view -> colorp[0][0]);
#endif
        }
        else
        {
#ifdef GTK4
          select_unselect_atoms (NULL, NULL, & view -> colorp[0][1]);
#else
          select_unselect_atoms (NULL, & view -> colorp[0][1]);
#endif
        }
      }
#ifdef GTK4
      else if (state & GDK_ALT_MASK)
#else
      else if (state & GDK_MOD1_MASK)
#endif
      {
        if (view -> mode == EDITION)
        {
#ifdef GTK4
          set_mode (NULL, & view -> colorp[0][0]);
#else
          // GTK3 Menu Action To Check
          check_menu_item_set_active ((gpointer)view -> ogl_mode[0], TRUE);
          set_mode (view -> ogl_mode[0], & view -> colorp[0][0]);
#endif
        }
      }
      else if (get_project_by_id(view -> proj) -> natomes)
      {
        change_color_map (view, 0);
        update (view);
      }
      break;
    case GDK_KEY_b:
      if (get_project_by_id(view -> proj) -> natomes)
      {
#ifdef GTK4
          set_style (NULL, & view -> colorp[BALL_AND_STICK][0]);
#else
          // GTK3 Menu Action To Check
          check_menu_item_set_active ((gpointer)view -> ogl_styles[BALL_AND_STICK], TRUE);
          set_style (view -> ogl_styles[BALL_AND_STICK], & view -> colorp[BALL_AND_STICK][0]);
#endif

      }
      break;
    case GDK_KEY_c:
      if (get_project_by_id(view -> proj) -> natomes)
      {
        if (state & GDK_CONTROL_MASK)
        {
          if (view -> anim -> last -> img -> selected[0] -> selected)
          {
            opengl_project_changed (view -> proj);
            selected_aspec = -1;
            is_selected = 1;
            view -> nth_copy = 1;
            view -> insert_coords = get_insertion_coordinates (view);
#ifdef GTK4
            copy_the_atoms (NULL, NULL, & cut_sel);
#else
            copy_the_atoms (NULL, & cut_sel);
#endif
            update_insert_combos ();
          }
          else
          {
            if (copied_object)
            {
              g_free (copied_object);
              copied_object = NULL;
            }
          }
        }
        else
        {
#ifdef GTK4
          set_style (NULL, & view -> colorp[CYLINDERS][0]);
#else
          // GTK3 Menu Action To Check
          check_menu_item_set_active ((gpointer)view -> ogl_styles[CYLINDERS], TRUE);
          set_style (view -> ogl_styles[CYLINDERS], & view -> colorp[CYLINDERS][0]);
#endif
        }
      }
      break;
    case GDK_KEY_d:
      if (get_project_by_id(view -> proj) -> natomes)
      {
#ifdef GTK4
          set_style (NULL, & view -> colorp[PUNT][0]);
#else
        // GTK3 Menu Action To Check
        check_menu_item_set_active ((gpointer)view -> ogl_styles[PUNT], TRUE);
        set_style (view -> ogl_styles[PUNT], & view -> colorp[PUNT][0]);
#endif
      }
      break;
    case GDK_KEY_e:
      if (get_project_by_id(view -> proj) -> natomes)
      {
        if (state & GDK_CONTROL_MASK)
        {
          coord_properties (NULL, & view -> colorp[30][0]);
        }
#ifdef GTK4
        else if (state & GDK_ALT_MASK)
#else
        else if (state & GDK_MOD1_MASK)
#endif
        {
          if (view -> mode == ANALYZE && get_project_by_id(view -> proj) -> steps == 1)
          {
#ifdef GTK4
            set_mode (NULL, & view -> colorp[1][0]);
#else
            // GTK3 Menu Action To Check
            check_menu_item_set_active ((gpointer)view -> ogl_mode[1], TRUE);
            set_mode (view -> ogl_mode[1], & view -> colorp[1][0]);
#endif
          }
        }
      }
      break;
    case GDK_KEY_f:
#ifdef GTK4
      if (state & GDK_CONTROL_MASK) set_full_screen (NULL, NULL, view);
#else
      if (state & GDK_CONTROL_MASK) set_full_screen (NULL, view);
#endif
      break;
    case GDK_KEY_l:
      if ((state & GDK_CONTROL_MASK) && get_project_by_id(view -> proj) -> natomes)
      {
        opengl_project_changed (view -> proj);
        selected_aspec = is_selected = -1;
        if (view -> labelled == opengl_project -> natomes*opengl_project -> steps)
        {
#ifdef GTK4
          label_unlabel_atoms (NULL, NULL, & cut_lab);
#else
          label_unlabel_atoms (NULL, & cut_lab);
#endif
        }
        else
        {
#ifdef GTK4
          label_unlabel_atoms (NULL, NULL, & cut_sel);
#else
          label_unlabel_atoms (NULL, & cut_sel);
#endif
        }
      }
      break;
    case GDK_KEY_m:
      if (get_project_by_id(view -> proj) -> natomes)
      {
        if (state & GDK_CONTROL_MASK)
        {
          view -> anim -> last -> img -> m_is_pressed = 0;
          window_measures (NULL, view);
        }
        else if (view -> anim -> last -> img -> m_is_pressed < 2)
        {
          view -> anim -> last -> img -> m_is_pressed ++;
        }
        else
        {
          view -> anim -> last -> img -> m_is_pressed = 0;
        }
        view -> create_shaders[MEASU] = TRUE;
        update (view);
      }
      break;
    case GDK_KEY_n:
      if (state & GDK_CONTROL_MASK) on_create_new_project (NULL, NULL);
    case GDK_KEY_p:
      if (get_project_by_id(view -> proj) -> natomes) change_color_map (view, 1);
      break;
    case GDK_KEY_r:
      if ((state & GDK_CONTROL_MASK) && get_project_by_id(view -> proj) -> natomes) window_recorder (NULL, (gpointer)view);
      break;
    case GDK_KEY_s:
      if (get_project_by_id(view -> proj) -> natomes)
      {
        if (state & GDK_CONTROL_MASK)
        {
          spin_stop (NULL, data);
        }
        else
        {
#ifdef GTK4
          set_style (NULL, & view -> colorp[SPHERES][0]);
#else
          // GTK3 Menu Action To Check
          check_menu_item_set_active ((gpointer)view -> ogl_styles[SPHERES], TRUE);
          set_style (view -> ogl_styles[SPHERES], & view -> colorp[SPHERES][0]);
#endif
        }
      }
      break;
    case GDK_KEY_v:
      if (state & GDK_CONTROL_MASK)
      {
        if (copied_object && ! is_atom_win_active(view) && get_project_by_id(view -> proj) -> steps == 1 && view -> mode == EDITION)
        {
          //i = (! get_project_by_id(view -> proj) -> natomes) ? 1 : 0;
          opengl_project_changed (view -> proj);
#ifdef GTK4
          add_object (NULL, NULL, & view -> colorp[0][0]);
#else
          add_object (NULL, & view -> colorp[0][0]);
#endif
          view -> nth_copy ++;
          /*if (i)
          {
            i = activep;
            active_project_changed (view -> proj);
            frag_update = (active_project -> natomes > ATOM_LIMIT) ? 0 : 1;
            mol_update = (frag_update) ? ((active_project -> steps > STEP_LIMIT) ? 0 : 1) : 0;
            bonds_update = 1;
            on_calc_bonds_released (NULL, NULL);
            active_project_changed (i);
          }*/
        }
      }
      else if (get_project_by_id(view -> proj) -> natomes)
      {
#ifdef GTK4
        set_style (NULL, & view -> colorp[OGL_STYLES][0]);
#else
        // GTK3 Menu Action To Check
        check_menu_item_set_active ((gpointer)view -> filled_styles[0], TRUE);
        set_style (view -> filled_styles[0], & view -> colorp[OGL_STYLES][0]);
#endif
      }
      break;
    case GDK_KEY_w:
      if (get_project_by_id(view -> proj) -> natomes)
      {
#ifdef GTK4
        set_style (NULL, & view -> colorp[WIREFRAME][0]);
#else
        // GTK3 Menu Action To Check
        check_menu_item_set_active ((gpointer)view -> ogl_styles[WIREFRAME], TRUE);
        set_style (view -> ogl_styles[WIREFRAME], & view -> colorp[WIREFRAME][0]);
#endif
      }
      break;
    case GDK_KEY_x:
      if ((state & GDK_CONTROL_MASK) && get_project_by_id(view -> proj) -> natomes)
      {
        if (! is_atom_win_active(view) && get_project_by_id(view -> proj) -> steps == 1 && view -> mode == EDITION)
        {
          if (view -> anim -> last -> img -> selected[0] -> selected)
          {
            opengl_project_changed (view -> proj);
            selected_aspec = -1;
            is_selected = 1;
#ifdef GTK4
            copy_the_atoms (NULL, NULL, & cut_sel);
            remove_the_atoms (NULL, NULL, & cut_sel);
#else
            copy_the_atoms (NULL, & cut_sel);
            remove_the_atoms (NULL, & cut_sel);
#endif
          }
        }
      }
      break;
    case GDK_KEY_Right:
      if (get_project_by_id(view -> proj) -> natomes) set_motion (view, 1, 1, 1, FALSE, state);
      break;
    case GDK_KEY_Left:
      if (get_project_by_id(view -> proj) -> natomes) set_motion (view, 1, -1, 3, FALSE, state);
      break;
    case GDK_KEY_Up:
      if (get_project_by_id(view -> proj) -> natomes) set_motion (view, 0, 1, 3, TRUE, state);
      break;
    case GDK_KEY_Down:
      if (get_project_by_id(view -> proj) -> natomes) set_motion (view, 0, -1, 1, TRUE, state);
      break;
  }
}

#ifdef GTK3
G_MODULE_EXPORT gboolean on_key_pressed (GtkWidget * widg, GdkEventKey * event, gpointer data)
{
  if (event -> type == GDK_KEY_PRESS)
  {
    glwin_key_pressed (event -> keyval, event -> state, data);
  }
  return FALSE;
}
#else
G_MODULE_EXPORT gboolean on_glwin_key_pressed (GtkEventControllerKey * self, guint keyval, guint keycode, GdkModifierType state, gpointer data)
{
  glwin_key_pressed (keyval, state, data);
  return TRUE;
}
#endif

G_MODULE_EXPORT void on_win_realize (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  int * pix = get_project_by_id(view -> proj) -> tmp_pixels;
  int shift = 0;
  if (GTK_IS_WIDGET(view -> menu_bar)) shift = gtk_widget_get_allocated_height (view -> menu_bar);
  if (pix[0] > 0 && pix[1] > 0)
  {
    resize_this_window (view -> win, pix[0], pix[1]+shift);
    pix[0] = pix[1] = -1;
  }
  else
  {
    resize_this_window (view -> win, 500, 500);
  }
}

gboolean create_3d_model (int p, gboolean load)
{
/*
 * Configure a OpenGL-capable context.
 */
  struct project * this_proj = get_project_by_id(p);
#ifndef GTKGLAREA
  if (! glXQueryExtension (GDK_DISPLAY_XDISPLAY (gdk_display_get_default ()), NULL, NULL))
  {
    g_warning ("Sorry OpenGL is not supported !");
    return FALSE;
  }
  else
#endif
  {
    this_proj -> modelgl = g_malloc0 (sizeof*this_proj -> modelgl);
    this_proj -> modelgl -> init = FALSE;
    this_proj -> modelgl -> proj = this_proj -> id;
    GtkWidget * gl_vbox;
    if (load)
    {
      gchar * str = g_strdup_printf ("%s - 3D view - [%s mode]", this_proj -> name, mode_name[0]);
      this_proj -> modelgl -> win = create_win (str, MainWindow, FALSE, TRUE);
      // this_proj -> modelgl -> accel_group = gtk_accel_group_new ();
      // gtk_window_add_accel_group (GTK_WINDOW (this_proj -> modelgl -> win), this_proj -> modelgl -> accel_group);
      g_free (str);
      gl_vbox = create_vbox (BSEP);
      add_container_child (CONTAINER_WIN, this_proj -> modelgl -> win, gl_vbox);
      this_proj -> modelgl -> menu_box = create_hbox (0);
      add_box_child_start (GTK_ORIENTATION_VERTICAL, gl_vbox, this_proj -> modelgl -> menu_box, FALSE, FALSE, 0);
#ifdef GTK3
      this_proj -> modelgl -> menu_bar = gtk_menu_bar_new ();
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, this_proj -> modelgl -> menu_box, this_proj -> modelgl -> menu_bar, TRUE, TRUE, 0);
#endif
    }
#ifdef GTKGLAREA
    this_proj -> modelgl -> plot = gtk_gl_area_new ();
#else
    this_proj -> modelgl -> plot = gtk_drawing_area_new ();
#endif
    gtk_widget_set_size_request (this_proj -> modelgl -> plot, 100, 100);
#ifndef GTKGLAREA
    gtk_widget_set_double_buffered (this_proj -> modelgl -> plot, FALSE);
    this_proj -> modelgl -> glcontext = NULL;
#endif
    gtk_widget_set_hexpand (this_proj -> modelgl -> plot, TRUE);
    gtk_widget_set_vexpand (this_proj -> modelgl -> plot, TRUE);
#ifdef GTKGLAREA
#ifdef GTK3
    gtk_widget_add_events (GTK_WIDGET (this_proj -> modelgl -> plot),
                           GDK_SCROLL_MASK |
                           GDK_BUTTON1_MOTION_MASK | GDK_BUTTON2_MOTION_MASK |
                           GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
#endif
#else
    gtk_widget_add_events (GTK_WIDGET (this_proj -> modelgl -> plot),
                           GDK_SMOOTH_SCROLL_MASK |
                           GDK_BUTTON1_MOTION_MASK | GDK_BUTTON2_MOTION_MASK |
                           GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
#endif
#ifdef GTK4
    if (load)
    {
      add_widget_gesture_and_key_action (this_proj -> modelgl -> win, NULL, NULL, NULL,
                                                                      NULL, NULL, NULL,
                                                                      "glwin-key-pressed", G_CALLBACK(on_glwin_key_pressed), this_proj -> modelgl,
                                                                      NULL, NULL, NULL,
                                                                      NULL, NULL, NULL);
      add_widget_gesture_and_key_action (this_proj -> modelgl -> plot, "glwin-button-pressed", G_CALLBACK(on_glwin_button_pressed), this_proj -> modelgl,
                                                                       "glwin-button-released", G_CALLBACK(on_glwin_button_released), this_proj -> modelgl,
                                                                       NULL, NULL, NULL,
                                                                       "glwin-pointer-motion", G_CALLBACK(on_glwin_pointer_motion), this_proj -> modelgl,
                                                                       "glwin-pointer-scroll", G_CALLBACK(on_glwin_pointer_scoll), this_proj -> modelgl);
    }
    else
    {
      add_widget_gesture_and_key_action (this_proj -> modelgl -> plot, "glwin-button-pressed", G_CALLBACK(on_lib_button_pressed), this_proj -> modelgl,
                                                                       "glwin-button-released", G_CALLBACK(on_lib_button_released), this_proj -> modelgl,
                                                                       NULL, NULL, NULL,
                                                                       "glwin-pointer-motion", G_CALLBACK(on_glwin_pointer_motion), this_proj -> modelgl,
                                                                       NULL, NULL, NULL);
    }
    // add_window_gesture_action (window, "opengl-context-click", "pressed", G_CALLBACK(on_pressed_gl), NULL);
#else
    g_signal_connect (G_OBJECT (this_proj -> modelgl -> plot), "motion-notify-event", G_CALLBACK(on_motion), this_proj -> modelgl);
    g_signal_connect (G_OBJECT (this_proj -> modelgl -> plot), "button-release-event", G_CALLBACK(on_glwin_button_event), this_proj -> modelgl);
    if (load)
    {
      g_signal_connect (G_OBJECT (this_proj -> modelgl -> plot), "button-press-event", G_CALLBACK(on_glwin_button_event), this_proj -> modelgl);
      g_signal_connect (G_OBJECT (this_proj -> modelgl -> plot), "scroll-event", G_CALLBACK(on_scrolled), this_proj -> modelgl);
    }
    else
    {
      g_signal_connect (G_OBJECT (this_proj -> modelgl -> plot), "button-press-event", G_CALLBACK(on_lib_pressed), this_proj -> modelgl);
    }
#endif
    if (load)
    {
      add_box_child_start (GTK_ORIENTATION_VERTICAL, gl_vbox, this_proj -> modelgl -> plot, FALSE, TRUE, 0);
#ifdef GTK3
      g_signal_connect (G_OBJECT (this_proj -> modelgl -> win), "key-press-event", G_CALLBACK(on_key_pressed), this_proj -> modelgl);
#endif
      g_signal_connect (G_OBJECT (this_proj -> modelgl -> win), "realize", G_CALLBACK(on_win_realize), this_proj -> modelgl);
      add_gtk_close_event (this_proj -> modelgl -> win, G_CALLBACK(hide_this_window), NULL);
    }
    g_signal_connect (G_OBJECT (this_proj -> modelgl -> plot), "realize", G_CALLBACK(on_realize), this_proj -> modelgl);
#ifdef GTKGLAREA
    g_signal_connect (G_OBJECT (this_proj -> modelgl -> plot), "render", G_CALLBACK(on_expose), this_proj -> modelgl);
#else
    g_signal_connect (G_OBJECT (this_proj -> modelgl -> plot), "draw", G_CALLBACK(on_expose), this_proj -> modelgl);
#endif
    return TRUE;
  }
}

void prep_model (int p)
{
  struct project * this_proj = get_project_by_id (p);
  gboolean adv_bonding[2];
  if (this_proj -> modelgl == NULL)
  {
    if (create_3d_model (p, TRUE))
    {
      /*GtkWidget * dummy = create_menu_item (FALSE, "Dummy");
      add_menu_child (this_proj -> modelgl -> menu_bar, dummy);
      show_the_widgets (this_proj -> modelgl -> win);
      destroy_this_widget (dummy);*/
      show_the_widgets (this_proj -> modelgl -> win);
      if (this_proj -> initgl)
      {
        active_project_changed (p);
#ifdef GTK3
        // GTK3 Menu Action To Check
        active_glwin -> ogl_box_axis[0] = g_malloc0 (OGL_BOX*sizeof*active_glwin -> ogl_box_axis[0]);
        active_glwin -> ogl_box_axis[1] = g_malloc0 (OGL_AXIS*sizeof*active_glwin -> ogl_box_axis[1]);
#endif
        prepare_opengl_menu_bar (active_glwin);
        if (reading_input)
        {
          adv_bonding[0] = (active_project -> natomes > ATOM_LIMIT) ? 0 : tmp_adv_bonding[0];
          adv_bonding[1] = (active_project -> steps > STEP_LIMIT) ? 0 : tmp_adv_bonding[1];
          frag_update = (active_project -> natomes > ATOM_LIMIT) ? 0 : 1;
          mol_update = (frag_update) ? ((active_project -> steps > STEP_LIMIT) ? 0 : 1) : 0;
        }
        else
        {
          frag_update = (force_mol) ? 1 : (active_project -> natomes > ATOM_LIMIT) ? 0 : 1;
          mol_update = (force_mol) ? 1 : (frag_update) ? ((active_project -> steps > STEP_LIMIT) ? 0 : 1) : 0;
          adv_bonding[0] = adv_bonding[1] = TRUE;
        }
        if (active_project -> natomes && adv_bonding[0] && adv_bonding[1])
        {
          bonds_update = 1;
          active_project -> runc[0] = FALSE;
          on_calc_bonds_released (NULL, NULL);
        }
      }
    }
  }
  else
  {
    if (gtk_widget_get_visible(this_proj -> modelgl -> win))
    {
      gtk_widget_hide (this_proj -> modelgl -> win);
    }
    else
    {
      gtk_widget_show (this_proj -> modelgl -> win);
    }
  }
}
