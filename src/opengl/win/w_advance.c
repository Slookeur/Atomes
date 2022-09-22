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

#define TEMPLATES 7

extern void set_quality (int q, glwin * view);

gchar * material_template[TEMPLATES] = {"Opaque",
                                        "Brushed metal",
                                        "Shiny metal",
                                        "Plastic",
                                        "Transparent",
                                        "Translucent",
                                        "Diffuse"};

GLfloat template_parameters[TEMPLATES][5] ={{0.50, 0.50, 0.90, 1.00, 1.00},  // Ok
                                            {0.90, 0.60, 1.00, 1.50, 1.00},  // Ok
                                            {0.80, 0.40, 1.00, 1.00, 1.00},  // Ok
                                            {0.35, 0.15, 1.00, 1.50, 1.00},  // Ok
                                            {0.50, 0.50, 0.50, 1.00, 0.50},  //
                                            {0.50, 0.50, 0.50, 1.00, 0.75},  //
                                            {0.35, 0.80, 1.00, 1.50, 1.00}}; // Ok

float mat_min_max[5][2] = {{0.0, 1.0},
                           {0.0, 1.0},
                           {0.0, 10.0},
                           {0.0, 10.0},
                           {0.0, 1.0}};

gchar * settings[3][10] = {{"<u>Albedo:</u>",
                            "<u>Metallic:</u>",
                            "<u>Roughness:</u>",
                            "<u>Ambient occlusion:</u>",
                            "<u>Gamma correction:</u>",
                            "<u>Opacity:</u>"},
                           {"<u>Position:</u>",
                            "<u>Direction:</u>",
                            "<u>Intensity:</u>",
                            "<u>Constant attenuation:</u>",
                            "<u>Linear attenuation:</u>",
                            "<u>Quadratic attenuation:</u>",
                            "<u>Cone angle</u>",
                            "<u>Inner cutoff:</u>",
                            "<u>Outer cutoff:</u>"
                            "<u>Type:</u>"},
                            {"Color:"}};

gchar * lpos[3] = {"x", "y", "z"};
gchar * cpos[3] = {"r", "g", "b"};

GtkWidget * adv_box (GtkWidget * box, char * lab, int size, float xalign)
{
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box, hbox, TRUE, TRUE, 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label(lab, size, -1, xalign, 0.5), FALSE, FALSE, 5);
  return hbox;
}

GtkWidget * d_close;
int status;

#ifdef GTK4
G_MODULE_EXPORT void toggled_delete_ligth (GtkCheckButton * but, gpointer data)
{
  if (gtk_check_button_get_active (but))
#else
G_MODULE_EXPORT void toggled_delete_ligth (GtkToggleButton * but, gpointer data)
{
  if (gtk_toggle_button_get_active (but))
#endif
  {
    status --;
  }
  else
  {
    status ++;
  }
  if (status == 0)
  {
    widget_set_sensitive (d_close, 1);
  }
  else
  {
    widget_set_sensitive (d_close, 0);
  }
}

int * light_list;
GtkWidget ** light_but;

G_MODULE_EXPORT void run_light_source_to_be_removed (GtkDialog * win, gint response_id, gpointer data)
{
  image * img = (image *)data;
  int i, j;
  j = 0;
  for (i=0; i<img -> lights; i++)
  {
#ifdef GTK4
    if (gtk_check_button_get_active (GTK_CHECK_BUTTON(light_but[i])))
#else
    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(light_but[i])))
#endif
    {
      light_list[j] = i;
      j ++;
    }
  }
  destroy_this_dialog (win);
}

int * light_source_to_be_removed (int val, image * img, opengl_edition * ogl_edit)
{
  int i;
  gchar * str;
  status = val;
  GtkWidget * win = dialogmodal ("Remove light source(s)", GTK_WINDOW(ogl_edit -> win));
  GtkWidget * vbox = dialog_get_content_area (win);
  d_close =  gtk_dialog_get_widget_for_response (GTK_DIALOG (win), GTK_RESPONSE_CLOSE);
  widget_set_sensitive (d_close, 0);
  if (val > 1)
  {
    str = g_strdup_printf ("Please select the %d light sources to be removed: ", val);
  }
  else
  {
    str = g_strdup_printf ("Please select the %d light source to be removed: ", val);
  }
  bbox (vbox, str);
  g_free (str);
  light_but = g_malloc (img -> lights * sizeof*light_but);
  for (i=0; i<img -> lights; i++)
  {
    str = g_strdup_printf ("Light N°%d", i+1);
    light_but[i] = check_button (str, -1, 40, FALSE, G_CALLBACK(toggled_delete_ligth), (gpointer)GINT_TO_POINTER(i));
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, light_but[i], TRUE, TRUE, 0);
    g_free (str);
  }
  light_list = allocint(val);
  run_this_gtk_dialog (win, G_CALLBACK(run_light_source_to_be_removed), img);
  return light_list;
}

void print_light_source (Light source, int i)
{
  g_debug ("\n");
  g_debug ("Light N°%d", i);
  g_debug ("Type= %d", source.type);
  g_debug ("Pos_x= %f, Pos_y= %f, Pos_z= %f", source.position.x, source.position.y, source.position.z);
  g_debug ("Dir_x= %f, Dir_y= %f, Dir_z= %f", source.direction.x, source.direction.y, source.direction.z);
  g_debug ("Int_r= %f, Int_g= %f, Int_b= %f", source.intensity.x, source.intensity.y, source.intensity.z);
  g_debug ("Att_c= %f, Att_l= %f, Att_q= %f", source.attenuation.x, source.attenuation.y, source.attenuation.z);
  g_debug ("Spo_a= %f, Spo_i= %f, Spo_o= %f", source.spot_data.x, source.spot_data.y, source.spot_data.z);
  g_debug ("\n");
}

/*
  Light attenuation table (from Ogre3D):
  Distance(to object) 	Constant 	Linear 	Quadratic
        7                 1.0        0.7       1.8
       13                 1.0        0.35      0.44
       20                 1.0        0.22      0.20
       32                 1.0        0.14      0.07
       50                 1.0        0.09      0.032
       65                 1.0        0.07      0.017
      100                 1.0        0.045     0.0075
      160                 1.0        0.027     0.0028
      200                 1.0        0.022     0.0019
      325                 1.0        0.014     0.0007
      600                 1.0        0.007     0.0002
     3250                 1.0        0.0014    0.000007
*/

Light init_light_source (int type, float val, float vbl)
{
  Light new_light;
  new_light.type = type;
  new_light.fix = (type != 1) ? 0 : 1;
  new_light.show = 0;
  new_light.direction = vec3(0.0, 0.0, 0.0);
  double intensity = (type == 1) ? 100.0*DEFAULT_INTENSITY : DEFAULT_INTENSITY;
  // double intensity = DEFAULT_INTENSITY;
  if (val != vbl)
  {
    intensity *= exp (val/vbl);
  }
  if (vbl <= 50.0) intensity *= vbl / 100.0;

  new_light.intensity = vec3 (intensity, intensity, intensity);
  new_light.attenuation = vec3 (1.0, 0.14, 0.07);
  new_light.spot_data = vec3 (20.0, 20.0, 20.0);
  if (type == 0)
  {
    new_light.position  = vec3 (0.0, 0.0, 0.0);
    new_light.direction = vec3 (0.0, 0.0, -1.0);
  }
  else
  {
    new_light.position  = vec3 (vbl*1.5, 0.0, 0.0);
    if (type == 2)
    {
      new_light.intensity = v3_muls (new_light.intensity, 100.0);
      float tan = (val * sqrt(2.0) / 2.0) / (vbl - val);
      float tetha = fabs(atanf (tan)) * 90.0 / pi;
      new_light.spot_data = vec3 (tetha, tetha, tetha);
    }
  }
  return new_light;
}

Light copy_light_source (Light old_sp)
{
  Light new_sp;
  new_sp.type = old_sp.type;
  new_sp.fix = old_sp.fix;
  new_sp.show = old_sp.show;
  new_sp.position = old_sp.position;
  new_sp.direction = old_sp.direction;
  new_sp.intensity = old_sp.intensity;
  new_sp.attenuation = old_sp.attenuation;
  new_sp.spot_data = old_sp.spot_data;
  return new_sp;
}

Light * copy_light_sources (int dima, int dimb, Light * old_sp)
{
  int j;
  Light * new_sp = g_malloc (dima*sizeof * new_sp);
  for (j=0; j<dimb; j++)
  {
    //print_light_source (old_sp[j], j);
    new_sp[j] = copy_light_source (old_sp[j]);
    /*new_sp[j].type = old_sp[j].type;
    new_sp[j].fix = old_sp[j].fix;
    new_sp[j].show = old_sp[j].show;
    new_sp[j].position = old_sp[j].position;
    new_sp[j].direction = old_sp[j].direction;
    new_sp[j].intensity = old_sp[j].intensity;
    new_sp[j].attenuation = old_sp[j].attenuation;
    new_sp[j].spot_data = old_sp[j].spot_data;*/
    //print_light_source (new_sp[j], j);
  }
  return new_sp;
}

void show_active_light_data (opengl_edition * ogl_win, int li, int ti)
{
  Light * this_light = & get_project_by_id(ogl_win -> proj) -> modelgl -> anim -> last -> img -> l_ght[li];
  this_light -> type = ti;
  int i;
  for (i=0; i<2; i++)
  {
    if (is_the_widget_visible(ogl_win -> light_b_coord[i])) gtk_widget_hide (ogl_win -> light_b_coord[i]);
    if (is_the_widget_visible(ogl_win -> light_b_entry[i])) gtk_widget_hide (ogl_win -> light_b_entry[i]);
    if ((i == 0 && this_light -> type != 0) || (i == 1 && this_light -> type != 1))
    {
      gtk_widget_show (ogl_win -> light_b_coord[i]);
    }
  }
  if (this_light -> type > 0) gtk_widget_show (ogl_win -> light_b_entry[0]);
  if (this_light -> type > 1) gtk_widget_show (ogl_win -> light_b_entry[1]);

  if (is_the_widget_visible(ogl_win -> light_show)) gtk_widget_hide (ogl_win -> light_show);
  if (is_the_widget_visible(ogl_win -> advanced_light_box)) gtk_widget_hide (ogl_win -> advanced_light_box);
  widget_set_sensitive (ogl_win -> light_type, li);
  if (this_light -> type != 0)
  {
    gtk_widget_show (ogl_win -> advanced_light_box);
    gtk_widget_show (ogl_win -> light_show);
  }
}

void update_light_data (int li, opengl_edition * ogl_win)
{
  Light * this_light = & get_project_by_id(ogl_win -> proj) -> modelgl -> anim -> last -> img -> l_ght[li];
  gtk_combo_box_set_active (GTK_COMBO_BOX(ogl_win -> light_type), this_light -> type);
  gtk_combo_box_set_active (GTK_COMBO_BOX(ogl_win -> light_fix), this_light -> fix);
  show_active_light_data (ogl_win, li, this_light -> type);
  update_entry_double (GTK_ENTRY(ogl_win -> light_entry[0]), this_light -> attenuation.x);
  update_entry_double (GTK_ENTRY(ogl_win -> light_entry[1]), this_light -> attenuation.y);
  update_entry_double (GTK_ENTRY(ogl_win -> light_entry[2]), this_light -> attenuation.z);
  update_entry_double (GTK_ENTRY(ogl_win -> light_entry[3]), this_light -> spot_data.x);
  update_entry_double (GTK_ENTRY(ogl_win -> light_entry[4]), this_light -> spot_data.y);
  update_entry_double (GTK_ENTRY(ogl_win -> light_entry[5]), this_light -> spot_data.z);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[1][0]), this_light -> position.x);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[1][1]), this_light -> position.y);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[1][2]), this_light -> position.z);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[2][0]), this_light -> direction.x);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[2][1]), this_light -> direction.y);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[2][2]), this_light -> direction.z);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[3][0]), this_light -> intensity.x);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[3][1]), this_light -> intensity.y);
  update_entry_double (GTK_ENTRY(ogl_win -> entogl[3][2]), this_light -> intensity.z);
#ifdef GTK4
  gtk_check_button_set_active (GTK_CHECK_BUTTON(ogl_win -> light_show), this_light -> show);
#else
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(ogl_win -> light_show), this_light -> show);
#endif
}

G_MODULE_EXPORT void show_light_param (GtkComboBox * box, gpointer data)
{
  int li = gtk_combo_box_get_active (box);
  update_light_data (li, (opengl_edition *)data);
}

void create_lights_combo (image * this_image, opengl_edition * ogl_win)
{
  ogl_win -> lights = create_combo ();
  int i;
  gchar * str;
  for (i=0; i<this_image -> lights; i++)
  {
    str = g_strdup_printf ("Light N°%d", i+1);
    combo_text_append (ogl_win -> lights, str);
    g_free (str);
  }
  gtk_widget_set_size_request (ogl_win -> lights, 100, -1);
  g_signal_connect (G_OBJECT (ogl_win -> lights), "changed", G_CALLBACK(show_light_param), ogl_win);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, ogl_win -> lights_box, ogl_win -> lights, FALSE, FALSE, 10);
}

void add_remove_lights (int val, gpointer data)
{
  int i, j, k, m;
  glwin * view = (glwin *)data;
  struct project * this_proj = get_project_by_id(view -> proj);
  image * this_image = view -> anim -> last -> img;
  gboolean delete_ligth;
  i = this_image -> lights;
  Light * old_spots;
  if (val > i)
  {
#ifdef DEBUG
    g_debug ("ADDING_LIGHT_SOURCE:: val= %d, i= %d", val, i);
#endif
    // Adding light source(s)
    old_spots = copy_light_sources (i, i, this_image -> l_ght);
    g_free (this_image -> l_ght);
    this_image -> l_ght = copy_light_sources (val, i, old_spots);
    this_image -> lights = val;
    float pos = (this_proj -> cell.box[0].param[0][0] == 0.0) ? 20.0 : this_proj -> cell.box[0].param[0][0];
    for (j=i; j<val; j++)
    {
      this_image -> l_ght[j] = init_light_source (0, pos, this_proj -> modelgl -> p_moy); // Init directional by default
    }
    //free (old_spots);
  }
  else if (val < i)
  {
    // We need to remove a light
#ifdef DEBUG
    g_debug ("REMOVING_LIGHT_SOURCE:: val= %d, i= %d", val, i);
#endif
    int * ltr = light_source_to_be_removed (i-val, this_image, view -> opengl_win);
    if (ltr != NULL)
    {
      old_spots = copy_light_sources (i, i, this_image -> l_ght);
      for (k=0; k < i-val; k++)
      {
#ifdef DEBUG
        g_debug ("REMOVING_LIGHT_SOURCES:: k= %d, ltr[%d]= %d", k, k, ltr[k]);
#endif
      }
      g_free (this_image -> l_ght);
      this_image -> l_ght = g_malloc (val*sizeof*this_image -> l_ght);
      m = -1;
      for (j=0; j<i; j++)
      {
        delete_ligth = FALSE;
        for (k=0; k< i-val; k++)
        {
          if (j == ltr[k]) delete_ligth = TRUE;
        }
        if (! delete_ligth)
        {
          m ++;
          this_image -> l_ght[m] = copy_light_source (old_spots[j]);
        }
      }
      g_free (old_spots);
      this_image -> lights = val;
#ifdef DEBUG
      g_debug ("LIGHT(s) HAVE BEEN REMOVED:: NEW_LIGHTS_NUM= %d", val);
#endif
    }
  }
  //this_light = this_image -> l_ght;
  view -> create_shaders[LIGHT] = TRUE;
  view -> opengl_win -> lights = destroy_this_widget (view -> opengl_win -> lights);
  create_lights_combo (this_image, view -> opengl_win);
  gtk_widget_show (view -> opengl_win -> lights);
  gtk_combo_box_set_active (GTK_COMBO_BOX(view -> opengl_win -> lights), 0);
  update_light_data (0, view -> opengl_win);
  update (view);
}

G_MODULE_EXPORT void set_nlights_spin (GtkSpinButton * res, gpointer data)
{
  add_remove_lights (gtk_spin_button_get_value_as_int(res), data);
}

G_MODULE_EXPORT void set_nlights (GtkEntry * res, gpointer data)
{
  int i;
  const gchar * m;
  m = entry_get_text (res);
  i = (int) atof(m);
  add_remove_lights (i, data);
}

G_MODULE_EXPORT void update_light_param (GtkEntry * res, gpointer data)
{
  dint  * lid = (dint *)data;
  glwin * view = get_project_by_id(lid -> a) -> modelgl;
  if (view -> opengl_win)
  {
    int li = gtk_combo_box_get_active (GTK_COMBO_BOX(view -> opengl_win -> lights));
    Light * this_light = & view -> anim -> last -> img -> l_ght[li];
    const gchar * m = entry_get_text (res);
    double v = atof(m);
    switch (lid -> b)
    {
      case 0:
        this_light -> attenuation.x = v;
        break;
      case 1:
        this_light -> attenuation.y = v;
        break;
      case 2:
        this_light -> attenuation.z = v;
        break;
      case 3:
        this_light -> spot_data.x = v;
        break;
      case 4:
        this_light -> spot_data.y = v;
        break;
      case 5:
        this_light -> spot_data.z = v;
        break;
    }
    update_entry_double (res, v);
    if (this_light -> show) view -> create_shaders[LIGHT] = TRUE;
  }
  update (view);
}

void set_data_pos (vec3_t * vect, int pos, double v)
{
  switch (pos)
  {
    case 0:
      vect -> x = v;
      break;
    case 1:
      vect -> y = v;
      break;
    case 2:
      vect -> z = v;
      break;
  }
}

G_MODULE_EXPORT void set_object_pos (GtkEntry * res, gpointer data)
{
  tint * id = (tint *)data;
  const gchar * m = entry_get_text (res);
  double v = atof(m);
  glwin * view = get_project_by_id(id -> a) -> modelgl;
  image * this_image = view -> anim -> last -> img;
  if (id -> b == 0)
  {
    set_data_pos (& this_image -> m_terial.albedo, id -> c, v);
  }
  else if (id -> b == 4)
  {
    set_data_pos (& this_image -> f_g.color, id -> c, v);
  }
  else if (id -> b > 0 && view -> opengl_win)
  {
    int li = gtk_combo_box_get_active (GTK_COMBO_BOX(view -> opengl_win -> lights));
    switch (id -> b)
    {
      case 1:
        set_data_pos (& this_image -> l_ght[li].position, id -> c, v);
        break;
      case 2:
        set_data_pos (& this_image -> l_ght[li].direction, id -> c, v);
        break;
      case 3:
        set_data_pos (& this_image -> l_ght[li].intensity, id -> c, v);
        break;
    }
    if (this_image -> l_ght[li].show) view -> create_shaders[LIGHT] = TRUE;
  }
  if (view -> opengl_win) update_entry_double (res, v);
  update (view);
}

G_MODULE_EXPORT void set_light_type (GtkComboBox * box, gpointer data)
{
  opengl_edition * ogl_win = (opengl_edition *)data;
  int li = gtk_combo_box_get_active (GTK_COMBO_BOX(ogl_win -> lights));
  int ti = gtk_combo_box_get_active (box);
  show_active_light_data (ogl_win, li, ti);
}

G_MODULE_EXPORT void set_light_fix (GtkComboBox * box, gpointer data)
{
  opengl_edition * ogl_win = (opengl_edition *)data;
  glwin * view = get_project_by_id(ogl_win -> proj) -> modelgl;
  int li = gtk_combo_box_get_active (GTK_COMBO_BOX(ogl_win -> lights));
  view -> anim -> last -> img -> l_ght[li].fix = gtk_combo_box_get_active (box);
  view -> create_shaders[LIGHT] = TRUE;
  update (view);
}

#ifdef GTK4
G_MODULE_EXPORT void show_this_light (GtkCheckButton * but, gpointer data)
#else
G_MODULE_EXPORT void show_this_light (GtkToggleButton * but, gpointer data)
#endif
{
  opengl_edition * ogl_win = (opengl_edition *)data;
  glwin * view = get_project_by_id(ogl_win -> proj) -> modelgl;
  int li = gtk_combo_box_get_active (GTK_COMBO_BOX(ogl_win -> lights));
#ifdef GTK4
  view -> anim -> last -> img -> l_ght[li].show = gtk_check_button_get_active (but);
#else
  view -> anim -> last -> img -> l_ght[li].show = gtk_toggle_button_get_active (but);
#endif
  view -> create_shaders[LIGHT] = TRUE;
  update (view);
}

GtkWidget * create_setting_pos (int id, int kd, int jd, float * values, opengl_edition * ogl_win)
{
  int i;
  GtkWidget * setting_pos = create_vbox (5);
  if (kd != 0) abox (setting_pos, settings[kd][jd], 0);
  GtkWidget * box = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, setting_pos, box, FALSE, FALSE, 0);

  for (i=0; i<3; i++)
  {
    ogl_win -> pos_pointer[id][i].a = ogl_win -> proj;
    ogl_win -> pos_pointer[id][i].b = id;
    ogl_win -> pos_pointer[id][i].c = i;
    if (id > 0 && id < 3)
    {
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, gtk_label_new (lpos[i]), FALSE, FALSE, 20);
    }
    else
    {
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, gtk_label_new (cpos[i]), FALSE, FALSE, 20);
    }
    ogl_win -> entogl[id][i] = create_entry (G_CALLBACK(set_object_pos), 80, 10, FALSE, & ogl_win -> pos_pointer[id][i]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, ogl_win -> entogl[id][i], FALSE, FALSE, 0);
    update_entry_double (GTK_ENTRY(ogl_win -> entogl[id][i]), values[i]);
  }
  if (kd == 1 && id == 1)
  {
    ogl_win -> light_show = check_button ("Show light", -1, -1, FALSE, G_CALLBACK(show_this_light), ogl_win);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, ogl_win -> light_show, FALSE, FALSE, 15);
  }
  return setting_pos;
}

GtkWidget * lights_tab (glwin * view, opengl_edition * ogl_edit)
{
  int i, j, k;
  image * this_image = view -> anim -> last -> img;
  GtkWidget * vbox;
  GtkWidget * hbox, * lhbox;

  GtkWidget * layout = create_layout (-1, 300);
  vbox = add_vbox_to_layout (layout, 480, -1);

  GtkWidget * box = abox (vbox, "Number of light sources:\n(add or remove lights - up to 10 sources)", 10);
  gtk_widget_set_size_request (box, -1, 65);

  GtkWidget * nlights = spin_button (G_CALLBACK(set_nlights_spin), this_image -> lights, 1.0, 10.0, 1.0, 0, 100, view);
  gtk_widget_set_size_request (nlights, 25, -1);
  GtkWidget * fix = gtk_fixed_new ();
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, fix, FALSE, FALSE, 20);
  gtk_fixed_put (GTK_FIXED (fix), nlights, 0, 10);
  box = abox (vbox, "Configure light source:", 0);
  ogl_edit -> lights_box = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, ogl_edit -> lights_box, FALSE, FALSE, 10);
  create_lights_combo (this_image, ogl_edit);

  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, gtk_label_new ("Note: it is mandatory for light N°1 to be a directional light"), FALSE, FALSE, 10);

  box = abox (vbox, "Light configuration: ", 0);

  hbox = adv_box (vbox, settings[1][9], 170, 0.0);
  ogl_edit -> light_type = create_combo ();
  gchar * ltype[3] = {"Directional", "Point", "Spot"};
  for (i=0; i<3; i++)
  {
    combo_text_append (ogl_edit -> light_type, ltype[i]);
  }
  g_signal_connect (G_OBJECT (ogl_edit -> light_type), "changed", G_CALLBACK(set_light_type), ogl_edit);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, ogl_edit -> light_type, FALSE, FALSE, 10);

  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, gtk_label_new("Fixed by respect to: "), FALSE, FALSE, 10);
  ogl_edit -> light_fix = create_combo();
  gchar * lfix[2] = {"The viewer", "The model"};
  for (i=0; i<2; i++)
  {
    combo_text_append (ogl_edit -> light_fix, lfix[i]);
  }
  g_signal_connect (G_OBJECT (ogl_edit -> light_fix), "changed", G_CALLBACK(set_light_fix), ogl_edit);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, ogl_edit -> light_fix, FALSE, FALSE, 10);

  float values[3] = {0.0, 0.0, 0.0};
  // Direction
  ogl_edit -> light_b_coord[0] = create_setting_pos (1, 1, 0, values, ogl_edit);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, ogl_edit -> light_b_coord[0], FALSE, FALSE, 0);
  // Position
  ogl_edit -> light_b_coord[1] = create_setting_pos (2, 1, 1, values, ogl_edit);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, ogl_edit -> light_b_coord[1], FALSE, FALSE, 0);
  // Intensity
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, create_setting_pos (3, 1, 2, values, ogl_edit), FALSE, FALSE, 0);
  ogl_edit -> advanced_light_box = create_vbox (BSEP);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, ogl_edit -> advanced_light_box, FALSE, FALSE, 5);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ogl_edit -> advanced_light_box, markup_label("<b>Advanced parameters:</b>", -1, -1, 0.1, 0.65), FALSE, FALSE, 25);
  hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ogl_edit -> advanced_light_box, hbox, FALSE, FALSE, 0);
  k = 0;
  for (i=0; i<2; i++)
  {
    ogl_edit -> light_b_entry[i] = create_vbox (BSEP);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, ogl_edit -> light_b_entry[i], FALSE, FALSE, 0);
    for (j=0; j<3; j++)
    {
      ogl_edit -> light_entry[k] = create_entry (G_CALLBACK(update_light_param), 100, 15, FALSE, &ogl_edit -> pointer[k]);
      lhbox = adv_box (ogl_edit -> light_b_entry[i], settings[1][k+3], 170, 0.0);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, lhbox, ogl_edit -> light_entry[k], FALSE, FALSE, 10);
      if (i == 1) add_box_child_start (GTK_ORIENTATION_HORIZONTAL, lhbox, gtk_label_new("°"), FALSE, FALSE, 5);
      k ++;
    }
  }
  show_the_widgets (layout);
  gtk_combo_box_set_active (GTK_COMBO_BOX(ogl_edit -> lights), 0);
  return layout;
}

// ***************** MATERIAL ******************* //

#ifdef GTK4
G_MODULE_EXPORT void set_use_template_toggle (GtkCheckButton * but, gpointer data)
#else
G_MODULE_EXPORT void set_use_template_toggle (GtkToggleButton * but, gpointer data)
#endif
{
  int i, j, k;
  glwin * view = (glwin *)data;
  i = gtk_combo_box_get_active (GTK_COMBO_BOX(view -> opengl_win -> templates));
#ifdef GTK4
  j = gtk_check_button_get_active (but);
#else
  j = gtk_toggle_button_get_active (but);
#endif
  if (j)
  {
    if (i == -1) i = 3;
    for (k=0; k<5; k++) view -> anim -> last -> img -> m_terial.param[k+1] = template_parameters[i][k];
    gtk_combo_box_set_active (GTK_COMBO_BOX(view -> opengl_win -> templates), i);
    k = i+1;
  }
  else
  {
    k = 0;
  }
  view -> anim -> last -> img -> m_terial.predefine = k;
  widget_set_sensitive (view -> opengl_win -> templates, view -> anim -> last -> img -> m_terial.predefine);
  widget_set_sensitive (view -> opengl_win -> param_mat, ! view -> anim -> last -> img -> m_terial.predefine);
  update (view);
}

G_MODULE_EXPORT void set_template (GtkComboBox * box, gpointer data)
{
  int i, j;
  glwin * view = (glwin *)data;
  Material * mat = & view -> anim -> last -> img -> m_terial;
  i = gtk_combo_box_get_active (box);
  //gtk_range_set_value (GTK_RANGE(shiny), template_parameters[i][0]);
  for (j=0; j<5; j++)
  {
#ifdef DEBUG
    g_debug ("SET_TEMPLATES:: j= %d, val= %f", j, template_parameters[i][j]);
#endif
    //gtk_range_set_value (GTK_RANGE(base_ogl[0][j]), template_parameters[i][j+1]);
  }
  mat -> predefine = i + 1;
  for (j=0; j<5; j++) mat -> param[j+1] = template_parameters[i][j];
  update (view);
}

G_MODULE_EXPORT void set_l_model (GtkComboBox * box, gpointer data)
{
  glwin * view = (glwin *)data;
  view -> anim -> last -> img -> m_terial.param[0] = gtk_combo_box_get_active (box);
  update (view);
}

void param_has_changed (gpointer data, double val)
{
  dint * mid = (dint *)data;
  glwin * view = get_project_by_id(mid -> a) -> modelgl;
  Material * mat = & view -> anim -> last -> img -> m_terial;
  if (mat_min_max[mid -> b][0] >= 0.0 && val <= mat_min_max[mid -> b][1]) mat -> param[mid -> b + 1] = val;
  if (view -> opengl_win)
  {
    update_entry_double (GTK_ENTRY(view -> opengl_win -> m_entry[mid -> b]), mat -> param[mid -> b + 1]);
    gtk_range_set_value (GTK_RANGE(view -> opengl_win -> m_scale[mid -> b]), mat -> param[mid -> b + 1]);
  }
  update (view);
}

G_MODULE_EXPORT void update_mat_param (GtkEntry * res, gpointer data)
{
  const gchar * m = entry_get_text (res);
  double v = atof(m);
  param_has_changed (data, v);
}

G_MODULE_EXPORT gboolean scroll_scale_param (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  param_has_changed (data, value);
  return FALSE;
}

G_MODULE_EXPORT void scale_param (GtkRange * range, gpointer data)
{
  param_has_changed (data, gtk_range_get_value (range));
}

G_MODULE_EXPORT gboolean scroll_scale_quality (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  set_quality ((int)value, data);
#ifdef GTK4
  update_menu_bar ((glwin *)data);
#endif
  return FALSE;
}

G_MODULE_EXPORT void scale_quality (GtkRange * range, gpointer data)
{
  set_quality ((int)gtk_range_get_value (range), data);
#ifdef GTK4
  update_menu_bar ((glwin *)data);
#endif
}

GtkWidget * materials_tab (glwin * view, opengl_edition * ogl_edit)
{
  GtkWidget * layout = create_layout (-1, 300);
  GtkWidget * vbox = add_vbox_to_layout (layout, 650, -1);
  int i;
  Material * this_material = & view -> anim -> last -> img -> m_terial;

  GtkWidget * box = adv_box (vbox, "<b>Quality</b>: ", 100, 1.0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, create_hscale (2, 500, 1, view -> anim -> last -> img -> quality, GTK_POS_TOP, 1, 150,
                                                   G_CALLBACK(scale_quality), G_CALLBACK(scroll_scale_quality), view), FALSE, FALSE, 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, markup_label("<b>Lightning model</b>: ", 100, -1, 0.0, 0.5), FALSE, FALSE, 10);
  GtkWidget * lmodel = create_combo ();
  GtkWidget * fix = gtk_fixed_new ();
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, fix, FALSE, FALSE, 0);
  gtk_fixed_put (GTK_FIXED (fix), lmodel, 0, 10);

  char * l_model[6] = {"None", "Phong", "Blinn", "Cook-Torrance-Blinn", "Cook-Torrance-Beckmann", "Cook-Torrance-GCX"};
  for (i=0; i<6; i++)
  {
    combo_text_append (lmodel, l_model[i]);
  }
  g_signal_connect (G_OBJECT (lmodel), "changed", G_CALLBACK(set_l_model), view);
  gtk_widget_set_size_request (lmodel, 100, -1);
  gtk_combo_box_set_active (GTK_COMBO_BOX(lmodel), this_material -> param[0]);

  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, create_hsep (), FALSE, FALSE, 10);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox,
                       check_button ("<b>Use template</b>", 100, 40, this_material -> predefine, G_CALLBACK(set_use_template_toggle), view),
                       FALSE, FALSE, 0);
  box = abox (vbox, "<b>Templates</b>: ", 0);
  ogl_edit -> templates  = create_combo ();
  for (i=0; i<TEMPLATES; i++)
  {
    combo_text_append (ogl_edit -> templates, material_template[i]);
  }
  gtk_combo_box_set_active (GTK_COMBO_BOX(ogl_edit -> templates), this_material -> predefine-1);
  g_signal_connect (G_OBJECT (ogl_edit -> templates), "changed", G_CALLBACK(set_template), view);
  gtk_widget_set_size_request (ogl_edit -> templates, 100, -1);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, ogl_edit -> templates, FALSE, FALSE, 0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, create_hsep (), FALSE, FALSE, 10);
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 0);
  ogl_edit -> param_mat = create_vbox (BSEP);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, ogl_edit -> param_mat, FALSE, FALSE, 50);
#ifdef GTK4
  box = abox (ogl_edit -> param_mat, "Material properties: ", 0);
#else
  box = abox (ogl_edit -> param_mat, "Material properties: ", 5);
#endif
  GtkWidget * m_fixed;
  for (i=0; i<5; i++)
  {
    box = adv_box (ogl_edit -> param_mat, settings[0][i+1], 130, 0.0);
    ogl_edit -> m_scale[i] =  create_hscale (mat_min_max[i][0], mat_min_max[i][1], 0.001, this_material -> param[i+1],
                                             GTK_POS_TOP, 3, 200, G_CALLBACK(scale_param), G_CALLBACK(scroll_scale_param), & ogl_edit -> pointer[i]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, ogl_edit -> m_scale[i], FALSE, FALSE, 10);
    ogl_edit -> m_entry[i] = create_entry (G_CALLBACK(update_mat_param), 100, 15, FALSE, & ogl_edit -> pointer[i]);
    update_entry_double(GTK_ENTRY(ogl_edit -> m_entry[i]), this_material -> param[i+1]);
    m_fixed = gtk_fixed_new ();
    gtk_fixed_put (GTK_FIXED(m_fixed), ogl_edit -> m_entry[i], 0, 15);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, m_fixed, FALSE, FALSE, 15);
  }
  float values[] = {this_material -> albedo.x,
                    this_material -> albedo.y,
                    this_material -> albedo.z};

  adv_box (ogl_edit -> param_mat, settings[0][0], 130, 0.0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ogl_edit -> param_mat, create_setting_pos (0, 0, 0, values, ogl_edit), FALSE, FALSE, 5);
  show_the_widgets (layout);
  widget_set_sensitive (ogl_edit -> templates, this_material -> predefine);
  widget_set_sensitive (ogl_edit -> param_mat, ! this_material -> predefine);
  return layout;
}

// ***************** FOG ******************* //

void fog_param_changed (gpointer data, GLfloat u, GtkRange * range)
{
  dint * fid = (dint *)data;
  glwin * view = get_project_by_id (fid -> a) -> modelgl;
  Fog * this_fog = & view -> anim -> last -> img -> f_g;
  GLfloat v, w;
  if (fid -> b > 0)
  {
    v = this_fog -> depth[0];
    w = this_fog -> depth[1];
    if (fid -> b == 1 && u < w)
    {
      this_fog -> depth[0] = u;
    }
    else if (fid -> b == 2 && u > v)
    {
      this_fog -> depth[1] = u;
    }
    else if (view -> opengl_win)
    {
      if (fid -> b == 1)
      {
        gtk_range_set_value (range, (gdouble) (w-0.1));
      }
      else
      {
        gtk_range_set_value (range, (gdouble) (v+0.1));
      }
    }
  }
  else
  {
    this_fog -> density = u;
  }
    update (view);
}

G_MODULE_EXPORT gboolean scroll_set_fog_param (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  fog_param_changed (data, (GLfloat) value, range);
  return FALSE;
}

G_MODULE_EXPORT void set_fog_param (GtkRange * range, gpointer data)
{
  fog_param_changed (data, (GLfloat) gtk_range_get_value (range), range);
}

G_MODULE_EXPORT void set_fog_type (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  Fog * this_fog = & view -> anim -> last -> img -> f_g;
  this_fog -> based = gtk_combo_box_get_active (GTK_COMBO_BOX(widg));
  update (view);
}

void setup_fog_dialogs (glwin * view, int fid)
{
  Fog * this_fog = & view -> anim -> last -> img -> f_g;
  this_fog -> mode = fid;
  if (this_fog -> mode)
  {
    gtk_widget_show (view -> opengl_win -> param_fog);
    if (this_fog -> mode == 1)
    {
      gtk_widget_show (view -> opengl_win -> depth_box);
      gtk_widget_hide (view -> opengl_win -> dens_box);
    }
    else
    {
      gtk_widget_hide (view -> opengl_win -> depth_box);
      gtk_widget_show (view -> opengl_win -> dens_box);
    }
  }
  else
  {
    gtk_widget_hide (view -> opengl_win -> param_fog);
  }
}

G_MODULE_EXPORT void set_fog_mode (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  setup_fog_dialogs (view, gtk_combo_box_get_active (GTK_COMBO_BOX(widg)));
  update (view);
}

GtkWidget * fog_tab (glwin * view, opengl_edition * ogl_edit)
{
  Fog * this_fog = & view -> anim -> last -> img -> f_g;
  GtkWidget * layout = create_layout (480, -1);
  GtkWidget * vbox = add_vbox_to_layout (layout, 480, -1);

  GtkWidget * box = abox (vbox, "Select fog mode:", 10);
  GtkWidget * fogmod = create_combo ();
  combo_text_append (fogmod, "None");
  combo_text_append (fogmod, "Linear");
  combo_text_append (fogmod, "Exponential");
  combo_text_append (fogmod, "Exponential squared");
  gtk_widget_set_size_request (fogmod, 100, -1);
  gtk_combo_box_set_active (GTK_COMBO_BOX(fogmod), this_fog -> mode);
  g_signal_connect (G_OBJECT (fogmod), "changed", G_CALLBACK(set_fog_mode), view);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, fogmod, FALSE, FALSE, 0);

  ogl_edit -> param_fog = create_vbox (5);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, ogl_edit -> param_fog, FALSE, FALSE, 5);

  box = abox (ogl_edit -> param_fog, " Fog type: ", 0.0);
  GtkWidget * fogtype = create_combo ();
  combo_text_append (fogtype, "Plane based");
  combo_text_append (fogtype, "Range based");
  gtk_widget_set_size_request (fogtype, 100, -1);
  gtk_combo_box_set_active (GTK_COMBO_BOX(fogtype), this_fog -> based);
  g_signal_connect (G_OBJECT (fogtype), "changed", G_CALLBACK(set_fog_type), view);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, fogtype, FALSE, FALSE, 0);

  ogl_edit -> dens_box = abox (ogl_edit -> param_fog, " Fog density: ", 0.0);
  ogl_edit -> fog_range[0] = create_hscale (0.0, 1.0, 0.0001, this_fog -> density, GTK_POS_LEFT, 5,
                                            250, G_CALLBACK(set_fog_param), G_CALLBACK(scroll_set_fog_param), & ogl_edit -> pointer[0]);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, ogl_edit -> dens_box, ogl_edit -> fog_range[0], FALSE, FALSE, 0);

  char * depthfog[2] = {"\t depth<sup>*</sup> start: ", "\t depth<sup>*</sup> end: "};

  ogl_edit -> depth_box = create_vbox (5);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ogl_edit -> param_fog, ogl_edit -> depth_box, FALSE, FALSE, 0);
  box = abox (ogl_edit -> depth_box, " Fog depth: ", 0.0);
  int i;
  for (i=0; i<2; i++)
  {
    box = adv_box (ogl_edit -> depth_box, depthfog[i], 170, 0.0);
    ogl_edit -> fog_range[i+1] = create_hscale (0.0, 100.0, 0.1, this_fog -> depth[i], GTK_POS_LEFT, 2,
                                                250, G_CALLBACK(set_fog_param), G_CALLBACK(scroll_set_fog_param), & ogl_edit -> pointer[i+1]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, ogl_edit -> fog_range[i+1], FALSE, FALSE, 0);
  }
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ogl_edit -> depth_box, markup_label("* % of the OpenGL model depth.", -1, -1, 0.5, 0.5) , FALSE, FALSE, 5);

  float values[] = {this_fog -> color.x,
                    this_fog -> color.y,
                    this_fog -> color.z};
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ogl_edit -> param_fog, create_setting_pos (4, 2, 0, values, ogl_edit), FALSE, FALSE, 5);
  show_the_widgets (layout);
  return layout;
}

void close_advanced_opengl (gpointer data)
{
  glwin * view = (glwin *)data;
  view -> opengl_win -> win = destroy_this_widget (view -> opengl_win -> win);
  g_free (view -> opengl_win);
  view -> opengl_win = NULL;
}

#ifdef GTK4
G_MODULE_EXPORT gboolean close_advanced (GtkWidget * window, gpointer data)
#else
G_MODULE_EXPORT gboolean close_advanced (GtkWidget * widg, GdkEvent * event, gpointer data)
#endif
{
  close_advanced_opengl (data);
  return FALSE;
}

G_MODULE_EXPORT void opengl_advanced (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  if (view -> opengl_win == NULL)
  {
    view -> opengl_win = g_malloc0 (sizeof*view -> opengl_win);
    view -> opengl_win -> proj = view -> proj;
    int i;
    for (i=0; i<6; i++)
    {
      view -> opengl_win -> pointer[i].a = view -> proj;
      view -> opengl_win -> pointer[i].b = i;
    }
    gchar * str = g_strdup_printf ("OpenGL material aspect and light settings - %s", get_project_by_id(view -> proj) -> name);
    view -> opengl_win -> win = create_win (str, view -> win, FALSE, FALSE);
    g_free (str);
#ifdef DEBUG
    gtk_window_set_resizable (GTK_WINDOW (view -> opengl_win -> win), TRUE);
#endif
    GtkWidget * vbox = create_vbox (5);
    add_container_child (CONTAINER_WIN, view -> opengl_win -> win, vbox);
#ifdef GTK4
   gtk_widget_set_size_request (vbox, 650 , 650);
#else
    gtk_widget_set_size_request (vbox, 650 , 630);
#endif
    GtkWidget * notebook = gtk_notebook_new ();
    gtk_widget_show (notebook);
#ifdef GTK4
   gtk_widget_set_vexpand (notebook, TRUE);
#endif
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, notebook, TRUE, TRUE, 0);
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), materials_tab (view, view -> opengl_win), markup_label("<b>Material aspect</b>", -1, -1, 0.0, 0.5));
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), lights_tab (view, view -> opengl_win), markup_label("<b>Configure light sources</b>", -1, -1, 0.0, 0.5));
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), fog_tab (view, view -> opengl_win), markup_label("<b>Configure fog</b>", -1, -1, 0.0, 0.5));
    add_gtk_close_event (view -> opengl_win -> win, G_CALLBACK(close_advanced), view);
  }
  show_the_widgets (view -> opengl_win -> win);
  update_light_data (0, view -> opengl_win);
  setup_fog_dialogs (view, view -> anim -> last -> img -> f_g.mode);
}
