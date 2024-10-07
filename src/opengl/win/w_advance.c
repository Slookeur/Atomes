/* This file is part of the 'atomes' software

'atomes' is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

'atomes' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with 'atomes'.
If not, see <https://www.gnu.org/licenses/>

Copyright (C) 2022-2024 by CNRS and University of Strasbourg */

/*!
* @file w_advance.c
* @short Functions to create the OpenGL parameters edition window
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'w_advance.c'
*
* Contains:
*

 - The functions to create the OpenGL parameters edition window

*
* List of functions:

  int * light_source_to_be_removed (int val, image * img, opengl_edition * ogl_edit);

  G_MODULE_EXPORT gboolean scroll_scale_param (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data);
  G_MODULE_EXPORT gboolean scroll_scale_quality (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data);
  G_MODULE_EXPORT gboolean scroll_set_fog_param (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data);
  G_MODULE_EXPORT gboolean close_advanced (GtkWidget * window, gpointer data);
  G_MODULE_EXPORT gboolean close_advanced (GtkWidget * widg, GdkEvent * event, gpointer data);

  void print_light_source (Light source, int i);
  void show_active_light_data (opengl_edition * ogl_win, int lid, int tid);
  void update_light_data (int li, opengl_edition * ogl_win);
  void create_lights_combo (image * this_image, opengl_edition * ogl_win);
  void add_remove_lights (int val, gpointer data);
  void set_data_pos (vec3_t * vect, int pos, double v);
  void param_has_changed (gpointer data, double val);
  void fog_param_changed (gpointer data, GLfloat u, GtkRange * range);
  void setup_fog_dialogs (glwin * view, int fid);
  void close_advanced_opengl (gpointer data);

  G_MODULE_EXPORT void toggled_delete_ligth (GtkCheckButton * but, gpointer data);
  G_MODULE_EXPORT void toggled_delete_ligth (GtkToggleButton * but, gpointer data);
  G_MODULE_EXPORT void run_light_source_to_be_removed (GtkDialog * win, gint response_id, gpointer data);
  G_MODULE_EXPORT void show_light_param (GtkComboBox * box, gpointer data);
  G_MODULE_EXPORT void set_nlights_spin (GtkSpinButton * res, gpointer data);
  G_MODULE_EXPORT void set_nlights (GtkEntry * res, gpointer data);
  G_MODULE_EXPORT void update_light_param (GtkEntry * res, gpointer data);
  G_MODULE_EXPORT void set_object_pos (GtkEntry * res, gpointer data);
  G_MODULE_EXPORT void set_light_type (GtkComboBox * box, gpointer data);
  G_MODULE_EXPORT void set_light_fix (GtkComboBox * box, gpointer data);
  G_MODULE_EXPORT void show_this_light (GtkCheckButton * but, gpointer data);
  G_MODULE_EXPORT void show_this_light (GtkToggleButton * but, gpointer data);
  G_MODULE_EXPORT void set_use_template_toggle (GtkCheckButton * but, gpointer data);
  G_MODULE_EXPORT void set_use_template_toggle (GtkToggleButton * but, gpointer data);
  G_MODULE_EXPORT void set_template (GtkComboBox * box, gpointer data);
  G_MODULE_EXPORT void set_l_model (GtkComboBox * box, gpointer data);
  G_MODULE_EXPORT void update_mat_param (GtkEntry * res, gpointer data);
  G_MODULE_EXPORT void scale_param (GtkRange * range, gpointer data);
  G_MODULE_EXPORT void scale_quality (GtkRange * range, gpointer data);
  G_MODULE_EXPORT void set_fog_param (GtkRange * range, gpointer data);
  G_MODULE_EXPORT void set_fog_type (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void set_fog_mode (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void opengl_advanced (GtkWidget * widg, gpointer data);

  GtkWidget * adv_box (GtkWidget * box, char * lab, int size, float xalign);
  GtkWidget * create_setting_pos (int pid, int lid, float * values, opengl_edition * ogl_win);
  GtkWidget * lights_tab (glwin * view, opengl_edition * ogl_edit);
  GtkWidget * materials_tab (glwin * view, opengl_edition * ogl_edit);
  GtkWidget * fog_tab (glwin * view, opengl_edition * ogl_edit);

  Light init_light_source (int type, float val, float vbl);
  Light copy_light_source (Light old_sp);
  Light * copy_light_sources (int dima, int dimb, Light * old_sp);

*/

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

/*!
  \fn GtkWidget * adv_box (GtkWidget * box, char * lab, int size, float xalign)

  \brief create a box with markup label

  \param box the GtkWidget sending the signal
  \param lab label
  \param size size
  \param xalign x alignement
*/
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
/*!
  \fn G_MODULE_EXPORT void toggled_delete_ligth (GtkCheckButton * but, gpointer data)

  \brief toggle delete light callback GTK4

  \param but the GtkCheckButton sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void toggled_delete_ligth (GtkCheckButton * but, gpointer data)
{
  if (gtk_check_button_get_active (but))
#else
/*!
  \fn G_MODULE_EXPORT void toggled_delete_ligth (GtkToggleButton * but, gpointer data)

  \brief toggle delete light callback GTK3

  \param but the GtkToggleButton sending the signal
  \param data the associated data pointer
*/
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

/*!
  \fn G_MODULE_EXPORT void run_light_source_to_be_removed (GtkDialog * win, gint response_id, gpointer data)

  \brief remove light source(s) - running the dialog

  \param win the GtkDialog sending the signal
  \param response_id the response id
  \param data the associated data pointer
*/
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

/*!
  \fn int * light_source_to_be_removed (int val, image * img, opengl_edition * ogl_edit)

  \brief remove light source(s) - creating the dialog

  \param val number of light(s) to remove
  \param img the target image
  \param ogl_edit the target OpenGL edition window
*/
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

/*!
  \fn void print_light_source (Light source, int i)

  \brief print light source data

  \param source the light source
  \param i the light source id
*/
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

/*!
  \fn Light init_light_source (int type, float val, float vbl)

  \brief initialize a light source

  \param type the type of light
  \param val
  \param vbl
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

/*!
  \fn Light copy_light_source (Light old_sp)

  \brief create a copy of a light source

  \param old_sp the light source to copy
*/
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

/*!
  \fn Light * copy_light_sources (int dima, int dimb, Light * old_sp)

  \brief create a copy of a list of light sources

  \param dima new list size
  \param dimb old list size to duplicate
  \param old_sp old light sources
*/
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

/*!
  \fn void show_active_light_data (opengl_edition * ogl_win, int lid, int tid)

  \brief show active light data

  \param ogl_win the target OpenGL edition window
  \param lid the light id
  \param tid the light type
*/
void show_active_light_data (opengl_edition * ogl_win, int lid, int tid)
{
  Light * this_light = & get_project_by_id(ogl_win -> proj) -> modelgl -> anim -> last -> img -> l_ght[lid];
  this_light -> type = tid;
  int i;
  for (i=0; i<2; i++)
  {
    if (is_the_widget_visible(ogl_win -> light_b_coord[i])) hide_the_widgets (ogl_win -> light_b_coord[i]);
    if (is_the_widget_visible(ogl_win -> light_b_entry[i])) hide_the_widgets (ogl_win -> light_b_entry[i]);
    if ((i == 0 && this_light -> type != 0) || (i == 1 && this_light -> type != 1))
    {
      show_the_widgets (ogl_win -> light_b_coord[i]);
    }
  }
  if (this_light -> type > 0) show_the_widgets (ogl_win -> light_b_entry[0]);
  if (this_light -> type > 1) show_the_widgets (ogl_win -> light_b_entry[1]);

  if (is_the_widget_visible(ogl_win -> light_show)) hide_the_widgets (ogl_win -> light_show);
  if (is_the_widget_visible(ogl_win -> advanced_light_box)) hide_the_widgets (ogl_win -> advanced_light_box);
  widget_set_sensitive (ogl_win -> light_type, lid);
  if (this_light -> type != 0)
  {
    show_the_widgets (ogl_win -> advanced_light_box);
    show_the_widgets (ogl_win -> light_show);
  }
}

/*!
  \fn void update_light_data (int li, opengl_edition * ogl_win)

  \brief update light data

  \param li the light id
  \param ogl_win the target OpenGL edition window
*/
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

/*!
  \fn G_MODULE_EXPORT void show_light_param (GtkComboBox * box, gpointer data)

  \brief update light parameters based on light id in combo box

  \param box the GtkComboBox sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void show_light_param (GtkComboBox * box, gpointer data)
{
  int li = gtk_combo_box_get_active (box);
  update_light_data (li, (opengl_edition *)data);
}

/*!
  \fn void create_lights_combo (image * this_image, opengl_edition * ogl_win)

  \brief create light combo box

  \param this_image the target image parameters
  \param ogl_win the target OpenGL edition window
*/
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

/*!
  \fn void add_remove_lights (int val, gpointer data)

  \brief add or remove lights

  \param val total number of light(s)
  \param data the associated data pointer
*/
void add_remove_lights (int val, gpointer data)
{
  int i, j, k, m;
  glwin * view = (glwin *)data;
  project * this_proj = get_project_by_id(view -> proj);
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
  show_the_widgets (view -> opengl_win -> lights);
  gtk_combo_box_set_active (GTK_COMBO_BOX(view -> opengl_win -> lights), 0);
  update_light_data (0, view -> opengl_win);
  update (view);
}

/*!
  \fn G_MODULE_EXPORT void set_nlights_spin (GtkSpinButton * res, gpointer data)

  \brief  change the number of light(s) - spin button

  \param res the GtkSpinButton sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_nlights_spin (GtkSpinButton * res, gpointer data)
{
  add_remove_lights (gtk_spin_button_get_value_as_int(res), data);
}

/*!
  \fn G_MODULE_EXPORT void set_nlights (GtkEntry * res, gpointer data)

  \brief change the number of light(s) - entry

  \param res the GtkEntry sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_nlights (GtkEntry * res, gpointer data)
{
  int i;
  const gchar * m;
  m = entry_get_text (res);
  i = (int) string_to_double ((gpointer)m);
  add_remove_lights (i, data);
}

/*!
  \fn G_MODULE_EXPORT void update_light_param (GtkEntry * res, gpointer data)

  \brief update light parameter

  \param res the GtkEntry sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void update_light_param (GtkEntry * res, gpointer data)
{
  dint  * lid = (dint *)data;
  glwin * view = get_project_by_id(lid -> a) -> modelgl;
  if (view -> opengl_win)
  {
    int li = gtk_combo_box_get_active (GTK_COMBO_BOX(view -> opengl_win -> lights));
    Light * this_light = & view -> anim -> last -> img -> l_ght[li];
    const gchar * m = entry_get_text (res);
    double v = string_to_double ((gpointer)m);
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

/*!
  \fn void set_data_pos (vec3_t * vect, int pos, double v)

  \brief modify a vector component

  \param vect vector to adjust
  \param pos position to adjust
  \param v new value
*/
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

/*!
  \fn G_MODULE_EXPORT void set_object_pos (GtkEntry * res, gpointer data)

  \brief set object position

  \param res the GtkEntry sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_object_pos (GtkEntry * res, gpointer data)
{
  tint * id = (tint *)data;
  const gchar * m = entry_get_text (res);
  double v = string_to_double ((gpointer)m);
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

/*!
  \fn G_MODULE_EXPORT void set_light_type (GtkComboBox * box, gpointer data)

  \brief set light type callback

  \param box the GtkComboBox sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_light_type (GtkComboBox * box, gpointer data)
{
  opengl_edition * ogl_win = (opengl_edition *)data;
  int li = gtk_combo_box_get_active (GTK_COMBO_BOX(ogl_win -> lights));
  int ti = gtk_combo_box_get_active (box);
  show_active_light_data (ogl_win, li, ti);
}

/*!
  \fn G_MODULE_EXPORT void set_light_fix (GtkComboBox * box, gpointer data)

  \brief set light fix callback

  \param box the GtkComboBox sending the signal
  \param data the associated data pointer
*/
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
/*!
  \fn G_MODULE_EXPORT void show_this_light (GtkCheckButton * but, gpointer data)

  \brief show / hide this light callback GTK4

  \param but the GtkCheckButton sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void show_this_light (GtkCheckButton * but, gpointer data)
#else
/*!
  \fn G_MODULE_EXPORT void show_this_light (GtkToggleButton * but, gpointer data)

  \brief show / hide this light callback GTK3

  \param but the GtkToggleButton sending the signal
  \param data the associated data pointer
*/
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

/*!
  \fn GtkWidget * create_setting_pos (int pid, int lid, float * values, opengl_edition * ogl_win)

  \brief create OpenGL setting entries table

  \param pid parameter id (0 = material, 1 = light direction, 2 = light position, 3 = light intensity, 4 = fog)
  \param lid parameter label id
  \param values target parameter values
  \param ogl_win the target OpenGL edition window
*/
GtkWidget * create_setting_pos (int pid, int lid, float * values, opengl_edition * ogl_win)
{
  int i;
  GtkWidget * setting_pos = create_vbox (5);
  if (pid != 0) abox (setting_pos, settings[(pid < 4) ? 1 : 2][lid], 0);
  GtkWidget * box = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, setting_pos, box, FALSE, FALSE, 0);

  for (i=0; i<3; i++)
  {
    ogl_win -> pos_pointer[pid][i].a = ogl_win -> proj;
    ogl_win -> pos_pointer[pid][i].b = pid;
    ogl_win -> pos_pointer[pid][i].c = i;
    if (pid > 0 && pid < 3)
    {
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, gtk_label_new (lpos[i]), FALSE, FALSE, 20);
    }
    else
    {
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, gtk_label_new (cpos[i]), FALSE, FALSE, 20);
    }
    ogl_win -> entogl[pid][i] = create_entry (G_CALLBACK(set_object_pos), 80, 10, FALSE, & ogl_win -> pos_pointer[pid][i]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, ogl_win -> entogl[pid][i], FALSE, FALSE, 0);
    update_entry_double (GTK_ENTRY(ogl_win -> entogl[pid][i]), values[i]);
  }
  if (pid == 1)
  {
    ogl_win -> light_show = check_button ("Show light", -1, -1, FALSE, G_CALLBACK(show_this_light), ogl_win);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, ogl_win -> light_show, FALSE, FALSE, 15);
  }
  return setting_pos;
}

/*!
  \fn GtkWidget * lights_tab (glwin * view, opengl_edition * ogl_edit)

  \brief OpenGL light(s) parameters tab

  \param view the target glwin
  \param ogl_edit the target OpenGL edition window
*/
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
  ogl_edit -> light_b_coord[0] = create_setting_pos (1, 0, values, ogl_edit);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, ogl_edit -> light_b_coord[0], FALSE, FALSE, 0);
  // Position
  ogl_edit -> light_b_coord[1] = create_setting_pos (2, 1, values, ogl_edit);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, ogl_edit -> light_b_coord[1], FALSE, FALSE, 0);
  // Intensity
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, create_setting_pos (3, 2, values, ogl_edit), FALSE, FALSE, 0);
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
/*!
  \fn G_MODULE_EXPORT void set_use_template_toggle (GtkCheckButton * but, gpointer data)

  \brief use or not OpenGL material template callback GTK4

  \param but the GtkCheckButton sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_use_template_toggle (GtkCheckButton * but, gpointer data)
#else
/*!
  \fn G_MODULE_EXPORT void set_use_template_toggle (GtkToggleButton * but, gpointer data)

  \brief use or not OpenGL material template callback GTK3

  \param but the GtkToggleButton sending the signal
  \param data the associated data pointer
*/
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

/*!
  \fn G_MODULE_EXPORT void set_template (GtkComboBox * box, gpointer data)

  \brief change the OpenGL material template

  \param box the GtkComboBox sending the signal
  \param data the associated data pointer
*/
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

/*!
  \fn G_MODULE_EXPORT void set_l_model (GtkComboBox * box, gpointer data)

  \brief change OpenGL lightning model

  \param box the GtkComboBox sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_l_model (GtkComboBox * box, gpointer data)
{
  glwin * view = (glwin *)data;
  view -> anim -> last -> img -> m_terial.param[0] = gtk_combo_box_get_active (box);
  update (view);
}

/*!
  \fn void param_has_changed (gpointer data, double val)

  \brief update OpenGL material parameter

  \param data the associated data pointer
  \param val the new value
*/
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

/*!
  \fn G_MODULE_EXPORT void update_mat_param (GtkEntry * res, gpointer data)

  \brief update OpenGL material parameter - entry

  \param res the GtkEntry sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void update_mat_param (GtkEntry * res, gpointer data)
{
  const gchar * m = entry_get_text (res);
  double v = string_to_double ((gpointer)m);
  param_has_changed (data, v);
}

/*!
  \fn G_MODULE_EXPORT gboolean scroll_scale_param (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)

  \brief update OpenGL material parameter - scroll callback

  \param range the GtkRange sending the signal
  \param scroll the associated scroll type
  \param value the range value
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean scroll_scale_param (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  param_has_changed (data, value);
  return FALSE;
}

/*!
  \fn G_MODULE_EXPORT void scale_param (GtkRange * range, gpointer data)

  \brief update OpenGL material parameter - range callback

  \param range the GtkRange sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void scale_param (GtkRange * range, gpointer data)
{
  param_has_changed (data, gtk_range_get_value (range));
}

/*!
  \fn G_MODULE_EXPORT gboolean scroll_scale_quality (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)

  \brief update OpenGL quality - scroll callback

  \param range the GtkRange sending the signal
  \param scroll the associated scroll type
  \param value the range value
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean scroll_scale_quality (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  set_quality ((int)value, data);
#ifdef GTK4
  update_menu_bar ((glwin *)data);
#endif
  return FALSE;
}

/*!
  \fn G_MODULE_EXPORT void scale_quality (GtkRange * range, gpointer data)

  \brief update OpenGL quality - range callback

  \param range the GtkRange sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void scale_quality (GtkRange * range, gpointer data)
{
  set_quality ((int)gtk_range_get_value (range), data);
#ifdef GTK4
  update_menu_bar ((glwin *)data);
#endif
}

/*!
  \fn GtkWidget * materials_tab (glwin * view, opengl_edition * ogl_edit)

  \brief OpenGL material parameters tab

  \param view the target glwin
  \param ogl_edit the target OpenGL edition window
*/
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

  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, gtk_separator_new (GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 10);
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
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, gtk_separator_new (GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 10);
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
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ogl_edit -> param_mat, create_setting_pos (0, 0, values, ogl_edit), FALSE, FALSE, 5);
  show_the_widgets (layout);
  widget_set_sensitive (ogl_edit -> templates, this_material -> predefine);
  widget_set_sensitive (ogl_edit -> param_mat, ! this_material -> predefine);
  return layout;
}

// ***************** FOG ******************* //

/*!
  \fn void fog_param_changed (gpointer data, GLfloat u, GtkRange * range)

  \brief update OpenGL fog parameter

  \param data the associated data pointer
  \param u the new value
  \param range the GtkRange to udapte if needed
*/
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

/*!
  \fn G_MODULE_EXPORT gboolean scroll_set_fog_param (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)

  \brief update OpenGL fog parameter - scroll callback

  \param range the GtkRange sending the signal
  \param scroll the associated scroll type
  \param value the range value
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean scroll_set_fog_param (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  fog_param_changed (data, (GLfloat) value, range);
  return FALSE;
}

/*!
  \fn G_MODULE_EXPORT void set_fog_param (GtkRange * range, gpointer data)

  \brief update OpenGL fog parameter - range callback

  \param range the GtkRange sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_fog_param (GtkRange * range, gpointer data)
{
  fog_param_changed (data, (GLfloat) gtk_range_get_value (range), range);
}

/*!
  \fn G_MODULE_EXPORT void set_fog_type (GtkWidget * widg, gpointer data)

  \brief set OpenGL fog type

  \param widg the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_fog_type (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  Fog * this_fog = & view -> anim -> last -> img -> f_g;
  this_fog -> based = gtk_combo_box_get_active (GTK_COMBO_BOX(widg));
  update (view);
}

/*!
  \fn void setup_fog_dialogs (glwin * view, int fid)

  \brief update OpenGL fog tab based of fog type

  \param view the target glwin
  \param fid the fog mode
*/
void setup_fog_dialogs (glwin * view, int fid)
{
  Fog * this_fog = & view -> anim -> last -> img -> f_g;
  this_fog -> mode = fid;
  if (this_fog -> mode)
  {
    show_the_widgets (view -> opengl_win -> param_fog);
    if (this_fog -> mode == 1)
    {
      show_the_widgets (view -> opengl_win -> depth_box);
      hide_the_widgets (view -> opengl_win -> dens_box);
    }
    else
    {
      hide_the_widgets (view -> opengl_win -> depth_box);
      show_the_widgets (view -> opengl_win -> dens_box);
    }
  }
  else
  {
    hide_the_widgets (view -> opengl_win -> param_fog);
  }
}

/*!
  \fn G_MODULE_EXPORT void set_fog_mode (GtkWidget * widg, gpointer data)

  \brief set fog mode callback

  \param widg the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_fog_mode (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  setup_fog_dialogs (view, gtk_combo_box_get_active (GTK_COMBO_BOX(widg)));
  update (view);
}

/*!
  \fn GtkWidget * fog_tab (glwin * view, opengl_edition * ogl_edit)

  \brief OpenGL fog parameters tab

  \param view the target glwin
  \param ogl_edit the target OpenGL edition window
*/
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
  add_box_child_start (GTK_ORIENTATION_VERTICAL, ogl_edit -> param_fog, create_setting_pos (4, 0, values, ogl_edit), FALSE, FALSE, 5);
  show_the_widgets (layout);
  return layout;
}

/*!
  \fn void close_advanced_opengl (gpointer data)

  \brief close OpenGL rendering window free data

  \param data the associated data pointer
*/
void close_advanced_opengl (gpointer data)
{
  glwin * view = (glwin *)data;
  view -> opengl_win -> win = destroy_this_widget (view -> opengl_win -> win);
  g_free (view -> opengl_win);
  view -> opengl_win = NULL;
}

#ifdef GTK4
/*!
  \fn G_MODULE_EXPORT gboolean close_advanced (GtkWidget * window, gpointer data)

  \brief close OpenGL rendering advanced window callback GTK4

  \param window the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean close_advanced (GtkWidget * window, gpointer data)
#else
/*!
  \fn G_MODULE_EXPORT gboolean close_advanced (GtkWidget * widg, GdkEvent * event, gpointer data)

  \brief close OpenGL rendering advanced window callback GTK3

  \param widg the GtkWidget sending the signal
  \param event the GdkEvent triggering the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean close_advanced (GtkWidget * widg, GdkEvent * event, gpointer data)
#endif
{
  close_advanced_opengl (data);
  return FALSE;
}

/*!
  \fn G_MODULE_EXPORT void opengl_advanced (GtkWidget * widg, gpointer data)

  \brief create OpenGL rendering advanced window

  \param widg the GtkWidget sending the signal
  \param data the associated data pointer
*/
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
    show_the_widgets (notebook);
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
