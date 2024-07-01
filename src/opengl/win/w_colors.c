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
* @file w_colors.c
* @short Functions to create the color selection dialogs
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'w_colors.c'
*
* Contains:
*

 - The functions to create the color selection dialogs

*
* List of functions:

  void window_color (project * this_proj, glwin * view);

  G_MODULE_EXPORT void run_window_color (GtkDialog * win, gint response_id, gpointer data);
  G_MODULE_EXPORT void to_run_back_color_window (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void to_run_back_color_window (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void to_run_box_color_window (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void to_run_box_color_window (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void to_run_atom_color_window (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void to_run_atom_color_window (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void run_window_color_coord (GtkDialog * win, gint response_id, gpointer data);
  G_MODULE_EXPORT void window_color_coord (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void window_color_coord (GtkWidget * widg, gpointer data);

*/

#include "global.h"
#include "interface.h"
#include "project.h"
#include "glwindow.h"
#include "glview.h"
#include "color_box.h"

/*
  The object type for color change
    -2    : background
    -1    : box
     > -1 : coordination
*/
int wc_cid;

/*!
  \fn G_MODULE_EXPORT void run_window_color (GtkDialog * win, gint response_id, gpointer data)

  \brief window color chooser - running the dialog

  \param win the GtkDialog sending the signal
  \param response_id the response id
  \param data the associated data pointer
*/
G_MODULE_EXPORT void run_window_color (GtkDialog * win, gint response_id, gpointer data)
{
  project * this_proj = (project *)data;

  if (response_id == GTK_RESPONSE_OK)
  {
    ColRGBA colo = get_window_color (GTK_WIDGET(win));
    if (wc_cid == -2)
    {
      this_proj -> modelgl -> anim -> last -> img -> backcolor = colo;
      this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
    }
    else if (wc_cid == -1)
    {
      this_proj -> modelgl -> anim -> last -> img -> box_color = colo;
      this_proj -> modelgl -> create_shaders[MDBOX] = TRUE;
    }
    else
    {
      this_proj -> modelgl -> anim -> last -> img -> at_color[wc_cid] = colo;
      int shaders[2] = {ATOMS, BONDS};
      re_create_md_shaders (2, shaders, this_proj);
      int shader[1] = {POLYS};
      if (this_proj -> modelgl ->  anim -> last -> img -> color_map[1] == 0) re_create_md_shaders (1, shader, this_proj);
    }
    update (this_proj -> modelgl);
  }
  destroy_this_dialog (win);
}

/*!
  \fn void window_color (project * this_proj, glwin * view)

  \brief window color chooser - creating the dialog

  \param this_proj the target project
  \param view the target glwin
*/
void window_color (project * this_proj, glwin * view)
{
  gchar * str;
  GdkRGBA col;
  if (wc_cid == -2)
  {
    str = g_strdup_printf ("Background color");
    col = colrgba_togtkrgba (view -> anim -> last -> img -> backcolor);
  }
  else if (wc_cid == -1)
  {
    str = g_strdup_printf ("Model box color");
    col = colrgba_togtkrgba (view -> anim -> last -> img -> box_color);
  }
  else
  {
    if (wc_cid < this_proj -> nspec)
    {
      str = g_strdup_printf ("%s - atom(s) color", this_proj -> chemistry -> label[wc_cid]);
    }
    else
    {
      str = g_strdup_printf ("%s* - clone(s) color", this_proj -> chemistry -> label[wc_cid-this_proj -> nspec]);
    }
    col = colrgba_togtkrgba (view -> anim -> last -> img -> at_color[wc_cid]);
  }
  GtkWidget * win = gtk_color_chooser_dialog_new (str, GTK_WINDOW(view -> win));
  gtk_window_set_modal (GTK_WINDOW(win), TRUE);
  gtk_color_chooser_set_use_alpha (GTK_COLOR_CHOOSER(win), TRUE);
  gtk_color_chooser_set_rgba (GTK_COLOR_CHOOSER(win), & col);
  g_free (str);
  run_this_gtk_dialog (win, G_CALLBACK(run_window_color), this_proj);
}

#ifdef GTK4
/*!
  \fn G_MODULE_EXPORT void to_run_back_color_window (GSimpleAction * action, GVariant * parameter, gpointer data)

  \brief to run background color selection window callback GTK4

  \param action the GAction sending the signal
  \param parameter GVariant parameter of the GAction, if any
  \param data the associated data pointer
*/
G_MODULE_EXPORT void to_run_back_color_window (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*!
  \fn G_MODULE_EXPORT void to_run_back_color_window (GtkWidget * widg, gpointer data)

  \brief to run background color selection window callback GTK3

  \param widg the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void to_run_back_color_window (GtkWidget * widg, gpointer data)
#endif
{
  glwin * view = (glwin *) data;
  wc_cid = -2;
  window_color (get_project_by_id(view -> proj), view);
  update (view);
}

#ifdef GTK4
/*!
  \fn G_MODULE_EXPORT void to_run_box_color_window (GSimpleAction * action, GVariant * parameter, gpointer data)

  \brief to run box color selection window callback GTK4

  \param action the GAction sending the signal
  \param parameter GVariant parameter of the GAction, if any
  \param data the associated data pointer
*/
G_MODULE_EXPORT void to_run_box_color_window (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*!
  \fn G_MODULE_EXPORT void to_run_box_color_window (GtkWidget * widg, gpointer data)

  \brief  to run box color selection window callback GTK3

  \param widg the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void to_run_box_color_window (GtkWidget * widg, gpointer data)
#endif
{
  glwin * view = (glwin *) data;
  wc_cid = -1;
  window_color (get_project_by_id(view -> proj), view);
  view -> create_shaders[MDBOX] = TRUE;
  update (view);
}

#ifdef GTK4
/*!
  \fn G_MODULE_EXPORT void to_run_atom_color_window (GSimpleAction * action, GVariant * parameter, gpointer data)

  \brief to run atom color selection window callback GTK4

  \param action the GAction sending the signal
  \param parameter GVariant parameter of the GAction, if any
  \param data the associated data pointer
*/
G_MODULE_EXPORT void to_run_atom_color_window (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*!
  \fn G_MODULE_EXPORT void to_run_atom_color_window (GtkWidget * widg, gpointer data)

  \brief to run atom color selection window callback GTK3

  \param widg the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void to_run_atom_color_window (GtkWidget * widg, gpointer data)
#endif
{
  tint * id = (tint *) data;
  // g_debug ("Atom color:: proj= %d, id -> b= %d, id -> c= %d", id -> a,  id -> b, id -> c);
  project * this_proj = get_project_by_id(id -> a);
  wc_cid = id -> c;
  window_color (this_proj, this_proj -> modelgl);
  int shaders[3] = {ATOMS, BONDS, SELEC};
  re_create_md_shaders (3, shaders, this_proj);
  this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  update (this_proj -> modelgl);
}

/*!
  \fn G_MODULE_EXPORT void run_window_color_coord (GtkDialog * win, gint response_id, gpointer data)

  \brief window to select a color - running the dialog

  \param win the GtkDialog sending the signal
  \param response_id the response id
  \param data the associated data pointer
*/
G_MODULE_EXPORT void run_window_color_coord (GtkDialog * win, gint response_id, gpointer data)
{
  qint * cid = (qint *)data;
  int c, g, s;
  project * this_proj = get_project_by_id(cid -> a);
  s = cid -> b;
  c = cid -> c;
  g = cid -> d;
  if (response_id == GTK_RESPONSE_OK)
  {
    if (g > 1) s = 0;
    this_proj -> modelgl -> anim -> last -> img -> spcolor[g][s][c] = get_window_color (GTK_WIDGET(win));
    int shaders[4] = {ATOMS, BONDS, POLYS, RINGS};
    re_create_md_shaders (4, shaders, this_proj);
    update (this_proj -> modelgl);
  }
  destroy_this_dialog (win);
}

#ifdef GTK4
/*!
  \fn G_MODULE_EXPORT void window_color_coord (GSimpleAction * action, GVariant * parameter, gpointer data)

  \brief create a window to select a color callback GTK4

  \param action the GAction sending the signal
  \param parameter GVariant parameter of the GAction, if any
  \param data the associated data pointer
*/
G_MODULE_EXPORT void window_color_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*!
  \fn G_MODULE_EXPORT void window_color_coord (GtkWidget * widg, gpointer data)

  \brief create a window to select a color callback GTK3

  \param widg the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void window_color_coord (GtkWidget * widg, gpointer data)
#endif
{
  qint * cid = (qint *)data;
  gchar * str;
  int c, g, s;
  project * this_proj = get_project_by_id(cid -> a);
  s = cid -> b;
  c = cid -> c;
  g = cid -> d;
  g_debug ("s= %d, c= %d, g= %d", s, c, g);
  switch (g)
  {
    case 0:
      str = g_strdup_printf ("%s atom(s) %d fold coordination sphere color", this_proj -> chemistry -> label[s],
      this_proj -> coord -> geolist[0][s][c]);
      break;
    case 1:
      str = g_strdup_printf ("%s - %s coordination sphere color", this_proj -> chemistry -> label[s],
                             prepare_for_title(exact_name(env_name (this_proj, c, s, 1, NULL))));
      break;
    case 2:
      str = g_strdup_printf ("Fragment N°%d color", c);
      g = s;
      s = 0;
      break;
    case 3:
      str = g_strdup_printf ("Molecule N°%d color", c);
      g = s;
      s = 0;
      break;
    case 9:
      str = g_strdup_printf ("%d atom chain(s) color", this_proj -> coord -> geolist[g][0][c]);
      s = 0;
      break;
    default:
      str = g_strdup_printf ("%s - %d atom ring(s) color", rings_type[s], this_proj -> coord -> geolist[g][0][c]);
      s = 0;
      break;
  }
  GtkWidget * win = gtk_color_chooser_dialog_new (str, GTK_WINDOW(this_proj -> modelgl -> win));
  g_free (str);
  set_color_chooser_color (win, this_proj -> modelgl -> anim -> last -> img -> spcolor[g][s][c]);
  gtk_color_chooser_set_use_alpha (GTK_COLOR_CHOOSER(win), TRUE);
  gtk_window_set_modal ((GtkWindow *)win, TRUE);
  run_this_gtk_dialog (win, G_CALLBACK(run_window_color_coord), data);
}
