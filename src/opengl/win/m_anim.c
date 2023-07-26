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
* This file: 'm_anim.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  G_MODULE_EXPORT void to_spin (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void to_seq (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void to_rec (GSimpleAction * action, GVariant * parameter, gpointer data);

  GtkWidget * menu_anim (glwin * view, int id);

  GMenu * menu_anim (glwin * view, int popm);

*/

#include "global.h"
#include "glwindow.h"

extern G_MODULE_EXPORT void window_spinner (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void window_recorder (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void window_sequencer (GtkWidget * widg, gpointer data);

#ifdef GTK3
/*
*  GtkWidget * menu_anim (glwin * view, int id)
*
*  Usage:
*
*  glwin * view :
*  int id       :
*/
GtkWidget * menu_anim (glwin * view, int id)
{
  GtkWidget * menuanim = create_menu_item (FALSE, "Animate");
  GtkWidget * menua = gtk_menu_new ();
  gtk_menu_item_set_submenu ((GtkMenuItem *)menuanim, menua);
  if (id == 0)
  {
    view -> ogl_anim[0] = gtk3_menu_item (menua, "Spin", IMG_STOCK, (gpointer)MEDIA_LOOP, G_CALLBACK(window_spinner), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
  }
  else
  {
    gtk3_menu_item (menua, "Spin", IMG_STOCK, (gpointer)MEDIA_LOOP, G_CALLBACK(window_spinner), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
  }
  GtkWidget * item = gtk3_menu_item (menua, "Sequencer", IMG_STOCK, (gpointer)MEDIA_PLAY, G_CALLBACK(window_sequencer), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
  if (get_project_by_id(view -> proj) -> steps == 1)
  {
    widget_set_sensitive (item, 0);
  }
  if (id == 0)
  {
    view -> ogl_anim[1] = gtk3_menu_item (menua, "Recorder", IMG_STOCK, (gpointer)RECORD, G_CALLBACK(window_recorder), (gpointer)view, TRUE, GDK_KEY_r, GDK_CONTROL_MASK, FALSE, FALSE, FALSE);
  }
  else
  {
    gtk3_menu_item (menua, "Recorder", IMG_STOCK, (gpointer)RECORD, G_CALLBACK(window_recorder), (gpointer)view, TRUE, GDK_KEY_r, GDK_CONTROL_MASK, FALSE, FALSE, FALSE);
  }
  widget_set_sensitive (menuanim, get_project_by_id(view -> proj) -> natomes);
  return (menuanim);
}
#else
/*
*  G_MODULE_EXPORT void to_spin (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage:
*
*  GSimpleAction * action :
*  GVariant * parameter   :
*  gpointer data          :
*/
G_MODULE_EXPORT void to_spin (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  window_spinner (NULL, data);
}

/*
*  G_MODULE_EXPORT void to_seq (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage:
*
*  GSimpleAction * action :
*  GVariant * parameter   :
*  gpointer data          :
*/
G_MODULE_EXPORT void to_seq (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  window_sequencer (NULL, data);
}

/*
*  G_MODULE_EXPORT void to_rec (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage:
*
*  GSimpleAction * action :
*  GVariant * parameter   :
*  gpointer data          :
*/
G_MODULE_EXPORT void to_rec (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  window_recorder (NULL, data);
}

/*
*  GMenu * menu_anim (glwin * view, int popm)
*
*  Usage:
*
*  glwin * view :
*  int popm     :
*/
GMenu * menu_anim (glwin * view, int popm)
{
  GMenu * menu = g_menu_new ();
  append_opengl_item (view, menu, "Spin", "spin", popm, popm, NULL, IMG_STOCK, (gpointer)MEDIA_LOOP, FALSE, G_CALLBACK(to_spin), (gpointer)view, FALSE, FALSE, FALSE, TRUE);
  append_opengl_item (view, menu, "Sequencer", "seq", popm, popm, NULL, IMG_STOCK, (gpointer)MEDIA_PLAY, FALSE, G_CALLBACK(to_seq), (gpointer)view, FALSE, FALSE, FALSE, TRUE);
  append_opengl_item (view, menu, "Recorder", "rec", popm, popm, "<CTRL>R", IMG_STOCK, (gpointer)RECORD, FALSE, G_CALLBACK(to_rec), (gpointer)view, FALSE, FALSE, FALSE, TRUE);

  return menu;
}
#endif
