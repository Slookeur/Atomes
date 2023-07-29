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
* This file: 'm_proj.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  G_MODULE_EXPORT void set_camera_pos (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void to_set_camera_pos (GSimpleAction * action, GVariant * parameter, gpointer data);

  GtkWidget * menu_proj (glwin * view);

  GMenu * menu_proj (glwin * view, int popm);

*/

#include "global.h"
#include "glview.h"
#include "glwindow.h"

enum position {
  RIGHT = 0,
  LEFT = 1,
  TOP = 2,
  BOTTOM = 3,
  FRONT = 4,
  BACK = 5,
};

/*
*  G_MODULE_EXPORT void set_camera_pos (GtkWidget * widg, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void set_camera_pos (GtkWidget * widg, gpointer data)
{
  tint * id = (tint *)data;
  double angle_x, angle_y;
  switch (id -> b)
  {
    case RIGHT:
      angle_x = 0.0;
      angle_y = -90.0;
      break;
    case LEFT:
      angle_x = 0.0;
      angle_y = 90.0;
      break;
    case TOP:
      angle_x = 90.0;
      angle_y = 0.0;
      break;
    case BOTTOM:
      angle_x = -90.0;
      angle_y =  0.0;
      break;
    case FRONT:
      angle_x = 0.0;
      angle_y = 0.0;
      break;
    case BACK:
      angle_x = 0.0;
      angle_y = 180.0;
      break;
  }
  vec4_t q_a, q_b;
  vec3_t axis;
  axis.x = 0.0;
  axis.y = 1.0;
  axis.z = 0.0;
  q_a = axis_to_quat (axis, -pi*angle_y/180.0);
  axis.x = 1.0;
  axis.y = 0.0;
  axis.z = 0.0;
  q_b = axis_to_quat (axis, -pi*angle_x/180.0);
  get_project_by_id(id -> a) -> modelgl -> anim -> last -> img -> rotation_quaternion = q4_mul (q_a, q_b);
  update (get_project_by_id(id -> a) -> modelgl);
}

#ifdef GTK3
/*
*  GtkWidget * menu_proj (glwin * view)
*
*  Usage:
*
*  glwin * view : the target glwin
*/
GtkWidget * menu_proj (glwin * view)
{
  GtkWidget * menup = gtk_menu_new ();
  GtkWidget * r = create_menu_item (FALSE, "Right [1, 0, 0]");
  g_signal_connect (G_OBJECT (r), "activate", G_CALLBACK(set_camera_pos), & view -> colorp[RIGHT][0]);
  gtk_menu_shell_append ((GtkMenuShell *)menup, r);
  GtkWidget * l = create_menu_item (FALSE, "Left [-1, 0, 0]");
  g_signal_connect (G_OBJECT (l), "activate", G_CALLBACK(set_camera_pos), & view -> colorp[LEFT][0]);
  gtk_menu_shell_append ((GtkMenuShell *)menup, l);
  GtkWidget * t = create_menu_item (FALSE, "Top [0, 1, 0]");
  g_signal_connect (G_OBJECT (t), "activate", G_CALLBACK(set_camera_pos), & view -> colorp[TOP][0]);
  gtk_menu_shell_append ((GtkMenuShell *)menup, t);
  GtkWidget * b = create_menu_item (FALSE, "Bottom [0, -1, 0]");
  g_signal_connect (G_OBJECT (b), "activate", G_CALLBACK(set_camera_pos), & view -> colorp[BOTTOM][0]);
  gtk_menu_shell_append ((GtkMenuShell *)menup, b);
  GtkWidget * f = create_menu_item (FALSE, "Front [0, 0, 1]");
  g_signal_connect (G_OBJECT (f), "activate", G_CALLBACK(set_camera_pos), & view -> colorp[FRONT][0]);
  gtk_menu_shell_append ((GtkMenuShell *)menup, f);
  GtkWidget * a = create_menu_item (FALSE, "Back [0, 0, -1]");
  g_signal_connect (G_OBJECT (a), "activate", G_CALLBACK(set_camera_pos), & view -> colorp[BACK][0]);
  gtk_menu_shell_append ((GtkMenuShell *)menup, a);

  return menup;
}
#else
/*
*  G_MODULE_EXPORT void to_set_camera_pos (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage:
*
*  GSimpleAction * action : the GAction sending the signal
*  GVariant * parameter   : GVariant parameter of the GAction
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT void to_set_camera_pos (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  set_camera_pos (NULL, data);
}

/*
*  GMenu * menu_proj (glwin * view, int popm)
*
*  Usage:
*
*  glwin * view : the target glwin
*  int popm     : main app (0) or popup (1)
*/
GMenu * menu_proj (glwin * view, int popm)
{
  GMenu * menu = g_menu_new ();
  gchar * projection[6]={"Right [1, 0, 0]", "Left [-1, 0, 0]", "Top [0, 1, 0]", "Bottom [0, -1, 0]", "Front [0, 0, 1]", "Back [0, 0, -1]"};
  int i;
  for (i=0; i<6; i++)
  {
    append_opengl_item (view, menu, projection[i], "proj", popm, i, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(to_set_camera_pos), & view -> colorp[i][0], FALSE, FALSE, FALSE, TRUE);
  }
  return menu;
}
#endif
