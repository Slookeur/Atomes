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
* This file: 'glview.c'
*
*  Contains: 
*
*
*
*
*  List of subroutines: 

  GLuint * allocgluint (int  val);
  GLuint ** allocdgluint (int xal, int yal);

  GLfloat ** allocdGLfloat (int xal, int yal);

  gboolean is_GLExtension_Supported (const char * extension);

  G_MODULE_EXPORT gboolean on_motion (GtkWidget * widg, GdkEvent * event, gpointer data);
  G_MODULE_EXPORT gboolean on_lib_pressed (GtkWidget * widg, GdkEvent * event, gpointer data);
  G_MODULE_EXPORT gboolean on_glwin_button_event (GtkWidget * widg, GdkEvent * event, gpointer data);
  G_MODULE_EXPORT gboolean on_scrolled (GtkWidget * widg, GdkEvent * event, gpointer data);
  G_MODULE_EXPORT gboolean on_glwin_pointer_scoll (GtkEventControllerScroll * event, gdouble dx, gdouble dy, gpointer data);
  G_MODULE_EXPORT gboolean on_expose (GtkGLArea * area, GdkGLContext * context, gpointer data);
  G_MODULE_EXPORT gboolean on_expose (GtkWidget * widg, cairo_t * cr, gpointer data);

  void sort (int dim, int * tab);
  void update_atom_neighbors_ (int * stp, int * at, int * nv);
  void update_this_neighbor_ (int * stp, int * at, int * iv, int * nv);
  void update (glwin * view);
  void transform (glwin * view, double aspect);
  void reshape (glwin * view, int width, int height);
  void rotate_x_y (glwin * view, double angle_x, double angle_y);
  void save_rotation_quaternion (glwin * view);
  void edit_for_motion (glwin * view);
  void motion (glwin * view, gint x, gint y, GdkModifierType state);
  void render_this_gl_window (glwin * view, GtkGLArea * area, gint button, double ptx, double pty);
  void render_this_gl_window (glwin * view, GtkWidget * widg, gint button);
  void glwin_lib_pressed (double x, double y, guint event_type, int event_button, gpointer data);
  void glwin_button_event (double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data);
  void zoom (glwin * view, int delta);
  void init_camera (struct project * this_proj, int get_depth);
  void image_init_spec_data (image * img, struct project * this_proj, int nsp);
  void set_img_lights (struct project * this_proj, image * img);
  void init_img (struct project * this_proj);
  void init_opengl (glwin * view);
  void center_molecule (struct project * this_proj);
  void center_this_molecule (glwin * view);
  void free_glwin_spec_data (struct project * this_proj, int spec);
  void glwin_init_spec_data (struct project * this_proj, int nspec);
  void init_glwin (glwin * view);
  void gtk_window_change_gdk_visual (GtkWidget * win);

  G_MODULE_EXPORT void on_glwin_pointer_motion (GtkEventControllerMotion * motc, gdouble x, gdouble y, gpointer data);
  G_MODULE_EXPORT void on_lib_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
  G_MODULE_EXPORT void on_lib_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
  G_MODULE_EXPORT void on_glwin_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
  G_MODULE_EXPORT void on_glwin_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
  G_MODULE_EXPORT void on_realize (GtkGLArea * area, gpointer data);
  G_MODULE_EXPORT void on_realize (GtkWidget * widg, gpointer data);

  GError * init_gtk_gl_area (GtkGLArea * area);

  ColRGBA set_default_color (int z);

*/

#include "global.h"
#include "interface.h"
#include "initcoord.h"
#include "bind.h"
#include "project.h"
#include "glview.h"

extern vec3_t arc_ball_init;
extern vec4_t old_rotation_quaternion;
extern void process_the_hits (glwin * view, gint event_button, double ptx, double pty);
extern void arc_ball_rotation (glwin * view, int x, int y);
extern vec3_t get_arc_ball_vector (glwin * view, int x, int y);
extern Light init_light_source (int type, float val, float vbl);
extern void rotate_quat (struct project * this_proj, vec4_t q, int status, int axis);
extern void translate (struct project * this_proj, int status, int axis, vec3_t trans);
extern vec3_t get_bary (struct project * this_proj, int status);
extern void update_labels (glwin * view);
extern void prepare_atom_edition (gpointer data, gboolean visible);
extern atom_search * allocate_atom_search (int proj, int action, int searchid, int tsize);
extern int action_atoms_from_project (struct project * this_proj, atom_search * asearch, gboolean visible);
extern struct insert_object * create_object_from_frag_mol (struct project * this_proj, int coord, int geo, atom_search * remove);

GLenum ogl_texture;

/*
*  GLuint * allocgluint (int  val)
*
*  Usage: 
*
*  int  val : 
*/
GLuint * allocgluint (int  val)
{
  GLuint * var = NULL;

  var = g_malloc0 (val*sizeof*var);
  return var;
}

/*
*  GLuint ** allocdgluint (int xal, int yal)
*
*  Usage: 
*
*  int xal : 
*  int yal : 
*/
GLuint ** allocdgluint (int xal, int yal)
{
  GLuint ** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocgluint(yal);
  }
  return var;
}

/*
*  GLfloat ** allocdGLfloat (int xal, int yal)
*
*  Usage: 
*
*  int xal : 
*  int yal : 
*/
GLfloat ** allocdGLfloat (int xal, int yal)
{
  GLfloat ** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = g_malloc0 (yal*sizeof*var[i]);
  }
  return var;
}

const float light_pos[MAX_LIGHTS][4] = {{-0.1f,  0.1f,  1.0f, 0.0f},
                                        { 1.0f,  2.0f,  0.5f, 0.0f},
                                        {-1.0f,  2.0f, -1.0f, 0.0f},
                                        {-1.0f, -1.0f,  0.0f, 0.0f}};

/*
*  ColRGBA set_default_color (int z)
*
*  Usage: 
*
*  int z : 
*/
ColRGBA set_default_color (int z)
{
  ColRGBA col;
  double colors[116][3]={{1.00, 1.00, 1.00},
	              {0.85, 1.00, 1.00},
                  {0.80, 0.50, 1.00},
                  {0.76, 1.00, 0.00},
                  {1.00, 0.71, 0.71},
                  {0.56, 0.56, 0.56},
                  {0.18, 0.31, 0.97},
                  {1.00, 0.05, 0.05},
                  {0.56, 0.87, 0.31},
                  {0.70, 0.89, 0.96},
                  {0.67, 0.36, 0.95},
                  {0.54, 1.00, 0.00},
                  {0.75, 0.65, 0.65},
                  {0.94, 0.78, 0.62},
                  {1.00, 0.50, 0.00},
                  {1.00, 1.00, 0.19},
                  {0.12, 0.94, 0.12},
                  {0.50, 0.82, 0.89},
                  {0.56, 0.25, 0.83},
                  {0.24, 1.00, 0.00},
                  {0.90, 0.90, 0.90},
                  {0.75, 0.76, 0.78},
                  {0.65, 0.65, 0.67},
                  {0.54, 0.60, 0.78},
                  {0.61, 0.48, 0.78},
                  {0.87, 0.40, 0.20},
                  {0.94, 0.56, 0.62},
                  {0.31, 0.81, 0.31},
                  {0.78, 0.50, 0.20},
                  {0.49, 0.50, 0.69},
                  {0.76, 0.56, 0.56},
                  {0.40, 0.56, 0.56},
                  {0.74, 0.50, 0.89},
                  {1.00, 0.63, 0.00},
                  {0.65, 0.16, 0.16},
                  {0.36, 0.72, 0.82},
                  {0.44, 0.18, 0.69},
                  {0.00, 1.00, 0.00},
                  {0.58, 1.00, 1.00},
                  {0.58, 0.88, 0.88},
                  {0.45, 0.76, 0.79},
                  {0.33, 0.71, 0.71},
                  {0.23, 0.62, 0.62},
                  {0.14, 0.56, 0.56},
                  {0.04, 0.49, 0.55},
                  {0.00, 0.41, 0.52},
                  {0.75, 0.75, 0.75},
                  {1.00, 0.85, 0.56},
                  {0.65, 0.46, 0.45},
                  {0.40, 0.50, 0.50},
                  {0.62, 0.39, 0.71},
                  {0.83, 0.48, 0.00},
                  {0.58, 0.00, 0.58},
                  {0.26, 0.62, 0.69},
                  {0.34, 0.09, 0.56},
                  {0.00, 0.79, 0.00},
                  {0.44, 0.83, 1.00},
                  {1.00, 1.00, 0.78},
                  {0.85, 1.00, 0.78},
                  {0.78, 1.00, 0.78},
                  {0.64, 1.00, 0.78},
                  {0.56, 1.00, 0.78},
                  {0.38, 1.00, 0.78},
                  {0.27, 1.00, 0.78},
                  {0.19, 1.00, 0.78},
                  {0.12, 1.00, 0.78},
                  {0.00, 1.00, 0.61},
                  {0.00, 0.90, 0.46},
                  {0.00, 0.83, 0.32},
                  {0.00, 0.75, 0.22},
                  {0.00, 0.67, 0.14},
                  {0.30, 0.76, 1.00},
                  {0.30, 0.65, 1.00},
                  {0.13, 0.58, 0.84},
                  {0.15, 0.49, 0.67},
                  {0.15, 0.40, 0.59},
                  {0.09, 0.33, 0.53},
                  {0.81, 0.81, 0.87},
                  {1.00, 0.81, 0.13},
                  {0.72, 0.72, 0.81},
                  {0.65, 0.33, 0.30},
                  {0.34, 0.35, 0.38},
                  {0.62, 0.31, 0.71},
                  {0.67, 0.36, 0.00},
                  {0.46, 0.31, 0.27},
                  {0.26, 0.51, 0.59},
                  {0.26, 0.00, 0.40},
                  {0.00, 0.49, 0.00},
                  {0.44, 0.67, 0.98},
                  {0.00, 0.73, 1.00},
                  {0.00, 0.63, 1.00},
                  {0.00, 0.56, 1.00},
                  {0.00, 0.50, 1.00},
                  {0.00, 0.42, 1.00},
                  {0.33, 0.36, 0.95},
                  {0.54, 0.31, 0.89},
                  {0.63, 0.21, 0.83},
                  {0.70, 0.12, 0.83},
                  {0.70, 0.12, 0.73},
                  {0.70, 0.05, 0.65},
                  {0.74, 0.05, 0.53},
                  {0.78, 0.00, 0.40},
                  {0.80, 0.00, 0.35},
                  {0.82, 0.00, 0.31},
                  {0.85, 0.00, 0.27},
                  {0.88, 0.00, 0.22},
                  {0.90, 0.00, 0.18},
                  {0.92, 0.00, 0.15},
                  {0.93, 0.00, 0.14},
                  {0.94, 0.00, 0.13},
                  {0.95, 0.00, 0.12},
                  {0.96, 0.00, 0.10},
                  {0.97, 0.00, 0.10},
                  {0.98, 0.00, 0.10},
                  {0.99, 0.00, 0.10}};
  col.red = colors[z-1][0];
  col.green = colors[z-1][1];
  col.blue = colors[z-1][2];
  col.alpha = 1.0;
  return col;
}

void update_bonds_ (int * bd, int * stp,
                    int * bdim, int bda[* bdim], int bdb[* bdim],
                    double * x, double * y, double * z)
{
  int i, j, k;

  active_glwin -> allbonds[* bd] += * bdim;
  active_glwin -> bonds[* stp][* bd] = * bdim;

  if (* bdim > 0)
  {
    active_glwin -> bondid[* stp][* bd] = NULL;
    active_glwin -> bondid[* stp][* bd] = allocdint (* bdim, 2);
    for (i=0; i< * bdim; i++)
    {
      j = bda[i] - 1;
      k = bdb[i] - 1;
      active_glwin -> bondid[* stp][* bd][i][0] = j;
      active_glwin -> bondid[* stp][* bd][i][1] = k;
    }
    if (* bd)
    {
      if (active_glwin -> clones[* stp] != NULL)
      {
        g_free (active_glwin -> clones[* stp]);
        active_glwin -> clones[* stp] = NULL;
      }
      active_glwin -> clones[* stp] = g_malloc0 (*bdim*sizeof*active_glwin -> clones[* stp]);
      for (i=0; i< * bdim; i++)
      {
        active_glwin -> clones[* stp][i].x = x[i];
        active_glwin -> clones[* stp][i].y = y[i];
        active_glwin -> clones[* stp][i].z = z[i];
        j = bda[i] - 1;
        k = bdb[i] - 1;
        active_project -> atoms[* stp][j].cloned = TRUE;
        active_project -> atoms[* stp][k].cloned = TRUE;
      }
    }
  }
}

/*
*  void sort (int dim, int * tab)
*
*  Usage: 
*
*  int dim   : 
*  int * tab : 
*/
void sort (int dim, int * tab)
{
  int i, j, val;
  for (i=1; i<dim; i++)
  {
    val = tab[i];
    for (j=i-1; j>-1; j--)
    {
      if (tab[j] <= val) break;
      tab[j+1] = tab[j];
    }
    tab[j+1]=val;
  }
}

/*
*  void update_atom_neighbors_ (int * stp, int * at, int * nv)
*
*  Usage: 
*
*  int * stp : 
*  int * at  : 
*  int * nv  : 
*/
void update_atom_neighbors_ (int * stp, int * at, int * nv)
{
  active_project -> atoms[* stp][* at].numv = * nv;
  if (* nv)
  {
    active_project -> atoms[* stp][* at].vois = allocint(* nv);
  }
}

/*
*  void update_this_neighbor_ (int * stp, int * at, int * iv, int * nv)
*
*  Usage: 
*
*  int * stp : 
*  int * at  : 
*  int * iv  : 
*  int * nv  : 
*/
void update_this_neighbor_ (int * stp, int * at, int * iv, int * nv)
{
  active_project -> atoms[* stp][* at].vois[* iv] = * nv - 1;
  if (* iv == active_project -> atoms[* stp][* at].numv - 1)
  {
    sort (active_project -> atoms[* stp][* at].numv, active_project -> atoms[* stp][* at].vois);
  }
}

/*
*  void update (glwin * view)
*
*  Usage: 
*
*  glwin * view : the target glwin pointer
*/
void update (glwin * view)
{
  gtk_gl_area_queue_render ((GtkGLArea *)view -> plot);
#ifdef G_OS_WIN32
  gtk_widget_hide (view -> plot);
  gtk_widget_show (view -> plot);
#endif
}

/*
*  void transform (glwin * view, double aspect)
*
*  Usage: 
*
*  glwin * view  : the target glwin pointer
*  double aspect : 
*/
void transform (glwin * view, double aspect)
{
  GLdouble w, h;
  GLdouble dw, dh;

  double zoom = view -> anim -> last -> img -> zoom;
  view -> zoom_factor = zoom * 0.1 * view -> anim -> last -> img -> p_depth / (2.0 * view -> anim -> last -> img -> gfar);
  if (view -> anim -> last -> img -> rep == ORTHOGRAPHIC)
  {
    zoom *= (view -> anim -> last -> img -> p_depth /  view -> anim -> last -> img -> gnear);
  }
  dw = view -> anim -> last -> img -> c_shift[0]*2.0*zoom;
  dh = view -> anim -> last -> img -> c_shift[1]*2.0*zoom;
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
  view -> anim -> last -> img -> gleft = -w + dw;
  view -> anim -> last -> img -> gright = w + dw;
  view -> anim -> last -> img -> gbottom = -h + dh;
  view -> anim -> last -> img -> gtop = h + dh;

  if (view -> anim -> last -> img -> rep == PERSPECTIVE)
  {
    view -> projection_matrix = m4_frustum (view -> anim -> last -> img -> gleft,
                                            view -> anim -> last -> img -> gright,
                                            view -> anim -> last -> img -> gbottom,
                                            view -> anim -> last -> img -> gtop,
                                            view -> anim -> last -> img -> gnear,
                                            view -> anim -> last -> img -> gfar);
  }
  else
  {
    view -> projection_matrix = m4_ortho (view -> anim -> last -> img -> gleft,
                                          view -> anim -> last -> img -> gright,
                                          view -> anim -> last -> img -> gbottom,
                                          view -> anim -> last -> img -> gtop,
                                         -view -> anim -> last -> img -> gfar,
                                          view -> anim -> last -> img -> gfar);
  }
}

/*
*  void reshape (glwin * view, int width, int height)
*
*  Usage: 
*
*  glwin * view : the target glwin pointer
*  int width    : 
*  int height   : 
*/
void reshape (glwin * view, int width, int height)
{
  double aspect;
  int scale = 1.0;
  if (view -> win)
  {
    if (GTK_IS_WIDGET(view -> win))
    {
      scale = gtk_widget_get_scale_factor (view -> win);
    }
  }
  glViewport (0, 0, (GLsizei) scale * width, (GLsizei) scale * height);
  view -> view_port = vec4 (0.0, 0.0, width, height);
  aspect = (double) width / (double) height;
  transform (view, aspect);
}

/*
*  void rotate_x_y (glwin * view, double angle_x, double angle_y)
*
*  Usage: 
*
*  glwin * view   : the target glwin pointer
*  double angle_x : 
*  double angle_y : 
*/
void rotate_x_y (glwin * view, double angle_x, double angle_y)
{
  vec3_t axis;
  vec4_t q_a, q_b, q_c;
  axis.x = 0.0;
  axis.y = 1.0;
  axis.z = 0.0;
  q_a = axis_to_quat (axis, -pi*angle_y/180.0);
  axis.x = 1.0;
  axis.y = 0.0;
  axis.z = 0.0;
  q_b = axis_to_quat (axis, -pi*angle_x/180.0);
  q_c = q4_mul (q_a, q_b);
  view -> anim -> last -> img -> c_angle[0] -= angle_x;
  view -> anim -> last -> img -> c_angle[1] -= angle_y;
  int i;
  for (i=0; i<2; i++)
  {
    if (abs(view -> anim -> last -> img -> c_angle[i]) > 180.0) view -> anim -> last -> img -> c_angle[i] = 0.0;
    if (view -> camera_widg[i+3])
    {
      if (GTK_IS_WIDGET(view -> camera_widg[i+3]))
      {
        gtk_spin_button_set_value ((GtkSpinButton *)view -> camera_widg[i+3], view -> anim -> last -> img -> c_angle[i]);
      }
    }
  }

  if (view -> mode != EDITION)
  {
    view -> anim -> last -> img -> rotation_quaternion = q4_mul (old_rotation_quaternion, q_c);
  }
  else
  {
    rotate_quat (get_project_by_id(view -> proj), q_c, 1, 1);
    init_default_shaders (view);
#ifdef GTK3
    // GTK3 Menu Action To Check
    set_advanced_bonding_menus (view);
#endif
  }
}

/*
*  void save_rotation_quaternion (glwin * view)
*
*  Usage: 
*
*  glwin * view : the target glwin pointer
*/
void save_rotation_quaternion (glwin * view)
{
  //int i;
  //for (i=0; i<4; i++) old_rotation_quaternion[i] = view -> anim -> last -> img -> rotation_quaternion[i];
  old_rotation_quaternion.w = view -> anim -> last -> img -> rotation_quaternion.w;
  old_rotation_quaternion.x = view -> anim -> last -> img -> rotation_quaternion.x;
  old_rotation_quaternion.y = view -> anim -> last -> img -> rotation_quaternion.y;
  old_rotation_quaternion.z = view -> anim -> last -> img -> rotation_quaternion.z;
}

/*
*  void edit_for_motion (glwin * view)
*
*  Usage: 
*
*  glwin * view : the target glwin pointer
*/
void edit_for_motion (glwin * view)
{
  gboolean check_edit = FALSE;
  struct project * this_proj = get_project_by_id(view -> proj);
  prepare_atom_edition (& view -> colorp[0][0], FALSE);
  atom_search * move_search = allocate_atom_search (this_proj -> id, DISPL, DISPL, this_proj -> natomes);
  int ** frag = allocdint (this_proj -> coord -> totcoord[2], 2);
  int i, j, k;
  j = 0;
  for (i=0; i<this_proj -> natomes; i++)
  {
    k = this_proj -> atoms[0][i].coord[2];
    frag[k][0] ++;
    if (this_proj -> atoms[0][i].pick[0])
    {
      move_search -> todo[i] = 1;
      frag[k][1] ++;
      j ++;
    }
  }
  if (j)
  {
    for (i=0; i<this_proj -> coord -> totcoord[2]; i++)
    {
      if (frag[i][1] && frag[i][1] < frag[i][0])
      {
        check_edit = TRUE;
        break;
      }
    }
    if (check_edit)
    {
      action_atoms_from_project (this_proj, move_search,
                                 (this_proj -> modelgl -> atom_win) ? this_proj -> modelgl -> atom_win -> visible : FALSE);
    }
    else
    {
      struct insert_object * object;
      for (i=0; i<this_proj -> coord -> totcoord[2]; i++)
      {
        if (frag[i][1])
        {
          object_motion = TRUE;
          object = create_object_from_frag_mol (this_proj, 2, i, NULL);
          object_motion = FALSE;
          g_free (object);
        }
      }
    }
    g_free (frag);
  }
  view -> baryc[1] = get_bary (this_proj, 1);
  move_search = free_this_search_data (move_search);
  view -> rebuild[0][0] = FALSE;
  view -> rebuild[0][1] = TRUE;
}

/*
*  void motion (glwin * view, gint x, gint y, GdkModifierType state)
*
*  Usage: 
*
*  glwin * view          : the target glwin pointer
*  gint x                : 
*  gint y                : 
*  GdkModifierType state : 
*/
void motion (glwin * view, gint x, gint y, GdkModifierType state)
{
  view -> mouseAction = MOTION;
  int i;
  if (view -> mouseStatus == CLICKED)
  {
    if (view -> mode == EDITION && view -> rebuild[0][0]) edit_for_motion (view);

    if (state & GDK_BUTTON1_MASK)
    {
      arc_ball_rotation (view, x, y);
    }
    else if (state & GDK_BUTTON2_MASK)
    {
      if (view -> mode != EDITION)
      {
        view -> anim -> last -> img -> c_shift[0] -= (double) (x - view -> mouseX) / view -> pixels[0];
        view -> anim -> last -> img -> c_shift[1] += (double) (y - view -> mouseY) / view -> pixels[1];
        for (i=0; i<2; i++)
        {
          if (view -> camera_widg[i+5])
          {
            if (GTK_IS_WIDGET(view -> camera_widg[i+5]))
            {
              gtk_spin_button_set_value ((GtkSpinButton *)view -> camera_widg[i+5], - view -> anim -> last -> img -> c_shift[i]);
            }
          }
        }
      }
      else
      {
        vec3_t pos_a = vec3(x, - y, 0.75);
        vec3_t pos_b = vec3(view -> mouseX, - view -> mouseY, 0.75);
        vec3_t trans_a = v3_un_project (pos_a, view -> view_port, view -> projection_matrix);
        vec3_t trans_b = v3_un_project (pos_b, view -> view_port, view -> projection_matrix);
        vec3_t trans;
        trans.x = (trans_a.x - trans_b.x);
        trans.y = (trans_b.y - trans_a.y);
        if (view -> anim -> last -> img -> rep == PERSPECTIVE)
        {
          trans.x *= view -> anim -> last -> img -> p_depth;
          trans.y *= view -> anim -> last -> img -> p_depth;
        }
        trans.z = 0.0;
        translate (get_project_by_id(view -> proj), 1, 1, trans);
      }
    }
    if (view -> mode == EDITION)
    {
      init_default_shaders (view);
#ifdef GTK3
      // GTK3 Menu Action To Check
      set_advanced_bonding_menus (view);
#endif
    }
    view -> mouseX = x;
    view -> mouseY = y;
    update (view);
  }
}

#ifdef GTK3
/*
*  G_MODULE_EXPORT gboolean on_motion (GtkWidget * widg, GdkEvent * event, gpointer data)
*
*  Usage: 
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  GdkEvent * event : 
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean on_motion (GtkWidget * widg, GdkEvent * event, gpointer data)
{
  glwin * view = (glwin *) data;
  if (view -> mouseStatus != RELEASED)
  {
    GdkEventMotion * mevent = (GdkEventMotion *)event;
    gint x, y;
    GdkModifierType state;
    if (mevent -> is_hint)
    {
      gdk_window_get_device_position (mevent -> window, mevent -> device, & x, & y, & state);
    }
    else
    {
      x = (gint) mevent -> x;
      y = (gint) mevent -> y;
      state = (GdkModifierType) mevent -> state;
    }
    motion (view, x, y, state);
  }
  return FALSE;
}
#else
/*
*  G_MODULE_EXPORT void on_glwin_pointer_motion (GtkEventControllerMotion * motc, gdouble x, gdouble y, gpointer data)
*
*  Usage: 
*
*  GtkEventControllerMotion * motc : 
*  gdouble x                       : 
*  gdouble y                       : 
*  gpointer data                   : the associated data pointer
*/
G_MODULE_EXPORT void on_glwin_pointer_motion (GtkEventControllerMotion * motc, gdouble x, gdouble y, gpointer data)
{
  glwin * view = (glwin *) data;
  if (view -> mouseStatus != RELEASED)
  {
    motion (view, (gint)x, (gint)y, gtk_event_controller_get_current_event_state((GtkEventController*)motc));
  }
}
#endif

#ifdef GTKGLAREA
/*
*  void render_this_gl_window (glwin * view, GtkGLArea * area, gint button, double ptx, double pty)
*
*  Usage: 
*
*  glwin * view     : the target glwin pointer
*  GtkGLArea * area : 
*  gint button      : 
*  double ptx       : 
*  double pty       : 
*/
void render_this_gl_window (glwin * view, GtkGLArea * area, gint button, double ptx, double pty)
#else
/*
*  void render_this_gl_window (glwin * view, GtkWidget * widg, gint button)
*
*  Usage: 
*
*  glwin * view     : the target glwin pointer
*  GtkWidget * widg : the GtkWidget sending the signal
*  gint button      : 
*/
void render_this_gl_window (glwin * view, GtkWidget * widg, gint button)
#endif
{
#ifdef GTKGLAREA
  view -> pixels[0] = gtk_widget_get_allocated_width (GTK_WIDGET(area));
  view -> pixels[1] = gtk_widget_get_allocated_height (GTK_WIDGET(area));
  gtk_gl_area_make_current (area);
  if (gtk_gl_area_get_error (area) == NULL)
#else
  view -> pixels[0] = gtk_widget_get_allocated_width (widg);
  view -> pixels[1] = gtk_widget_get_allocated_height (widg);
  GdkWindow * win = gtk_widget_get_window (widg);
  if (glXMakeCurrent (GDK_WINDOW_XDISPLAY (win), GDK_WINDOW_XID (win), view -> glcontext))
#endif
  {
    reshape (view, view -> pixels[0], view -> pixels[1]);
    draw (view);
    if (view -> to_pick)
    {
      if (button) process_the_hits (view, button, ptx, pty);
      view -> to_pick = FALSE;
      reshape (view, view -> pixels[0], view -> pixels[1]);
      draw (view);
    }
#ifdef GTKGLAREA
    glFlush ();
#else
    glXSwapBuffers (GDK_WINDOW_XDISPLAY (win), GDK_WINDOW_XID (win));
#endif
  }
}

/*
*  void glwin_lib_pressed (double x, double y, guint event_type, int event_button, gpointer data)
*
*  Usage: 
*
*  double x         : 
*  double y         : 
*  guint event_type : 
*  int event_button : 
*  gpointer data    : the associated data pointer
*/
void glwin_lib_pressed (double x, double y, guint event_type, int event_button, gpointer data)
{
  glwin * view = (glwin *) data;
  switch (event_type)
  {
    case GDK_BUTTON_PRESS:
      view -> mouseStatus = CLICKED;
      view -> mouseX = x;
      view -> mouseY = y;
      if (event_button == 1)
      {
        save_rotation_quaternion (view);
        arc_ball_init = get_arc_ball_vector (view, view -> mouseX, view -> mouseY);
        view -> to_pick = FALSE;
        update (view);
      }
      break;
    case GDK_BUTTON_RELEASE:
      view -> mouseStatus = RELEASED;
      break;
  }
}

#ifdef GTK3
/*
*  G_MODULE_EXPORT gboolean on_lib_pressed (GtkWidget * widg, GdkEvent * event, gpointer data)
*
*  Usage: 
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  GdkEvent * event : 
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean on_lib_pressed (GtkWidget * widg, GdkEvent * event, gpointer data)
{
  GdkEventButton * bevent = (GdkEventButton*)event;
  glwin_lib_pressed (bevent -> x, bevent -> y, bevent -> type, bevent -> button, data);
  return FALSE;
}
#else
/*
*  G_MODULE_EXPORT void on_lib_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
*
*  Usage: 
*
*  GtkGesture * gesture : 
*  int n_press          : 
*  double x             : 
*  double y             : 
*  gpointer data        : the associated data pointer
*/
G_MODULE_EXPORT void on_lib_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  glwin_lib_pressed (x, y, GDK_BUTTON_PRESS, gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), data);
}

/*
*  G_MODULE_EXPORT void on_lib_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
*
*  Usage: 
*
*  GtkGesture * gesture : 
*  int n_press          : 
*  double x             : 
*  double y             : 
*  gpointer data        : the associated data pointer
*/
G_MODULE_EXPORT void on_lib_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  glwin_lib_pressed (x, y, GDK_BUTTON_RELEASE, gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), data);
}
#endif // GTK3

gl_pop_info to_pop;

/*
*  void glwin_button_event (double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data)
*
*  Usage: 
*
*  double event_x     : 
*  double event_y     : 
*  guint event_button : 
*  guint event_type   : 
*  guint32 event_time : 
*  gpointer data      : the associated data pointer
*/
void glwin_button_event (double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data)
{
  glwin * view = (glwin *) data;
  switch (event_type)
  {
    case GDK_BUTTON_PRESS:
      view -> mouseStatus = CLICKED;
      view -> mouseX = event_x;
      view -> mouseY = event_y;
      clock_gettime (CLOCK_MONOTONIC, & start_time);
      if (event_button == 1 || event_button == 3)
      {
        save_rotation_quaternion (view);
        arc_ball_init = get_arc_ball_vector (view, view -> mouseX, view -> mouseY);
        view -> nth_copy = 0;
        view -> insert_coords = get_insertion_coordinates (view);
#ifdef GTKGLAREA
        render_this_gl_window (view, GTK_GL_AREA(view -> plot), event_button, event_x, event_y);
#else
        render_this_gl_window (view, plot, event_button);
#endif
      }
      break;
    case GDK_BUTTON_RELEASE:
      view -> mouseStatus = RELEASED;
      clock_gettime (CLOCK_MONOTONIC, & stop_time);
      if (get_calc_time (start_time, stop_time) < 0.4)
      {
        view -> to_pick = TRUE;
#ifdef GTKGLAREA
        render_this_gl_window (view, GTK_GL_AREA(view -> plot), event_button, event_x, event_y);
#else
        render_this_gl_window (view, plot, event_button);
#endif
      }
      if (view -> mode == EDITION && view -> mouseAction == MOTION)
      {
        view -> baryc[1] = get_bary (get_project_by_id(view -> proj), 1);
        view -> mouseAction = ANALYZE;
      }
      if (event_button == 3)
      {
        switch (to_pop.action)
        {
          case 1:
            popup_main_menu (view, to_pop.x, to_pop.y);
            break;
          case 2:
            popup_selection (view, to_pop.x, to_pop.y, to_pop.pts[0], to_pop.pts[1], to_pop.pts[2], to_pop.pts[3], to_pop.pts[4]);
            break;
        }
        to_pop.action = 0;
      }
      break;
  }
}

#ifdef GTK3
/*
*  G_MODULE_EXPORT gboolean on_glwin_button_event (GtkWidget * widg, GdkEvent * event, gpointer data)
*
*  Usage: 
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  GdkEvent * event : 
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean on_glwin_button_event (GtkWidget * widg, GdkEvent * event, gpointer data)
{
  GdkEventButton * bevent = (GdkEventButton*)event;
  glwin_button_event (bevent -> x, bevent -> y, bevent -> button, bevent -> type, bevent -> time, data);
  return FALSE;
}
#else
/*
*  G_MODULE_EXPORT void on_glwin_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
*
*  Usage: 
*
*  GtkGesture * gesture : 
*  int n_press          : 
*  double x             : 
*  double y             : 
*  gpointer data        : the associated data pointer
*/
G_MODULE_EXPORT void on_glwin_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  glwin_button_event (x, y, gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), GDK_BUTTON_PRESS, gtk_event_controller_get_current_event_time((GtkEventController *)gesture), data);
}

/*
*  G_MODULE_EXPORT void on_glwin_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
*
*  Usage: 
*
*  GtkGesture * gesture : 
*  int n_press          : 
*  double x             : 
*  double y             : 
*  gpointer data        : the associated data pointer
*/
G_MODULE_EXPORT void on_glwin_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  glwin_button_event (x, y, gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), GDK_BUTTON_RELEASE, gtk_event_controller_get_current_event_time((GtkEventController *)gesture), data);
}
#endif // GTK3

// Zoom in or out
/*
*  void zoom (glwin * view, int delta)
*
*  Usage: 
*
*  glwin * view : the target glwin pointer
*  int delta    : 
*/
void zoom (glwin * view, int delta)
{
  view -> anim -> last -> img -> zoom += delta * view -> zoom_factor;
  if (view -> camera_widg[0])
  {
    if (GTK_IS_WIDGET(view -> camera_widg[0]))
    {
      gtk_spin_button_set_value ((GtkSpinButton *)view -> camera_widg[0], 1.0-0.5*view -> anim -> last -> img -> zoom);
    }
  }
}

#ifdef GTK3
/*
*  G_MODULE_EXPORT gboolean on_scrolled (GtkWidget * widg, GdkEvent * event, gpointer data)
*
*  Usage: 
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  GdkEvent * event : 
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean on_scrolled (GtkWidget * widg, GdkEvent * event, gpointer data)
{
  glwin * view = (glwin *) data;
  GdkEventScroll * sevent = (GdkEventScroll *)event;
  if (sevent -> direction == GDK_SCROLL_UP)
  {
    zoom (view, 1);
  }
  else if (sevent -> direction == GDK_SCROLL_DOWN && view -> anim -> last -> img -> zoom > ZOOM_MAX)
  {
    zoom (view, -1);
  }
  update_labels (view);
  update (view);
  return FALSE;
}
#else
/*
*  G_MODULE_EXPORT gboolean on_glwin_pointer_scoll (GtkEventControllerScroll * event, gdouble dx, gdouble dy, gpointer data)
*
*  Usage: 
*
*  GtkEventControllerScroll * event : 
*  gdouble dx                       : 
*  gdouble dy                       : 
*  gpointer data                    : the associated data pointer
*/
G_MODULE_EXPORT gboolean on_glwin_pointer_scoll (GtkEventControllerScroll * event, gdouble dx, gdouble dy, gpointer data)
{
  glwin * view = (glwin *) data;
  if (dy < 0)
  {
    zoom (view, 1);
  }
  else if (dy > 0 && view -> anim -> last -> img -> zoom > ZOOM_MAX)
  {
    zoom (view, -1);
  }
  update_labels (view);
  update (view);
  return TRUE;
}
#endif

/*
*  void init_camera (struct project * this_proj, int get_depth)
*
*  Usage: 
*
*  struct project * this_proj : 
*  int get_depth              : 
*/
void init_camera (struct project * this_proj, int get_depth)
{
  glwin * view = this_proj -> modelgl;
  if (get_depth) view -> anim -> last -> img -> p_depth = (this_proj -> natomes) ? oglmax_ () : 50.0;
  if (view -> camera_widg[1])
  {
    if (GTK_IS_WIDGET(view -> camera_widg[1]))
    {
      gtk_spin_button_set_value ((GtkSpinButton *)view -> camera_widg[1], view -> anim -> last -> img -> p_depth);
    }
  }
  view -> anim -> last -> img -> gnear = 6.0;//view -> anim -> last -> img -> p_depth/15.0;
  if (view -> camera_widg[2])
  {
    if (GTK_IS_WIDGET(view -> camera_widg[2]))
    {
      gtk_spin_button_set_value ((GtkSpinButton *)view -> camera_widg[2], view -> anim -> last -> img -> gnear);
    }
  }
  view -> anim -> last -> img -> gfar = view -> anim -> last -> img -> p_depth*2.0;
  view -> anim -> last -> img -> rotation_quaternion.w = 0.0;
  view -> anim -> last -> img -> rotation_quaternion.x = 0.0;
  view -> anim -> last -> img -> rotation_quaternion.y = 0.0;
  view -> anim -> last -> img -> rotation_quaternion.z = 1.0;
  int i;
  for (i=0; i<2; i++)
  {
    view -> anim -> last -> img -> c_shift[i] = 0.0;
    view -> anim -> last -> img -> c_angle[i] = 0.0;
    if (view -> camera_widg[i+5])
    {
      if (GTK_IS_WIDGET(view -> camera_widg[i+5]))
      {
        gtk_spin_button_set_value ((GtkSpinButton *)view -> camera_widg[i+5], view -> anim -> last -> img -> c_shift[i]);
      }
    }
  }
  save_rotation_quaternion (view);
  rotate_x_y (view, CAMERA_ANGLE_X, CAMERA_ANGLE_Y);
  view -> anim -> last -> img -> zoom = ZOOM;
  if (view -> camera_widg[0])
  {
    if (GTK_IS_WIDGET(view -> camera_widg[0]))
    {
      gtk_spin_button_set_value ((GtkSpinButton *)view -> camera_widg[0], 1.0 - 0.5*view -> anim -> last -> img -> zoom);
    }
  }
}

/*
*  void image_init_spec_data (image * img, struct project * this_proj, int nsp)
*
*  Usage: 
*
*  image * img                : 
*  struct project * this_proj : 
*  int nsp                    : 
*/
void image_init_spec_data (image * img, struct project * this_proj, int nsp)
{
  int i, j;
  // Chemical species related
  for (i = 0; i<2; i++)
  {
    img -> show_label[i] = allocbool(nsp);
    img -> show_atom[i] = allocbool(nsp);
    for (j=0; j<nsp; j++) img -> show_atom[i][j] = TRUE;
  }
  img -> at_color = g_malloc0 (2*nsp*sizeof*img -> at_color);
  img -> sphererad = allocdouble (2*nsp);
  img -> pointrad = allocdouble (2*nsp);
  img -> atomicrad = allocdouble (2*nsp);
  img -> bondrad = allocddouble (2*nsp, 2*nsp);
  img -> linerad = allocddouble (2*nsp, 2*nsp);
  for (i = 0; i < nsp; i++)
  {
    img -> sphererad[i] = img -> sphererad[i+nsp] = this_proj -> chemistry -> chem_prop[CHEM_R][i]/2.0;
    img -> atomicrad[i] = img -> atomicrad[i+nsp] = this_proj -> chemistry -> chem_prop[CHEM_R][i];
    img -> pointrad[i] = img -> pointrad[i+nsp] = DEFAULT_SIZE;
    img -> at_color[i] = img -> at_color[i+nsp] = set_default_color ((int)this_proj -> chemistry -> chem_prop[CHEM_Z][i]);
    img -> linerad[i][i] = img -> linerad[i+nsp][i+nsp] = DEFAULT_SIZE;
    img -> bondrad[i][i] = img -> bondrad[i+nsp][i+nsp] = min(1.0, img -> sphererad[i]/2.0);
  }
  for (i=0; i < nsp-1; i++)
  {
    for (j=i+1; j < nsp; j++)
    {
      img -> linerad[i][j] = img -> linerad[j][i]
                           = img -> linerad[i+nsp][j+nsp]
                           = img -> linerad[j+nsp][i+nsp] = DEFAULT_SIZE;
      img -> bondrad[i][j] = min(1.0, img -> sphererad[i]/2.0);
      img -> bondrad[i][j] = min(img -> bondrad[i][j], img -> sphererad[j]/2.0);
      img -> bondrad[j][i] = img -> bondrad[i+nsp][j+nsp]
                           = img -> bondrad[j+nsp][i+nsp]
                           = img -> bondrad[i][j];
    }
  }
  for (i=0; i<10; i++)
  {
    img -> spcolor[i] = NULL;
    if (i < 2)
    {
      img -> spcolor[i] = g_malloc (nsp*sizeof*img -> spcolor[i]);
    }
    else
    {
      img -> spcolor[i] = g_malloc (1*sizeof*img -> spcolor[i]);
      img -> spcolor[i][0] = NULL;
    }
  }
}

/*
*  void set_img_lights (struct project * this_proj, image * img)
*
*  Usage: 
*
*  struct project * this_proj : 
*  image * img                : 
*/
void set_img_lights (struct project * this_proj, image * img)
{
  img -> lights = 3;
  if (img -> l_ght) g_free (img -> l_ght);
  img -> l_ght = g_malloc0 (3*sizeof*img -> l_ght);
  float val;
  if (this_proj -> cell.box)
  {
    val = (this_proj -> cell.box[0].param[0][0] == 0.0) ? img -> p_depth : this_proj -> cell.box[0].param[0][0];
  }
  else
  {
    val = img -> p_depth;
  }
  float vbl = img -> p_depth;
  img -> l_ght[0] = init_light_source (0, val, vbl);
  img -> l_ght[1] = init_light_source (1, val, vbl);
  img -> l_ght[2] = init_light_source (1, val, vbl);
}

/*
*  void init_img (struct project * this_proj)
*
*  Usage: 
*
*  struct project * this_proj : 
*/
void init_img (struct project * this_proj)
{
  int i;
  this_proj -> modelgl -> anim -> last -> img = g_malloc0(sizeof*this_proj -> modelgl -> anim -> last -> img);
  image * img = this_proj -> modelgl -> anim -> last -> img;
  img -> backcolor.red = 0.0;
  img -> backcolor.green = 0.0;
  img -> backcolor.blue = 0.0;
  img -> backcolor.alpha = 1.0;
  img -> box_color.red = 0.0;
  img -> box_color.green = 1.0;
  img -> box_color.blue = 0.0;
  img -> box_color.alpha = 1.0;
  img -> color_map[0] = 0;
  img -> color_map[1] = 0;
  img -> box_axis_rad[BOX] = 0.05;
  img -> box_axis_line[BOX] = DEFAULT_SIZE;
  img -> axispos = BOTTOM_RIGHT;
  img -> box_axis_rad[AXIS] = 0.1;
  img -> box_axis_line[AXIS] = DEFAULT_SIZE;
  img -> axis_length = 2.0*DEFAULT_SIZE;
  img -> axis_color = NULL;
  img -> axis_pos[0] = 50.0;
  img -> axis_pos[1] = 50.0;
  img -> axis_pos[2] = 0.0;
  img -> axis_labels = 1;
  img -> filled_type = NONE;
  img -> quality = QUALITY;
  img -> render = FILL;
  img -> rep = PERSPECTIVE;

  // Visual styles
  img -> style = (this_proj -> natomes <= 1000) ? BALL_AND_STICK : DEFAULT_STYLE;
  img -> box_axis[AXIS] = NONE; // (this_proj -> natomes <= 1000) ?  CYLINDERS : DEFAULT_STYLE;
  if (this_proj -> cell.pbc)
  {
    img -> box_axis[BOX] = (this_proj -> natomes <= 1000) ? CYLINDERS : DEFAULT_STYLE;
  }
  else
  {
    img -> box_axis[BOX] = NONE;
  }

  for (i=0; i<5; i++)
  {
    img -> labels_position[i] = 1;
    img -> labels_render[i] = BETTER_TEXT;
    if (i < 2) img -> labels_format[i] = SYMBOL_AND_NUM;
    img -> labels_font[i] = g_strdup_printf ("Sans Bold 12");
  }
  img -> mtilt = TRUE;
  img -> mfactor = 1;
  img -> mwidth = 1.0;
  for (i=0; i<2; i++)
  {
    img -> labels_font[3+i] = g_strdup_printf ("Courier New Bold 18");
    img -> labels_color[3+i] = g_malloc (sizeof*img -> labels_color[3]);
    img -> labels_color[3+i][0].red = 1.0;
    img -> labels_color[3+i][0].green = 1.0;
    img -> labels_color[3+i][0].blue = 1.0;
    img -> labels_color[3+i][0].alpha = 1.0;
    img -> selected[i] = g_malloc0 (sizeof*img -> selected[i]);
  }
  img -> axis_title[0] = g_strdup_printf ("x");
  img -> axis_title[1] = g_strdup_printf ("y");
  img -> axis_title[2] = g_strdup_printf ("z");

  img -> radall[0] = img -> radall[1] = 0.1;

  if (this_proj -> nspec) image_init_spec_data (img, this_proj, this_proj -> nspec);
  this_proj -> modelgl -> p_moy = img -> p_depth = (this_proj -> natomes) ? oglmax_ () : 50.0;
  set_img_lights (this_proj, img);
  img -> m_terial.predefine = 4;
  img -> m_terial.albedo = vec3(0.5, 0.5, 0.5);
  img -> m_terial.param[0] = DEFAULT_LIGHTING;
  img -> m_terial.param[1] = DEFAULT_METALLIC;
  img -> m_terial.param[2] = DEFAULT_ROUGHNESS;
  img -> m_terial.param[3] = DEFAULT_AO;
  img -> m_terial.param[4] = DEFAULT_GAMMA;
  img -> m_terial.param[5] = DEFAULT_OPACITY;

  img -> f_g.density = 0.005;
  img -> f_g.depth[0] = 1.0;
  img -> f_g.depth[1] = 90.0;
  img -> f_g.color = vec3 (0.01f, 0.01f, 0.01f);
}

/*
*  gboolean is_GLExtension_Supported (const char * extension)
*
*  Usage: 
*
*  const char * extension : 
*/
gboolean is_GLExtension_Supported (const char * extension)
{
  int i, j;
  i = j = 0;
  glGetIntegerv (GL_NUM_EXTENSIONS, & i);
  for (j=0; j<i; j++)
  {
    if (g_strcmp0 (extension, (const char*)glGetStringi(GL_EXTENSIONS, j)) == 0) return TRUE;
  }
  return FALSE;
}

/*
*  void init_opengl (glwin * view)
*
*  Usage: 
*
*  glwin * view : the target glwin pointer
*/
void init_opengl (glwin * view)
{
  glEnable (GL_DEPTH_TEST);
  glDepthMask (GL_TRUE);
  glDepthFunc (GL_LEQUAL);
  glDepthRange (0.0f, 1.0f);
  glClearDepth (1.0f);

  glEnable (GL_NORMALIZE);

  glShadeModel(GL_SMOOTH);
  glCullFace(GL_BACK);
  glEnable (GL_CULL_FACE);                         // Incompatible with polyedra viz

  glEnable (GL_COLOR_SUM_EXT);

  glEnable (GL_PROGRAM_POINT_SIZE);
  glEnable (GL_VERTEX_PROGRAM_POINT_SIZE);
  glEnable (GL_POINT_SPRITE);

  glEnable (GL_POINT_SMOOTH);
  glHint (GL_POINT_SMOOTH_HINT, GL_NICEST);
  glEnable (GL_LINE_SMOOTH);                       // Lines antialiasing
  glHint (GL_LINE_SMOOTH_HINT, GL_NICEST);

  glDisable (GL_POLYGON_SMOOTH);                   // To disable ploygon antialiasing
  glEnable (GL_POLYGON_STIPPLE);
  glEnable (GL_POLYGON_OFFSET_FILL);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glPolygonOffset (1.0, 1.0);
  glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);

  glHint (GL_FOG_HINT, GL_NICEST);
  glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST); // Really Nice Perspective Calculations

  ogl_texture = (is_GLExtension_Supported("GL_ARB_texture_rectangle")) ? GL_TEXTURE_RECTANGLE_ARB : GL_TEXTURE_2D;
}

/*
*  void center_molecule (struct project * this_proj)
*
*  Usage: 
*
*  struct project * this_proj : 
*/
void center_molecule (struct project * this_proj)
{
  int l, i, j;
  double x, y, z;
  // We need to center the molecule at (0.0, 0.0, 0.0)
  // Thus (0, 0, 0) will be the center of the box
  mat4_t rot;
  vec3_t bini, bend;
  for (l = 0; l < this_proj -> steps; l++)
  {
    x = 0.0;
    y = 0.0;
    z = 0.0;
    for (i = 0; i < this_proj -> natomes; i++)
    {
      x += this_proj -> atoms[l][i].x;
      y += this_proj -> atoms[l][i].y;
      z += this_proj -> atoms[l][i].z;
    }
    x /= this_proj -> natomes;
    y /= this_proj -> natomes;
    z /= this_proj -> natomes;
    for (i = 0; i < this_proj -> natomes; i++)
    {
      this_proj -> atoms[l][i].x -= x;
      this_proj -> atoms[l][i].y -= y;
      this_proj -> atoms[l][i].z -= z;
    }

    for (i=0; i<FILLED_STYLES; i++)
    {
      if (this_proj -> modelgl -> volume_box[i])
      {
        if (this_proj -> modelgl -> volume_box[i][l])
        {
          this_proj -> modelgl -> volume_box[i][l][6] -= x;
          this_proj -> modelgl -> volume_box[i][l][7] -= y;
          this_proj -> modelgl -> volume_box[i][l][8] -= z;
          rot = m4_rotation_anti_xyz (this_proj -> modelgl -> volume_box[i][l][3], this_proj -> modelgl -> volume_box[i][l][4], this_proj -> modelgl -> volume_box[i][l][5]);
          bini = m4_mul_coord (rot, vec3(this_proj -> modelgl -> volume_box[i][l][6],  this_proj -> modelgl -> volume_box[i][l][7],  this_proj -> modelgl -> volume_box[i][l][8]));
          bini.x -= x;
          bini.y -= y;
          bini.z -= z;
          rot = m4_rotation_xyz (this_proj -> modelgl -> volume_box[i][l][3], this_proj -> modelgl -> volume_box[i][l][4], this_proj -> modelgl -> volume_box[i][l][5]);
          bend = m4_mul_coord (rot, bini);
          this_proj -> modelgl -> volume_box[i][l][6] = bend.x;
          this_proj -> modelgl -> volume_box[i][l][7] = bend.y;
          this_proj -> modelgl -> volume_box[i][l][8] = bend.z;
        }
      }
      if (this_proj -> modelgl -> frag_box[i])
      {
        if (this_proj -> modelgl -> frag_box[i][l])
        {
          for (j=0; j<this_proj -> coord -> totcoord[2]; j++)
          {
            rot = m4_rotation_anti_xyz (this_proj -> modelgl -> frag_box[i][l][j][3], this_proj -> modelgl -> frag_box[i][l][j][4], this_proj -> modelgl -> frag_box[i][l][j][5]);
            bini = m4_mul_coord (rot, vec3(this_proj -> modelgl -> frag_box[i][l][j][6], this_proj -> modelgl -> frag_box[i][l][j][7], this_proj -> modelgl -> frag_box[i][l][j][8]));
            bini.x -= x;
            bini.y -= y;
            bini.z -= z;
            rot = m4_rotation_xyz (this_proj -> modelgl -> frag_box[i][l][j][3], this_proj -> modelgl -> frag_box[i][l][j][4], this_proj -> modelgl -> frag_box[i][l][j][5]);
            bend = m4_mul_coord (rot, bini);
            this_proj -> modelgl -> frag_box[i][l][j][6] = bend.x;
            this_proj -> modelgl -> frag_box[i][l][j][7] = bend.y;
            this_proj -> modelgl -> frag_box[i][l][j][8] = bend.z;
          }
        }
      }
    }
  }
  this_proj -> cell.crystal = FALSE;
}

/*
*  void center_this_molecule (glwin * view)
*
*  Usage: 
*
*  glwin * view : the target glwin pointer
*/
void center_this_molecule (glwin * view)
{
  center_molecule (get_project_by_id(view -> proj));
  view -> insert_coords = vec3(0.0, 0.0, 0.0);
  int shaders[6] = {ATOMS, BONDS, POLYS, RINGS, SELEC, VOLMS};
  re_create_md_shaders (6, shaders, get_project_by_id(view -> proj));
  view -> create_shaders[PICKS] = TRUE;
  view -> create_shaders[MDBOX] = TRUE;
  view -> create_shaders[LABEL] = TRUE;
  view -> create_shaders[MEASU] = TRUE;
  update (view);
}

/*
*  void free_glwin_spec_data (struct project * this_proj, int spec)
*
*  Usage: 
*
*  struct project * this_proj : 
*  int spec                   : 
*/
void free_glwin_spec_data (struct project * this_proj, int spec)
{
  int i, j, k;

  for (i=0; i<NUM_COLORS; i++)
  {
    if (this_proj -> modelgl -> colorp[i] != NULL)
    {
      g_free (this_proj -> modelgl -> colorp[i]);
      this_proj -> modelgl -> colorp[i] = NULL;
    }
  }
  for (i=0; i<10; i++)
  {
    k = (i > 2) ? 1 : spec;
    if (i < 2 || i > 3)
    {
      for (j=0; j<k; j++)
      {
        if (this_proj -> coord -> geolist[i][j] != NULL)
        {
          g_free (this_proj -> coord -> geolist[i][j]);
          this_proj -> coord -> geolist[i][j] = NULL;
        }
      }
      this_proj -> coord -> geolist[i]=NULL;
    }
  }
  for (i=0; i<spec; i++)
  {
    if (this_proj -> coord -> partial_geo[i] != NULL) g_free (this_proj -> coord -> partial_geo[i]);
    this_proj -> coord -> partial_geo[i] = NULL;
  }
  g_free (this_proj -> coord -> partial_geo);
  this_proj -> coord -> partial_geo = NULL;
}

/*
*  void glwin_init_spec_data (struct project * this_proj, int nspec)
*
*  Usage: 
*
*  struct project * this_proj : 
*  int nspec                  : 
*/
void glwin_init_spec_data (struct project * this_proj, int nspec)
{
  int i, j, k;
  for (i=0; i<NUM_COLORS; i++)
  {
    this_proj -> modelgl -> colorp[i] = NULL;
    this_proj -> modelgl -> colorp[i] = g_malloc (nspec*2*sizeof*this_proj -> modelgl -> colorp[i]);
    for (j=0; j<nspec*2; j++)
    {
      this_proj -> modelgl -> colorp[i][j].a = this_proj -> id;
      this_proj -> modelgl -> colorp[i][j].b = i;
      this_proj -> modelgl -> colorp[i][j].c = j;
    }
  }
#ifdef GTK3
  // GTK3 Menu Action To Check
  int l;
#endif
  for (i=0; i<10; i++)
  {
    k = (i > 2) ? 1 : nspec;
#ifdef GTK3
    // GTK3 Menu Action To Check
    for (j = 0; j < 2; j++)
    {
      this_proj -> modelgl -> oglmv[j][i] = NULL;
      this_proj -> modelgl -> oglmv[j][i] = g_malloc0 (k*sizeof*this_proj -> modelgl -> oglmv[j][i]);
      if (i < 9)
      {
        this_proj -> modelgl -> oglmc[j][i] = NULL;
        this_proj -> modelgl -> oglmc[j][i] = g_malloc0 (k*sizeof*this_proj -> modelgl -> oglmc[j][i]);
      }
      if (i < 2 || (i > 3 && i < 9))
      {
        this_proj -> modelgl -> oglmpv[j][i] = NULL;
        this_proj -> modelgl -> oglmpv[j][i] = g_malloc0 (k*sizeof*this_proj -> modelgl -> oglmpv[j][i]);
      }
      for (l=0; l<k; l++)
      {
        this_proj -> modelgl -> oglmv[j][i][l] = NULL;
        if (i < 9) this_proj -> modelgl -> oglmc[j][i][l] = NULL;
        if (i < 2 || (i > 3 && i < 9)) this_proj -> modelgl -> oglmpv[j][i][l] = NULL;
      }
    }
#endif
    if (i < 2 || i > 3)
    {
      this_proj -> coord -> geolist[i] = g_malloc0 (k*sizeof*this_proj -> coord -> geolist[i]);
      for (j=0; j<k; j++)
      {
        this_proj -> coord -> geolist[i][j] = NULL;
      }
    }
  }
  this_proj -> coord -> partial_geo = g_malloc0 (nspec*sizeof*this_proj -> coord -> partial_geo);
  for (i=0; i<nspec; i++) this_proj -> coord -> partial_geo[i] = NULL;
}

/*
*  void init_glwin (glwin * view)
*
*  Usage: 
*
*  glwin * view : the target glwin pointer
*/
void init_glwin (glwin * view)
{
  struct project * this_proj = get_project_by_id(view -> proj);    // Have to be the active project
  view -> anim = g_malloc0 (sizeof*view -> anim);
  struct snapshot * snap = g_malloc0 (sizeof*snap);
  view -> anim -> first = snap;
  view -> anim -> last = snap;
  init_img (this_proj);
  init_camera (this_proj, FALSE);
  view -> mouseStatus = RELEASED;
  view -> mouseAction = ANALYZE;
  // Warning, if not centered at start-up, dmtx could fail
  if (! this_proj -> cell.crystal) center_molecule (this_proj);

  view -> bonds = allocdint (this_proj -> steps, 2);
  view -> bondid = g_malloc0 (this_proj -> steps*sizeof*view -> bondid);
  view -> clones = g_malloc0 (this_proj -> steps*sizeof*view -> clones);
  int i;
  for (i=0; i < this_proj -> steps; i++)
  {
    view -> bondid[i] = g_malloc0 (2*sizeof*view -> bondid[i]);
    view -> clones[i] = NULL;
  }

  // Data that depends on the number of chemical species
  glwin_init_spec_data (this_proj, (this_proj -> nspec) ? this_proj -> nspec : 1);

  view -> stop = TRUE;
  view -> speed = 100;
  view -> zoom_factor = ZOOM_FACTOR;
  view -> mode = ANALYZE;
  view -> selection_mode = ATOMS;
  view -> rebuild[0][0] = view -> rebuild[1][0] = (this_proj -> steps > 1) ? FALSE : TRUE;
  view -> init = TRUE;
  init_opengl (view);
  init_shaders (view);
  this_proj -> initgl = TRUE;
#ifdef GTK4
  update_menu_bar (view);
#endif
}

/*
*  GError * init_gtk_gl_area (GtkGLArea * area)
*
*  Usage: 
*
*  GtkGLArea * area : 
*/
GError * init_gtk_gl_area (GtkGLArea * area)
{
  if (area == NULL)
  {
    area = (GtkGLArea *)gtk_gl_area_new ();
  }
  else
  {
    gtk_gl_area_make_current (area);
  }
  gtk_gl_area_set_has_depth_buffer (area, TRUE);
  gtk_gl_area_set_has_stencil_buffer (area, TRUE);
  return gtk_gl_area_get_error (area);
}

#ifdef GTK3
/*
*  void gtk_window_change_gdk_visual (GtkWidget * win)
*
*  Usage: 
*
*  GtkWidget * win : the GtkWidget sending the signal
*/
void gtk_window_change_gdk_visual (GtkWidget * win)
{
  // GTK+ > 3.15.1 uses an X11 visual optimized for GTK+'s OpenGL stuff
  // since revid dae447728d: https://github.com/GNOME/gtk/commit/dae447728d
  // However, in some cases it simply cannot start an OpenGL context.
  // This changes to the default X11 visual instead the GTK's default.
  GdkScreen * screen = gdk_screen_get_default ();
  GList * visuals = gdk_screen_list_visuals (screen);
  // printf("n visuals: %u\n", g_list_length(visuals));
  GdkX11Screen* x11_screen = GDK_X11_SCREEN (screen);
  g_assert (x11_screen != NULL);
  Visual * default_xvisual = DefaultVisual (GDK_SCREEN_XDISPLAY(x11_screen), GDK_SCREEN_XNUMBER(x11_screen));
  GdkVisual * default_visual = NULL;
  // int i = 0;
  while (visuals != NULL)
  {
    GdkVisual * visual = GDK_X11_VISUAL (visuals -> data);
    if (default_xvisual -> visualid == gdk_x11_visual_get_xvisual(GDK_X11_VISUAL (visuals -> data)) -> visualid)
    {
      // printf("Default visual %d\n", i);
      default_visual = visual;
    }
    // i++;
    visuals = visuals -> next;
  }
  gtk_widget_set_visual(GTK_WIDGET(win), default_visual);
}
#endif

#ifdef GTKGLAREA
/*
*  G_MODULE_EXPORT void on_realize (GtkGLArea * area, gpointer data)
*
*  Usage: 
*
*  GtkGLArea * area : 
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void on_realize (GtkGLArea * area, gpointer data)
#else
/*
*  G_MODULE_EXPORT void on_realize (GtkWidget * widg, gpointer data)
*
*  Usage: 
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void on_realize (GtkWidget * widg, gpointer data)
#endif
{
  glwin * view = (glwin *)data;
  GError * err;
#ifdef GTKGLAREA
  err = init_gtk_gl_area (area);
  if (err == NULL)
  {
#else
  GdkWindow * xwin = gtk_widget_get_window (view -> plot);
  GLint attr_list[] = {GLX_DOUBLEBUFFER,
                       GLX_RGBA,
                       GLX_DEPTH_SIZE, 1,
                       GLX_RED_SIZE,   1,
                       GLX_GREEN_SIZE, 1,
                       GLX_BLUE_SIZE,  1,
                       None};
  XVisualInfo * visualinfo = glXChooseVisual (GDK_WINDOW_XDISPLAY (xwin),
                                              gdk_screen_get_number (gdk_window_get_screen (xwin)), attr_list);
  view -> glcontext = glXCreateContext (GDK_WINDOW_XDISPLAY (xwin), visualinfo, NULL, TRUE);
  g_free (visualinfo);
  if (glXMakeCurrent (GDK_WINDOW_XDISPLAY (xwin), GDK_WINDOW_XID (xwin), view -> glcontext))
  {
#endif
    init_glwin (view);
  }
  else
  {
#ifdef GTK3
#ifdef GTKGLAREA
    gtk_window_change_gdk_visual (view -> win);
    err = init_gtk_gl_area (area);
    if (err == NULL)
    {
      init_glwin (view);
    }
    else
    {
#endif
#endif
      gchar * errm = g_strdup_printf ("Impossible to initialize the OpenGL 3D rendering ! \n %s\n", err -> message);
      g_error_free (err);
      show_error (errm, 0, MainWindow);
      g_free (errm);
      destroy_this_widget (view -> plot);
#ifdef GTK3
#ifdef GTKGLAREA
    }
#endif
#endif
    //quit_gtk();
  }
}

#ifdef GTKGLAREA
/*
*  G_MODULE_EXPORT gboolean on_expose (GtkGLArea * area, GdkGLContext * context, gpointer data)
*
*  Usage: 
*
*  GtkGLArea * area       : 
*  GdkGLContext * context : 
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT gboolean on_expose (GtkGLArea * area, GdkGLContext * context, gpointer data)
#else
/*
*  G_MODULE_EXPORT gboolean on_expose (GtkWidget * widg, cairo_t * cr, gpointer data)
*
*  Usage: 
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  cairo_t * cr     : 
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT gboolean on_expose (GtkWidget * widg, cairo_t * cr, gpointer data)
#endif
{
  glwin * view = (glwin *) data;
#ifdef GTK3
  GdkEvent * event = gtk_get_current_event ();
  if (event && event -> type == GDK_EXPOSE && ((GdkEventExpose *)event) -> count > 0) return TRUE;
#endif
#ifdef GTKGLAREA
  render_this_gl_window (view, area, 0, 0.0, 0.0);
#else
  render_this_gl_window (view, widg, 0);
#endif
  return TRUE;
}
