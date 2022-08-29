/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#ifndef GL_VIEW_H_
#define GL_VIEW_H_

#define QUALITY 30
#define DEFAULT_STYLE 1
#define DEFAULT_SIZE 1
#define DEFAULT_LIGHTING  1.00
#define DEFAULT_METALLIC  0.35
#define DEFAULT_ROUGHNESS 0.15
#define DEFAULT_AO        1.00
#define DEFAULT_GAMMA     1.50
#define DEFAULT_OPACITY   1.00

// default light data
#define MAX_LIGHTS 10
#define DEFAULT_INTENSITY 10.0

#define NEAR_PLANE 1.0
#define FAR_PLANE 10.0
#define CAMERA_ANGLE_X 5.0    // pitch in degree
#define CAMERA_ANGLE_Y -25.0  // heading in degree
#define CAMERA_ANGLE_Z 0.0    // Toll in degree
#define ZOOM 1.5              //
#define ZOOM_FACTOR 0.05      //
#define ZOOM_MAX 0.001        //

#define REFRESH 10
#define MAX_IN_SELECTION 21
#define COORD_MAX_MENU 20

enum radii {
  VDW = 0,
  INC = 1,
  COV = 2,
  ION = 3
};

enum representation {
  ORTHOGRAPHIC = 0,
  PERSPECTIVE = 1
};

enum modes {
  ANALYZE = 0,
  EDITION = 1,
  MOTION = 2,
  DL_POLY = 3,
  LAAMPS = 4,
  CPMD_AI = 5,
  CP2K_AI = 6,
  CP2K_QMMM = 7,
  CP2K_MM = 8
};

#define NUM_STYLES 7

enum styles {
  NONE = -1,
  BALL_AND_STICK = 0,
  WIREFRAME = 1,
  SPACEFILL = 2,
  SPHERES = 3,
  CYLINDERS = 4,
  PUNT = 5
};

enum render {
  FILL = 0,
  LINE = 1,
  PTS = 2
};

enum action {
  ROTATE = 0,
  MOVE = 1,
  SELECT = 2,
  ZOOM_IN_OUT = 3,
  SPIN = 4
};

enum mouse_status {
  CLICKED = 0,
  RELEASED = 1
};

enum axisposition {
  TOP_RIGHT = 0,
  TOP_LEFT = 1,
  BOTTOM_RIGHT = 2,
  BOTTOM_LEFT = 3,
  CENTER = 4,
  CUSTOM = 5
};

enum labels {
  ELEMENT_NAME = 0,
  SYMBOL = 1,
  SYMBOL_AND_NUM = 2,
  NUM = 3,
  ID_IN_MOLECULE = 4
};

enum actions {
  DISPL = 2,
  REPLACE = 3,
  REMOVE = 4,
  INSERT = 5,
  RANMOVE = 6
};

enum texts {
  SIMPLE_TEXT = 0,
  BETTER_TEXT = 1
};

/*enum material {
  AMBIENT = 0,
  DIFFUSE = 1,
  SPECULAR = 2,
  EMISSION = 3,
  OPACITY = 4
};*/

extern GLfloat ** allocdGLfloat (int xal, int yal);
extern GLfloat initlights[4][4];
extern gboolean pick;
extern glwin * wingl;
extern struct project * proj_gl;
extern coord_info * coord_gl;
extern box_info * box_gl;
extern cell_info * cell_gl;
extern int proj_sp;
extern int proj_at;
extern image * plot;
extern int qual;
extern int acolorm;
extern int pcolorm;
extern int step;
extern int gColorID[3];
extern int field_object;
extern GLenum ogl_texture;

extern gboolean create_bond (int ac, int bid, int ba, int bb, int sel, double length);
extern void draw_cylinder_bond (struct atom a, struct atom b, int bid, int ci, int bi);
extern void draw_cylinder_bond_to_pick (struct atom a, struct atom b, int bid);
extern void draw_wireframe_bond (struct atom a, struct atom b, int ci, int bi);
extern gboolean create_atom (GLUquadricObj * quadric, int aid, int ac, int sp, int sel);

extern void draw_vertices (int id);
extern void draw_atom_to_pick (struct atom at);
extern void draw_atom (struct atom at, double al);
extern void draw_rings_gl (int se, int ge, int ta, int id);
extern void draw_box (void);
extern void prepare_cuboid (vec3_t position, int id);
extern void prepare_axis (void);
extern void draw (glwin * view);
extern void render_all_strings (int glsl, int id);
extern void prepare_string (char * text, int id, ColRGBA col, vec3_t pos, float lshift[3],
                            struct atom * at, struct atom * bt, struct atom * ct);

ColRGBA init_color (int id, int numid);
ColRGBA set_default_color (int z);
extern void sort (int dim, int * tab);
extern vec3_t get_insertion_coordinates (glwin * view);
void setup_bonds (glwin * view);
void update (glwin * view);
void center_this_molecule (glwin * view);
G_MODULE_EXPORT void to_center_this_molecule (GtkWidget * widg, gpointer data);
#ifdef GTK3
G_MODULE_EXPORT gboolean on_lib_pressed (GtkWidget * widg, GdkEvent * event, gpointer data);
G_MODULE_EXPORT gboolean on_glwin_button_event (GtkWidget * widg, GdkEvent * event, gpointer data);
G_MODULE_EXPORT gboolean on_motion (GtkWidget * widg, GdkEvent * event, gpointer data);
G_MODULE_EXPORT gboolean on_scrolled (GtkWidget * widg, GdkEvent * event, gpointer data);
#else
G_MODULE_EXPORT void on_lib_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
G_MODULE_EXPORT void on_lib_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
G_MODULE_EXPORT void on_glwin_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
G_MODULE_EXPORT void on_glwin_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
G_MODULE_EXPORT void on_glwin_pointer_motion (GtkEventControllerMotion * motion, gdouble x, gdouble y, gpointer data);
G_MODULE_EXPORT gboolean on_glwin_pointer_scoll (GtkEventControllerScroll * event, gdouble dx, gdouble dy, gpointer data);
#endif
void reshape (glwin * view, int width, int height);
void zoom (glwin * view, int delta);

void init_opengl (glwin * view);
void init_camera (struct project * this_proj, gboolean get_depth);

#ifdef GTKGLAREA
  G_MODULE_EXPORT void on_realize (GtkGLArea * area, gpointer data);
  G_MODULE_EXPORT gboolean on_expose (GtkGLArea * area, GdkGLContext * context, gpointer data);
#else
  G_MODULE_EXPORT void on_realize (GtkWidget * area, gpointer data);
  G_MODULE_EXPORT gboolean on_expose (GtkWidget * widg, cairo_t * cr, gpointer data);
#endif

extern void debug_image (image img, int i);

extern struct distance distance_2d (struct atom * at, struct atom * bt);
extern struct distance distance_3d (cell_info * cell, int mdstep, struct atom * at, struct atom * bt);
extern struct angle angle_2d (struct atom * at, struct atom * bt, struct atom * ct);
extern struct angle angle_3d (cell_info * cell, int mdstep, struct atom * at, struct atom * bt, struct atom * ct);
extern struct angle dihedral_3d (cell_info * cell, int mdstep, struct atom * at, struct atom * bt, struct atom * ct, struct atom * dt);

extern struct atom * duplicate_atom (struct atom * at);
extern void at_shift (struct atom * at, float * shift);
extern void at_unshift (struct atom * at, float * shift);
extern int check_label_numbers (struct project * this_proj, int types);

extern mat4_t create_axis_matrices ();
extern mat4_t create_label_matrices ();

extern int nbs, nbl, nba;
extern void re_create_all_md_shaders (glwin * view);
extern void re_create_md_shaders (int nshaders, int shaders[nshaders], struct project * this_proj);
extern void cleaning_shaders (glwin * view, int shader);
extern void init_default_shaders (glwin * view);
extern void init_shaders(glwin * view);

extern glsl_program * init_shader_program (int object, int object_id,
                                           const GLchar * vertex, const GLchar * geometry, const GLchar * fragment,
                                           GLenum type_of_vertices, int narray, int nunif, gboolean lightning, object_3d * obj);

extern void update_selection_list (struct atom_selection * at_list, struct atom * at, gboolean add);
extern void update_all_selections (glwin * view, int pi);
extern void save_all_selections (glwin * view, int pi);

extern void process_selected_atom (struct project * this_proj, glwin * view, int id, int ac, int se, int pi);

extern void update_selection_tree (glwin * view, int pi, int id);
extern void update_all_menus (glwin * view, int nats);

extern gboolean is_coord_in_menu (int id, struct project * this_proj);

extern atom_search * free_this_search_data (atom_search * this_search);
#ifdef GTK3
extern G_MODULE_EXPORT void show_hide_clones (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void set_rep (GtkWidget * widg, gpointer data);
#else
extern G_MODULE_EXPORT void show_hide_clones (GSimpleAction * action, GVariant * state, gpointer data);
#endif

extern void popup_selection (glwin * view, double ptx, double pty, int se, int pe, int ai, int bi, int ac);
extern void popup_main_menu (glwin * view, double ptx, double pty);

typedef struct {
  int action;
  double x, y;
  int pts[5];
} gl_pop_info;

extern gl_pop_info to_pop;

#endif
