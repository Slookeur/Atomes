/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#ifndef GLWIN_H_
#define GLWIN_H_

#include "ogl_shading.h"

#ifndef GL_COLOR_SUM_EXT
# define GL_COLOR_SUM_EXT 0x8458
#endif

#ifndef GL_LIGHT_MODEL_COLOR_CONTROL_EXT
# define GL_LIGHT_MODEL_COLOR_CONTROL_EXT 0x81F8
#endif

#ifndef GL_SEPARATE_SPECULAR_COLOR_EXT
# define GL_SEPARATE_SPECULAR_COLOR_EXT 0x81FA
#endif

#ifndef GL_TEXTURE_RECTANGLE_ARB
# define GL_TEXTURE_RECTANGLE_ARB 0x84F5
#endif

#define BOX 0
#define AXIS 1

#define OGL_STYLES 6
#define FILLED_STYLES 4
#define OGL_RENDERS 3
#define OGL_BOX 8
#define OGL_AXIS 13
#define OGL_REPS 2
#define ATOM_MAPS 7
#define POLY_MAPS 7
#define OGL_COORDS 5
#define OGL_RINGS 12
#define NUM_COLORS 64
#define NINPUTS 6
#define NSELECTION 7

#define NGLOBAL_SHADERS 14

#define FROM_DATA    -8
#define FROM_SPEC    -7
#define FROM_MOL     -6
#define FROM_FRAG    -5
#define FROM_COORD_P -4
#define FROM_COORD_T -3
#define FROM_LIBRARY -2
#define FROM_PROJECT -1

enum shaders {
  ATOMS = 0,
  BONDS = 1,
  SELEC = 2,
  POLYS = 3,
  MDBOX = 4,
  MAXIS = 5,
  ARROW = 6,
  RINGS = 7,
  PICKS = 8,
  LABEL = 9,
  MEASU = 10,
  LIGHT = 11,
  SLABS = 12,
  VOLMS = 13
};

struct angle {
  double angle;
  gboolean pbc;
};

struct distance {
  double length;
  double x, y, z;
  gboolean pbc;
};

typedef struct {
  gboolean show[2];
  gboolean label[2];
  gboolean pick[2];
  gboolean cloned;
  int style;
} atom_data;


typedef struct {
  GtkWidget * win;
  GtkWidget * right;
  GtkWidget * left;
  GtkWidget * up;
  GtkWidget * down;
  GtkWidget * stop;
} spinner;

typedef struct {
  GtkWidget * win;
  GtkWidget * first;
  GtkWidget * last;
  GtkWidget * prev;
  GtkWidget * next;
  GtkWidget * jump;
  GtkWidget * play;
  GtkWidget * stop;
  GtkWidget * loop;
  GtkWidget * fast;
  GtkWidget * slow;
} sequencer;

typedef struct {
  GtkWidget * win;
  GtkWidget * rec;
  GtkWidget * stop;
} recorder;

typedef struct {
  int type;           // 0 = directional, 1 = point, 2 = spot light
  int fix;            // 0 = fix for the view, 1 = fix for the molecule
  int show;
  vec3_t position;
  vec3_t direction;   // For directional and spot light
  vec3_t intensity;   // Light colors
  vec3_t attenuation; // Constant, linear and quadratic
  vec3_t spot_data;   // Angle, inner and outer cutoff
} Light;

typedef struct {
  int predefine;
  vec3_t albedo;
  // 0 = lightning model
  // 2 = metallic
  // 3 = roughness
  // 4 = back lightning
  // 5 = gamma
  // 6 = opacity
  GLfloat param[6];
} Material;

typedef struct {
  int mode;       // NONE, LINEAR, EXP, EXP2
  int based;      // Plane based or Range based
  float density;
  float depth[2];
  vec3_t color;
} Fog;

struct screen_string {
  int id;
  int type;
  char * word;
  ColRGBA col;
  float shift[4];
  int num_instances;
  float * instances;
  struct screen_string * prev;
  struct screen_string * last;
};

struct selatom {
  int id;
  int sp;
  struct selatom * prev;
  struct selatom * next;
};

struct atom_selection {
  int selected;
  int * selected_bonds;
  ColRGBA * bond_colors;
  int * selected_angles;
  ColRGBA * angle_colors;
  int * selected_dihedrals;
  ColRGBA * dihedral_colors;
  struct selatom * first;
  struct selatom * last;
};

typedef struct {

  ColRGBA backcolor;
  // Color maps for atoms [0] and polyhedra [1]
  int color_map[2];

  gboolean * show_atom[2];                   // To draw or not the atom(s)
  gboolean * show_label[2];                  // To draw or not the label(s)
  gboolean * show_poly[9];                   // To draw or not the polyhedra
  gboolean * show_coord[10];                 // To draw or not the coordination(s)
  // Atoms
  ColRGBA * at_color;
  double * sphererad;
  double * pointrad;
  double * atomicrad;

  // Bonds
  double ** bondrad;
  double ** linerad;
  double radall[2];

  // Clones
  gboolean draw_clones;
  gboolean cloned_poly;

  // Labels
  // 0 = atoms, 1 = clones, 2 = axis, 3 = measures, 4 = measure edition
  int labels_position[5];
  int labels_render[5];
  int labels_scale[5];
  gchar * labels_font[5];
  ColRGBA * labels_color[5];
  double labels_shift[5][3];
  struct screen_string * labels_list[5];
  // 0 =Element name, 1 = Atomic symbol, 2 = Atomic symbol + ID number, 3 = ID number
  int labels_format[3];
  gboolean mtilt;
  int mpattern;
  int mfactor;
  double mwidth;

  int m_is_pressed;
  struct atom_selection * selected[2];

  // Model box and partial axis data
  // BOX = 0, AXIS = 1
  int box_axis[2];
  double box_axis_rad[2];
  double box_axis_line[2];
  ColRGBA box_color;
  ColRGBA * axis_color;
  // Extra cell on a/b/c
  int extra_cell[3];

  double axis_length;
  int axispos;
  GLdouble axis_pos[3];

  int axis_labels;
  gchar * axis_title[3];

 // Coordination(s)
  ColRGBA ** spcolor[10];                    // Coordination sphere colors

  GLdouble p_depth;
  GLdouble c_angle[2];                       // Cam. Angle pitch and heading
  GLdouble c_shift[2];                       // Cam. pos x / y
  GLdouble gnear, gfar;
  GLdouble gleft, gright;
  GLdouble gtop, gbottom;
  GLdouble zoom;

  vec4_t rotation_quaternion;

  // Only for recording
  int ** i_rings[5];
  atom_data * at_data;

  // Volumes
  gboolean show_vol[FILLED_STYLES];
  ColRGBA vol_col[FILLED_STYLES];
  gboolean * fm_show_vol[2][FILLED_STYLES];
  ColRGBA * fm_vol_col[2][FILLED_STYLES];

  int rotation_mode;

  int style;
  GLint quality;
  GLint render;
  int lights;
  int * light_loc;
  Light * l_ght;
  Material m_terial;
  Fog f_g;

  int filled_type;
  int step;
  int rep;
  int id;
} image;

struct snapshot {
  image * img;
  struct snapshot * prev;
  struct snapshot * next;
};

typedef struct {
  int frames;
  struct snapshot * first;
  struct snapshot * last;
} animation;

typedef struct {
  float ** data;                       // The value per MD step / atom for custom color map
  ColRGBA ** colors;                   // The color per MD step / atom for custom color map
  int points;
  float * positions;
  float cmin;
  float cmax;
  ColRGBA * values;
} colormap;

typedef struct {
  int proj;
  int action;
  GtkWidget * atom_tree;
  GtkWidget * mode_box;
  GtkWidget * object_box;
  GtkWidget * filter_box;
  GtkWidget * atom_box;
  GtkWidget * id_box;
  GtkWidget * big_box;
  GtkTreeStore * atom_model;
  GtkListStore * obj_model;
  GtkTreePath * path;
  GtkWidget * info[2];
  GtkWidget * preview[2];
  // 0 = Unselected
  // 1 = Selected
  // 2 = All
  int status;
  // O = Normal
  // 1 = Random
  int mode;
  // 0 = Atoms
  // 1 = Groups
  int object;
  int filter;
  int spec;
  int search_digit;
  int spec_to_add;
  int num_to_add;
  int in_selection;
  GtkWidget * entry_a, * entry_b;
  GtkWidget * but_a, * but_b;
  GtkWidget * img_a, * img_b;
  // Passivating = for all atoms of that type but not as a group
  gboolean passivating;
  gboolean was_selected;
  gboolean update_bonding;
  int set_for_all;
  int int_b;
  int * lab;
  int * pick;
  int todo_size;
  int * todo;
  tint pointer[6];
} atom_search;

struct insert_object {
  int id;
  int origin;
  float dim;
  gchar * name;
  int type;
  int atoms;
  int species;
  int * old_z;
  struct atom * at_list;

  int ifcl;
  int * bcid;
  double occ;
  //double * icoord;
  double * baryc;
  int bonds;
  int ** ibonds;
  //int ** coord_data;
  coord_info * coord;
  struct insert_object * prev;
  struct insert_object * next;
};

typedef struct {
  int multi;
  gchar * let;
  gchar * site;
  gchar *** pos;
} wyckoff_position;

typedef struct {
  int origin;
  gchar * name;
  gchar * pos[3];
  int nump;
  gchar *** points;
} spg_setting;

typedef struct {
  int id;
  gchar * name;
  gchar * hms;
  gchar * bravais;
  gchar * setting;
  int sid;
  int nums;
  spg_setting * settings;
  int numw;
  wyckoff_position * wyckoff;
  mat4_t coord_origin;
  mat4_t wyck_origin;
} space_group;

typedef struct {
  double param[2][3];
  double vect[3][3];
  double rvect[3][3];
  mat4_t frac_to_cart;
  mat4_t cart_to_frac;
  double vol;
  double dens;
} box_info;

typedef struct {
  box_info * box;
  int cextra[3];
  int pbc;                  // Apply pbc (still used ?)
  int frac;                 // Frac coordinates (still ?)
  int ltype;                // Lattice type (0=isolated, 1= a,b,c + angles 2= vectors)
  double volume;
  double density;
  gboolean npt;
  gboolean has_a_box;
  gboolean crystal;
  space_group * sp_group;
} cell_info;

typedef struct {
  GtkWidget * win;
  GtkWidget * bh_box;
  GtkWidget * bv_box[2];
  GtkWidget * cs_combo;
  GtkWidget * bl_box;
  GtkWidget * ltc_box;
  GtkWidget * ltc_cons;
  GtkWidget * bv_img;
  GtkWidget * bl_combo;
  GtkWidget * sg_box;
  GtkWidget * sg_combo;
  GtkWidget * sg_but;
  GtkWidget * so_box;
  GtkWidget * so_combo;
  GtkWidget * so_info;
  GtkWidget * la_combo;
  GtkWidget * lattice_grid;
  GtkWidget * lattice_box;
  GtkWidget * bentry[2][3];
  GtkWidget * ventry[3][3];
  GtkWidget * add_combo;
  GtkWidget * pbut;
  cell_info cell;
  int occupancy;
  gboolean overlapping;
  gboolean wrap;
  gboolean clones;
  int extrac[3];
  tint pointers[3][3];
} builder_edition;

typedef struct {
  GtkWidget * win;
  GtkWidget * vbox;
  GtkWidget * notebook;
  GtkWidget * atom_combo[5];
  GtkWidget * axis_combo[2];
  GtkWidget * axis_but[2];
  GtkWidget * edit_entry[6];
  GtkWidget * edit_scale[6];
  GtkWidget * at_expand[3];
  GtkWidget * edition_but[2];
  GtkTreePath ** replace_nodes[2];
  gboolean visible;
  int active;
  int axis[2];
  gboolean show_axis[2];
  gboolean rebuilt[2];
  gboolean old_axis;
  float new_param[3][2][6];
  float old_param[3][2][6];
  int repeat_move;
  float * msd;
  float * msd_all;
  // 0 = REPLACE
  // 1 = INSERT
  // 2 = CBUILD
  // 3 = PASSIVATE
  struct insert_object * to_be_inserted[4];
  struct insert_object * to_be_moved[2];
  int adv_bonding[2];
  coord_info * coord;
  int add_spec;
  double * new_z;
} atom_edition;

typedef struct {
  GtkWidget * win;
  GtkWidget * notebook;
  GtkWidget * put_in_box;
  GtkWidget * passivate;
  GtkWidget * superbut;
  GtkWidget * shift_box[2];
  GtkWidget * ax_cell[3];
  GtkWidget * edit_entry[21];
  GtkWidget * edit_scale[21];
  GtkWidget * density_box;
  GtkWidget * density;
  gboolean slab_show;
  gboolean slab_pbc;
  gboolean cut_this_slab;
  gboolean slab_passivate;
  int slab_type;
  int slab_act;
  int slab_out;
  double slab_vol;
  float slab_alpha;
  int slab_atoms;
  int * slab_lot;
  GtkWidget * slab_opts;
  GtkWidget * slab_hbox[3];
  GtkWidget * slab_param[3];
  GtkWidget * slab_box[6];
  GtkWidget * slab_info_box;
  GtkWidget * slab_info;
  gboolean homo_density;
  double cparam[21];
  double initbox[3];
  dint slab_pointer[5];
} cell_edition;

typedef struct {
  GtkWidget * win;
  GtkWidget * templates;
  GtkWidget * param_mat;
  GtkWidget * m_entry[5];
  GtkWidget * m_scale[5];
  GtkWidget * lights;
  GtkWidget * lights_box;

  GtkWidget * basic[2];
  GtkWidget * base_ogl[2][5];
  GtkWidget * entogl[5][3];

  GtkWidget * advanced_light_box;
  GtkWidget * light_type;
  GtkWidget * light_type_box;
  GtkWidget * light_fix;
  GtkWidget * light_show;
  GtkWidget * light_b_entry[2];
  GtkWidget * light_entry[6];
  GtkWidget * light_b_coord[2];

  GtkWidget * dens_box;
  GtkWidget * depth_box;
  GtkWidget * fog_range[3];
  GtkWidget * param_fog;

  int proj;
  dint pointer[6];
  tint pos_pointer[5][3];
} opengl_edition;

typedef struct {
  GtkWidget * win;
  GtkWidget * notebook;
  GtkTreeStore * rings_model[5];
  GtkTreeStore * chains_model;
  GtkListStore * frag_mol_model[2];
  int rst[5], rsz[5], ri[5];
  GtkWidget * rilab[5][3];
  int cst, csz, ch;
  GtkWidget * chlab[3];
} coord_edition;

typedef struct {
  GtkWidget * win;
  GtkWidget * label;
  GtkWidget * selection_tree[3];
} measures;

typedef struct {
  GtkWidget * win;
  // All model
  GtkWidget * compb[FILLED_STYLES];
  GtkWidget * hboxv[FILLED_STYLES];
  GtkWidget * hbvol[FILLED_STYLES];
  GtkWidget * lab_vol[FILLED_STYLES];
  double angp;
  // Fragments and molecules
  int sid[2];
  GtkWidget ** fm_compb[2][FILLED_STYLES];
  GtkWidget ** fm_hboxv[2][FILLED_STYLES];
  GtkWidget ** fm_hbvol[2][FILLED_STYLES];
  GtkWidget ** fm_lab_vol[2][FILLED_STYLES];
  GtkWidget * fm_vbox[2];
  GtkWidget * fm_vvbox[2];
  int ngeov[2];
  int * geov_id[2];
} volumes;

typedef struct {
  GtkWidget * win;
  GtkWidget * notebook;
} model_edition;

typedef struct {
  gboolean init;
  int proj;
  // The entire OpenGL window
  GtkWidget * win;
  // The menu bar
  GtkWidget * menu_box;
  GtkWidget * menu_bar;
#ifdef GTK3
  // OpenGL menus
  GtkWidget * ogl_styles[OGL_STYLES];
  GtkWidget * filled_styles[FILLED_STYLES];
  GtkWidget * color_styles[ATOM_MAPS+POLY_MAPS];
  GtkWidget * ogl_render[OGL_RENDERS];
  GtkWidget * ogl_rep[OGL_REPS];
  GtkWidget * ogl_quality;
  GtkWidget ** ogl_box_axis[2];
  GtkWidget * ogl_box[8];
  GtkWidget * ogl_mouse[3];
  GtkWidget * ogl_atoms[8];
  GtkWidget ** ogl_spec[2];
  GtkWidget ** ogl_lab[2];
  GtkWidget * ogl_bonds[14];
  GtkWidget * ogl_clones[6];
  GtkWidget * ogl_coord[OGL_COORDS];
  GtkWidget * ogl_rings[OGL_RINGS];
  GtkWidget * ogl_chains[2];
  GtkWidget ** ogl_geom[2][10];
  GtkWidget ** ogl_poly[2][9];
  GtkWidget ** oglmv[2][10];                 // Coordination sphere show/hide menus
  GtkWidget ** oglmc[2][9];                  // Coordination sphere color menus
  GtkWidget ** oglmpv[2][9];                 // Polyhedra view/hide menus
  GtkWidget * ogl_mode[2+2*NINPUTS];
  GtkWidget * ogl_smode[NSELECTION];
  GtkWidget * ogl_anim[2];
#endif
  GtkWidget * camera_widg[7];
  GtkWidget * rbuild[2];
  GtkWidget * cbuilder;

  // Matrices
  vec3_t model_position;
  vec4_t view_port;
  mat4_t projection_matrix;
  mat4_t model_matrix;
  mat4_t view_matrix;
  mat4_t un_view_matrix;
  mat4_t model_view_matrix;
  mat4_t view_model_matrix;
  mat4_t proj_model_matrix;
  mat4_t proj_model_view_matrix;
  mat4_t proj_view_matrix;
  mat4_t normal_matrix;
  mat4_t axis_proj_model_view_matrix;
  mat4_t label_projection_matrix;

  gboolean create_shaders[NGLOBAL_SHADERS];
  glsl_program *** ogl_glsl[NGLOBAL_SHADERS];
  int * n_shaders[NGLOBAL_SHADERS];
  opengl_edition * opengl_win;
  model_edition * model_win[2];
  builder_edition * builder_win;

  // OpenGL plot
  GtkWidget * plot;
#ifndef GTKGLAREA
  GLXContext glcontext;
#endif
  int pixels[2];
  int mouseX;
  int mouseY;
  int mouseAction;
  int mouseStatus;
  int nth_copy;
  gboolean to_pick;

  // Action mode
  // 0 = analyze (normal), 1 = edit structure
  int mode;
  // Selection mode
  // 0 = atom/bond, 1 = fragment, 2 = molecule, 3 = single frag, 4 = single mol
  int selection_mode;

  // For temporary backup purposes only:
  struct atom_selection * tmp_sel[2];
  int * stored_labels[2];

  int other_status;
  coord_edition * coord_win;
  int cmap[ATOM_MAPS];
  atom_edition * atom_win;
  double ** saved_coord[3];
  cell_edition * cell_win;
  // 0 = atoms
  // 1 = clones
  // 2 = MOVE
  // 3 = REPLACE
  // 4 = REMOVE
  // 5 = INSERT
  // 6 = RANMOVE
  // 7 = CBUILDER
  // 8 = PASSIVATE
  atom_search * search_widg[9];
  vec3_t baryc[3];
  vec3_t insert_coords;
  // 0 = Status
  // 1 = Translation / Rotation
  // 3 = x_t, y_t, z_t, x_r, y_r, z_r
  float edition_param[3][2][6]; // 0-2 translation, 3-5 rotation

  // Color pointers, these are used to deal with the
  // Callbacks for the OpenGL window menus, and are used
  // for atoms / box / axis ...
  tint * colorp[64];

  colormap * custom_map;

  int allbonds[2];                 // 0/1=norm/clones, overall total number of bonds
  int ** bonds;                    // A=MD step, B=0/1=norm/clones, VALUE=number of bonds
  int **** bondid;                 // A=MD step, B=0/1=norm/clones, C=bid, D: 0 = at_1 and 1 = at_2

  // Clones
  vec3_t ** clones;

  // Coordination
  // 0 = on move
  //    atom_win active: 0 = normal, 1 = random
  //    atom_win inactive: 0/1 to turn off/on
  // 1 = on copy
  //    atom_win inactive: 0/1 to turn off/on
  gboolean rebuild[2][2];
  gboolean bonding;
  gboolean adv_bonding[2];                   // 0 = Fraglments, 1 = Molecules
  qint ** gcid[10];                          // Geom colors pointers
  gboolean rings;
  int ring_max[5];                           // The largest ring size
  int ** num_rings[5];                       // The number of rings (Search type, step, ring size)
  int **** all_rings[5];                     // The atomic rings
  gboolean *** show_rpoly[5];                // Show polyhedra of selected rings
  gboolean chains;
  int chain_max;                             // The largest chain size
  int ** num_chains;                         // The number of chains (chain size, step)
  int **** all_chains;                        // The atomic chains

  // Volumes data
  gboolean volumes;
  double * atoms_volume[FILLED_STYLES];
  double * atoms_ppvolume[FILLED_STYLES];
  // [9] : a,b,c,alpha,beta,gama,p.x,p.y,p.z
  double ** volume_box[FILLED_STYLES];
  gboolean comp_vol[FILLED_STYLES];
  double ** frag_mol_volume[2][FILLED_STYLES];
  double ** frag_mol_ppvolume[2][FILLED_STYLES];
  double *** frag_box[FILLED_STYLES];
  gboolean ** fm_comp_vol[2][FILLED_STYLES];

  int labelled;
  int picked;

  // Color picking
  int to_be_picked;                          // Total number of objects that can be picked
  int atoms_to_be_picked;                    // Number of atoms that can be picked
  int clones_to_be_picked;                   // Number of atoms that can be picked
  int bonds_to_be_picked;                    // Number of bonds that can be picked (do not include clones)
  int * color_to_pick;                       // The different colors that can be picked

  // Spinner, player
  sequencer * player;
  gboolean play;
  gboolean stop;
  gboolean loop;
  int speed;
  spinner * spiner;
  gboolean spin[4];
  int spin_speed[4];
  recorder * rec;
  gboolean record;

  int frames;
  animation * anim;

  double cshift[3];
  gboolean wrapped;

  measures * measure_win;
  volumes * volume_win;

  float zoom_factor;
  GLdouble p_moy;
  gboolean fullscreen;

  int action_id;
  GSimpleActionGroup * action_group;
} glwin;

#endif
