/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#define GDK_SYNCHRONIZE

#ifndef GLOBAL_H_

#define GLOBAL_H_

#include <glib.h>
#include <glib/gi18n.h>
#include <locale.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include <pango/pangoft2.h>

#ifdef MAC_INTEGRATION
#  include <gtkosxapplication.h>
#endif

#ifdef G_OS_WIN32
#  include <windef.h>
#  include <windows.h>
#  define WIN32_LEAN_AND_MEAN 1
#  include <epoxy/gl.h>
#  include <GL/glu.h>
#else
#  define max(a,b) (a>=b?a:b)
#  define min(a,b) (a<=b?a:b)
#  include <epoxy/gl.h>
#  ifdef __APPLE__
#    include <OpenGL/glu.h>
#  else
#    include <GL/glu.h>
#    include <epoxy/glx.h>
#  endif
#endif

#include "math_3d.h"

typedef struct {
  int a;
  int b;
} dint;

typedef struct {
  int a;
  int b;
  int c;
} tint;

typedef struct {
  int a;
  int b;
  int c;
  int d;
} qint;

typedef struct {
  float red;
  float green;
  float blue;
  float alpha;
} ColRGBA;

typedef struct {
  // 0 = tot
  // 1 = partial
  // 2 = fragments
  // 3 = molecules
  // > 3 = rings
  // 9 = chains
  int species;
  int totcoord[10];
  int * ntg[10];
  int ** geolist[10];     // The matching table for geometries
  int *** partial_geo;    // List of partial geometries
  int cmin;               // Min. coordination
  int cmax;               // Maximum coordination
} coord_info;

#include "glwin.h"

#ifndef GTK4
#  include <gtk/gtkx.h>
#endif
#define MEDIA_NEXT "media-skip-forward"
#define MEDIA_PREV "media-skip-backward"
#define MEDIA_LAST "go-last"
#define MEDIA_FIRST "go-first"
#define MEDIA_GOTO "go-jump"
#define MEDIA_PLAY "media-playback-start"
#define MEDIA_STOP "media-playback-stop"
#define MEDIA_LOOP "view-refresh"
#define MEDIA_ULOOP "edit-cut"
#define MEDIA_SLOW "view-sort-ascending"
#define MEDIA_FAST "view-sort-descending"
#define GO_RIGHT "go-next"
#define GO_LEFT "go-previous"
#define GO_UP "go-up"
#define GO_DOWN "go-down"
#define PAGE_SETUP "document-page-setup"
#define FULLSCREEN "view-fullscreen"
#define FITBEST "zoom-fit-best"
#define RECORD "media-record"
#ifdef GTK4
#define YES "emblem-generic"
#else
#define YES "gtk-yes"
#endif
#define EDITA "edit-find-replace"
#define EDITC "edit-copy"
#define EDITF "edit-find"
#define ECUT "edit-cut"
#define DPROPERTIES "document-properties"
#define FNEW "document-new"
#define FOPEN "document-open"
#define FSAVE "document-save"
#define FSAVEAS "document-save-as"
#define FCLOSE "window-close"
#define FEXIT "application-exit"
#define LIST_ADD "list-add"
#define LIST_REM "list-remove"
#define CANCEL "process-stop"
#define EXECUTE "system-run"
#ifdef GTK4
#define APPLY "emblem-default"
#else
#define APPLY "gtk-apply"
#endif
#define DELETEB "edit-delete"
#define DIAL_ERROR "dialog-error"
#define AFILE "text-x-generic"
#define ABOUT "help-about"

#ifdef GTK4
enum ReliefStyle {
  GTK_RELIEF_NORMAL  = 0,
  GTK_RELIEF_HALF = 1,
  GTK_RELIEF_NONE = 2
};
enum ShadowStyle {
  GTK_SHADOW_NONE = 0,
  GTK_SHADOW_IN = 1,
  GTK_SHADOW_OUT = 2,
  GTK_SHADOW_ETCHED_IN = 3,
  GTK_SHADOW_ETCHED_OUT = 4
};
#define BSEP 3
#else
#define BSEP 0
#endif

enum ContainerType {
  CONTAINER_WIN = 0,
  CONTAINER_SCR = 1,
  CONTAINER_VIE = 2,
  CONTAINER_BUT = 3,
  CONTAINER_FRA = 4,
  CONTAINER_EXP = 5
};

enum ImageFormats {
  IMG_NONE = 0,
  IMG_PIXBUF = 1,
  IMG_SURFACE = 2,
  IMG_FILE = 3,
  IMG_STOCK = 4
};

#define IODEBUG FALSE

#define MAXDATC 8
#define MAXDATA 21

#define ATOM_LIMIT 100000
#define STEP_LIMIT 10000

#define OK            0
#define ERROR_RW      1
#define ERROR_PROJECT 2
#define ERROR_CURVE   3
#define ERROR_IMAGE   4
#define ERROR_ATOM_A  5
#define ERROR_ATOM_B  6
#define ERROR_UPDATE  7
#define ERROR_NO_WAY  8
#define ERROR_COORD   9
#define ERROR_RINGS  10
#define ERROR_CHAINS 11
#define ERROR_MOL    12

#define CHEM_Z 0
#define CHEM_M 1
#define CHEM_R 2
#define CHEM_N 3
#define CHEM_X 4

#define NCFORMATS 11
#define NDOTS 8
#define CHEM_PARAMS 5
#define NCALCS 12
#define NGRAPHS 10

#define NITEMS 16
#define OT 4
#define GR 0
#define SQ 1
#define SK 2
#define GK 3
#define BD 4
#define AN 5
#define RI 6
#define CH 7
#define SP 8
#define MS 9
#define BV 10
#define FF 12

#define DEFAULT_FONT_SIZE 8
#define DEFAULT_ALPHA 0.75
#define MINCUT 1.85

#ifdef G_OS_WIN32
extern gchar * PACKAGE_PREFIX;
extern gchar * PACKAGE_LIB_DIR;
extern gchar * PACKAGE_DATA_DIR;
extern gchar * PACKAGE_LOCALE_DIR;
extern gchar * PACKAGE_IMP;
extern gchar * PACKAGE_CON;
extern gchar * PACKAGE_IMG;
extern gchar * PACKAGE_PDF;
extern gchar * PACKAGE_SVG;
extern gchar * PACKAGE_EPS;
extern gchar * PACKAGE_PNG;
extern gchar * PACKAGE_JPG;
extern gchar * PACKAGE_BMP;
extern gchar * PACKAGE_TIFF;
extern gchar * PACKAGE_VOID;
extern gchar * PACKAGE_GR;
extern gchar * PACKAGE_SQ;
extern gchar * PACKAGE_BD;
extern gchar * PACKAGE_AN;
extern gchar * PACKAGE_RI;
extern gchar * PACKAGE_CH;
extern gchar * PACKAGE_SP;
extern gchar * PACKAGE_MS;
extern gchar * PACKAGE_TD;
extern gchar * PACKAGE_MOL;
extern gchar * PACKAGE_OGL;
extern gchar * PACKAGE_OGLM;
extern gchar * PACKAGE_OGLC;
extern gchar * PACKAGE_PRO;
extern gchar * PACKAGE_SET;
extern gchar * PACKAGE_LOGO;
extern gchar * PACKAGE_LAGPL;
extern gchar * PACKAGE_LABOUT;
extern gchar * PACKAGE_DOTA;
extern gchar * PACKAGE_DOTB;
extern gchar * PACKAGE_DOTC;
extern gchar * PACKAGE_DOTD;
extern gchar * PACKAGE_DOTE;
extern gchar * PACKAGE_DOTF;
extern gchar * PACKAGE_DOTG;
extern gchar * PACKAGE_DOTH;
extern gchar * PACKAGE_DFBD;
extern gchar * PACKAGE_DFAN;
extern gchar * PACKAGE_DFDI;
extern gchar * PACKAGE_DFTD;
extern gchar * PACKAGE_DFIN;
extern gchar * PACKAGE_SGCP;
extern gchar * PACKAGE_SGCI;
extern gchar * PACKAGE_SGCF;
extern gchar * PACKAGE_SGHP;
extern gchar * PACKAGE_SGTR;
extern gchar * PACKAGE_SGTI;
extern gchar * PACKAGE_SGTP;
extern gchar * PACKAGE_SGOP;
extern gchar * PACKAGE_SGOI;
extern gchar * PACKAGE_SGOC;
extern gchar * PACKAGE_SGOF;
extern gchar * PACKAGE_SGMP;
extern gchar * PACKAGE_SGMI;
extern gchar * PACKAGE_SGTC;
#endif
extern gchar * ATOMES_CONFIG;
extern gchar * ATOMES_URL;

extern gchar * mode_name[2];
extern gchar * calc_img[NCALCS-2];
extern gchar * graph_img[NGRAPHS];
extern gchar * dots[NDOTS];
extern gchar * bravais_img[14];
extern gchar * ifield[8];
extern gchar * projfile;
extern char * ifbug;
extern char * coord_files[NCFORMATS+1];
extern char * file_ext[NCFORMATS+1];
extern char * text_styles[OGL_STYLES];
extern char * text_filled[FILLED_STYLES];
extern char * calc_name[NCALCS-2];
extern char * graph_name[NGRAPHS] ;
extern char * rings_type[5];
extern char * untime[6];
extern gchar * workspacefile;

extern int nprojects;
extern int activep;
extern int activev;
extern int activef;
extern int inactep;
extern int activew;
extern int statusval;
extern int dialog_id;

extern int bonds_update;
extern int frag_update;
extern int mol_update;

extern int tmp_pixels[2];
extern int * pasted_todo;
extern struct insert_object * copied_object;

extern GMainLoop * Event_loop[5];

extern gboolean registered_atomes;
extern gboolean testing_atomes;
extern gboolean in_movie_encoding;
extern gboolean newspace;
extern gboolean reading_input;
extern gboolean tmp_adv_bonding[2];
extern gboolean column_label;
extern gboolean check_label;
extern gboolean object_motion;
extern gboolean selected_status;
extern gboolean silent_input;

extern struct timespec start_time;
extern struct timespec stop_time;

extern double opac;
extern double pi;

extern GtkWidget * MainWindow;
extern GtkWidget * MainView;
extern GtkWidget * MainFrame[2];
extern GtkWidget * pop;
extern GtkWidget * curvetoolbox;
extern GtkWidget * progressbar;
extern GtkWidget * MainScrol[2];
extern GtkWidget * atomes_logo;
extern GtkWidget * calc_dialog;
extern GtkWidget * register_button;

extern GtkTextTag * tag;
extern GtkStatusbar * statusbar;

extern ColRGBA std[6];

extern GdkPixbuf * THETD;
extern GdkPixbuf * THEMO;
extern GdkPixbuf * THEBD;
extern GdkPixbuf * SETTING;
extern GdkPixbuf * SETTINGS;
extern GdkPixbuf * OGL;
extern GdkPixbuf * OGLM;
extern GdkPixbuf * OGLC;
extern GdkPixbuf * RUN;

extern tint cut_sel;
extern tint cut_lab;

// Data structures
#define LINE_SIZE 160
struct line_node
{
  gchar * line;//[LINE_SIZE];
  struct line_node * next;
  struct line_node * prev;
};

typedef struct {
  int traj;
  int natomes;
  int steps;
  int nrec;
  int nspec;
  double * z;
  int * nsps;
  double ** coord;
  int * lot;
  int setting;
  cell_info lattice;
  gchar * info;
  // The following is only used for CIF files
  int * wyckoff;
  double * occupancy;
  int ** occupied;
  int * multi;
  int stolab;
  int * smislab;
  int tolab;
  int * mislab;
  int * lmislab;
  gchar ** label;
  gboolean cartesian;
} coord_file;

typedef struct {
  int natomes;
  double ** coord;
  int * lot;
  int * wyckoff;
  double * occupancy;
  int ** occupied;
  int * multi;
  int spec;
  gchar ** label;
  double * z;
  int setting;
  gboolean cartesian;
  cell_info lattice;
  gchar * info;
} cif_file;

typedef struct {
  GtkWidget * a;
  GtkWidget * b;
} dwidget;

typedef struct {
  gint start_x;
  gint start_y;
  gint time;
  gboolean MouseIsDown;
} MouseState;

typedef struct {
  MouseState mouseState;
  tint * id;
} CurveState;

typedef struct {
  ColRGBA datacolor;             // Data color
  double thickness;              // Data line thickness
  int dash;                      // Data line style
  int glyph;                     // Data glyph type
  double gsize;                  // Data glyph size
  int gfreq;                     // Data glyph frequency
  int aspect;                    // X/Y or histogram
  double hwidth;                 // Histogram width
  double hopac;                  // Histogram color opacity
  int hpos;                      // Histogram is transparent ?
} DataLayout;

struct cextra {
  tint id;
  DataLayout * layout;
  struct cextra * prev;
  struct cextra * next;
};

typedef struct {
  int extras;                    // Total number of data sets
  struct cextra * first;
  struct cextra * last;
} ExtraSets;

typedef struct {
  int cid;
  int ndata;                     // number of data points
  double * data[2];              // X and Y data
  double * err;                  // Error bar on Y if any.
  double cmin[2];                // Min of the data on X and Y
  double cmax[2];                // Max of the data on X and Y
  GtkWidget * plot;              // Drawing area
  GtkWidget * button;            // Interaction button for 'Toolboxes'
  GtkWidget * curve_vbox;        // Curve top boxes for the menu bar
  GtkWidget * curve_hbox;
  GtkWidget * window;            // Widget for the window
  GtkWidget * pos;               // Mouse cursor position in graph
  int wsize[2];                  // Curve window size
  GtkWidget * datatree;          // Widget for the selection tree
  qint idcol[2];                 // For navigation in the list view
  ColRGBA backcolor;             // Background color
  int format;                    // Format of output (screen, png, pdf, ...)
  char * name;                   // Name of the curve
// Axis
  double axmin[2];               // Min for the axis X and Y
  double axmax[2];               // Max for the axis X and Y
  gboolean axis_defaut_title[2]; // Use axis default title
  char * axis_title[2];          // Title of axis
  int axis_title_x[2];           // Position of the axis title
  int axis_title_y[2];           // Position of the axis title
  gchar * axis_title_font[2];    // Axis title font
  int scale[2];                  // Axis scale (linear or log)
  gboolean autoscale[2];         // Autoscale
  gboolean show_grid[2];         // Show/Hide axis grid
  gboolean show_axis[2];         // Show/Hide axis bar
  double majt[2];                // Value for major ticks
  int mint[2];                   // Number of minor ticks
  int ticks_io[2];               // Ticks in or out / axis bar
  int ticks_pos[2];              // Ticks position: normal, opposite, both
  int majt_size[2];              // Majors ticks size (pixels)
  int mint_size[2];              // Minors ticks size (pixels)
  int labels_pos[2];             // Ticks label position: normal, opposite, both
  int labels_digit[2];           // Significant digits for tick labels
  gchar * labels_font[2];        // Ticks label font
  double labels_angle[2];        // Ticks label angle
  int labels_shift_x[2];         // Ticks position shift from x axis
  int labels_shift_y[2];         // Ticks position shift from y axis
  // Legend
  gboolean show_legend;          // Show/Hide legend
  double legend_pos[2];          // Position of the legend
  gchar * legend_font;           // Legend font
  ColRGBA legend_color;          // Legend color
  gboolean show_legend_box;      // Display legend box
  int legend_box_dash;           // Legend box line style
  ColRGBA legend_box_color;      // Legend box color
  double legend_box_thickness;   // Legend box line thickness
// Title
  char * title;                  // Title to display
  gboolean show_title;           // Show/Hide title
  gboolean default_title;        // Use default title
  double title_pos[2];           // Position of the title
  gchar * title_font;            // Title font
  ColRGBA title_color;           // Title color
// Frame
  gboolean show_frame;           // Show/Hide frame
  int frame_type;                // Frame type
  int frame_dash;                // Frame line style
  double frame_thickness;        // Frame line thickness
  ColRGBA frame_color;           // Frame color
  double frame_pos[2][2];        // Frame  0 = x, 1 = y, 0 = min and 1 = max

  DataLayout * layout;
  ExtraSets * extrac;
  cairo_surface_t * surface;
  int draw_id;
  int bshift;

  gboolean displayed;
  char * cfile;
  gchar * path;                  // For the toolbox
  int action_id;                 // Unique Id to identify actions
  CurveState state;
  GSimpleActionGroup * action_group;
} curve;

struct atom {
  int id;
  int sp;
  double x, y, z;
  int numv;
  int * vois;
  // Coord:
  // 0 = Total coordination
  // 1 = Partial coordination
  // 2 = Fragment
  // 3 = Molecule
  // 4 = Field object id
  int coord[5];
  // Rings:
  // 0 = All
  // 1 = King's
  // 2 = Guttman's
  // 3 = Primitive
  // 4 = Strong
  int ** rings[5];
  int ** chain;
  int style;
  int fid;
  int faid;
  gboolean show[2];
  gboolean label[2];
  gboolean pick[2];
  gboolean cloned;
  struct atom * prev;
  struct atom * next;
};

struct molecule {
  int id;                                 // Molecule id number
  int md;                                 // MD step
  int multiplicity;                       // Multiplicity
  int * fragments;                        // Fragments list
  int natoms;                             // Number of atoms
  int nspec;                              // Number of chemical species
  int * species;                          // Number of atom by species
};

typedef struct {
  int * mol_by_step;                      // Num of mol by steps
  struct molecule ** mols;                // List of molecules by steps
} model;

typedef struct {
  gboolean prepare_file[2];
  // Field and Config files
  gboolean afp[MAXDATC+MAXDATA];
  int type;
  int energy_unit;
  int atom_init;
  int molecules;
  struct field_molecule * first_molecule;
  int nbody[5];
  struct field_nth_body * first_body[5];
  // Tersoff potential cross terms
  double *** cross;
  int extern_fields;
  struct field_external * first_external;

  // Control file
  double * sys_opts;
  double * io_opts;
  double * ana_opts;
  double * elec_opts;
  double * vdw_opts;
  double * met_opts;
  double * equi_opts;
  int ensemble;
  int thermostat;
  double * thermo_opts;
  double * md_opts;
  double * out_opts;

} classical_field;

struct thermostat {
  int id;
  // For CPMD: 0 = none, 2 = controlled, 3 = nose
  // For CP2K: 0 = none, 2 = langevin, 3 = csvr, 4 = gle, 5 = nose
  int type;
  // For CPMD: 0 = global, 1 = local
  // For CP2K: 0 = global, 1 = local, 2 = molecule
  int sys;
  gboolean show;
  double params[4];
  int natoms;
  int * list;
  struct thermostat * next;
  struct thermostat * prev;
};

struct dummy_atom {
  // 0 = type1, 1 = type2, ...
  int id;
  int type;
  int pick[2];
  gboolean show;
  double xyz[3];
  int coord[4];
  int natoms;
  int * list;
  int numv;
  int * vois;
  struct dummy_atom * next;
  struct dummy_atom * prev;
};

typedef struct {
  int calc_type;
  int restart[10];
  int thermostats;
  struct thermostat * ions_thermostat;
  struct thermostat * elec_thermostat;
  int fixat;
  int * fixlist;
  int ** fixcoord;
  int dummies;
  struct dummy_atom * dummy;
  double default_opts[17];
  double calc_opts[24];
  int ** pp;
  gchar * info;
} cpmd;

typedef struct {
  int input_type;
  double opts[42];
  double extra_opts[3][4];
  int thermostats;
  struct thermostat * ions_thermostat;
  int fixat[2];
  int * fixlist[2];
  int ** fixcoord[2];
  gchar * files[5];
  gchar *** spec_files;
  int ** spec_data;
  gchar * info;
} cp2k;

typedef struct {
  gchar * lab;
  gchar * name;
  int Z;
  float M;
  /* float r_cov;
  float r_vdw;
  float r_ion;
  float r_cry;
  float b_coh; */

 /* int s;
  int p;
  int d;
  int f; */
} element_data;

typedef struct {
  // 0 = Z, 1 = Mass, 2 = Radius, 3 = Neutrons, 4 = X-rays
  double ** chem_prop;
  int * nsps;
  int * formula;
  char ** label;
  char ** element;
  double ** cutoffs;
  double grtotcutoff;
} chemical_data;

struct insertion {
  gchar * type;
  gchar * object;
  double Z;
  int ats;
};

typedef struct {
  double Z;
  int natomes;
} sp_in_mol ;

struct project {
  int id;
  char * name;
  char * projfile;
  char * coordfile;
  char * bondfile;
  gboolean newproj;
  gboolean run;
  gboolean trun;
  gboolean runok[NGRAPHS];
  gboolean initok[NGRAPHS];
  gboolean visok[NGRAPHS];
  gboolean dmtx;
  gboolean initgl;
  int tfile;
  int nspec;                // Number of chemical species
  int natomes;              // Number of atoms
  int dummies;              // Number of atoms including extra cells
  int steps;                // Number of MD steps
  int tunit;                // Time unit between steps, if MD
  int numwid;

  int xcor;                 // S(q) X-rays calculation f(q) or approximated
  int readata;
  gboolean tovalidate;

  gboolean runc[3]; // Bonds, Angles, Mol
  int numc[NGRAPHS];
  int num_delta[NGRAPHS];
  // gr, sq, sk, gftt, bd, an, frag-mol, ch, sp, msd
  double calc_time[NGRAPHS];
  // 0 = Search type, 1 = NUMA
  int rsearch[2];
  // First col : search type (up to chains stat). Second col: search info
  // 0 = Initnode, 1 = RMAX, 2 = ABAB, 3 = Homo, 4 = , 5 = Done ?
  int rsparam[5][6];
  double rsdata[5][5];

  int csearch; // CNUMA
  // 0 = Initnode, 1 = AAAA, 2 = ABAB, 3 = Homo, 4 = 1221, 5 = RMAX, 6 = Done ?
  int csparam[7];
  double csdata[2];

  double delta[NGRAPHS];
  double min[NGRAPHS];
  double max[NGRAPHS];

  double fact[4];
  double sk_advanced[2];

  chemical_data * chemistry;
  coord_info * coord;
  cell_info cell;
  glwin * modelgl;
  model * modelfc;
  gboolean was_moved;

  struct atom ** atoms;
  int nmols;
  int tmp_pixels[2];
  GtkTextBuffer * text_buffer[NITEMS];
  tint * idcc[NGRAPHS];
  curve ** curves[NGRAPHS];

  classical_field * force_field[2];
  cpmd * cpmd_input[2];
  cp2k * cp2k_input[2];

#ifdef DEBUG
  GtkWidget * pix_tab[3];
  GtkWidget * pix_box;
  int pix[3];
  int ** pixels;
  int actif_pix;
#endif

  struct project * next;
  struct project * prev;
};

typedef struct {
  struct project * first;
  struct project * last;
}  workspace;

extern gchar * edition_action_names[3];
extern gchar * analyze_action_names[9];
extern GSimpleAction * edition_actions[3];
extern GSimpleAction * analyze_actions[9];
extern void add_action (GSimpleAction * action);
extern void remove_action (gchar * action_name);
extern void remove_edition_actions ();
extern void remove_edition_and_analyze_actions ();

extern GtkApplication * AtomesApp;
extern workspace workzone;
extern struct project * proj;
extern chemical_data * active_chem;
extern coord_info * active_coord;
extern cell_info * active_cell;
extern box_info * active_box;
extern image * active_image;
extern glwin * active_glwin;
extern struct project * active_project;
extern struct project * opengl_project;
extern element_data periodic_table_info[];

extern struct project * get_project_by_id (int p);
extern void opengl_project_changed (int id);
extern gboolean in_md_shaders (struct project * this_proj, int id);
extern void recreate_all_shaders (glwin * view);
extern gboolean is_atom_win_active (glwin * view);

extern G_MODULE_EXPORT void on_calc_bonds_released (GtkWidget * widg, gpointer data);
extern void update_rings_menus (glwin * view);
extern void clean_rings_data (int rid, glwin * view);
extern void update_chains_menus (glwin * view);
extern void clean_chains_data (glwin * view);
extern void clean_volumes_data (glwin * view);

extern void initcutoffs (chemical_data * chem, int species);
extern void cutoffsend (void);
extern void update_entry_int (GtkEntry * entry, int intval);
extern void update_entry_double (GtkEntry * entry, double doubleval);
extern void update_entry_long_double (GtkEntry * entry, double doubleval);
extern void update_entry_text (GtkEntry * entry, gchar * text);

extern int StringLength (char * str);

extern double * xsk;
extern gboolean * allocbool (int val);
extern gboolean ** allocdbool (int xal, int yal);
extern gboolean *** alloctbool (int xal, int yal, int zal);
extern int * allocint (int val);
extern int ** allocdint (int xal, int yal);
extern int *** alloctint (int xal, int yal, int zal);
extern int **** allocqint (int wal, int xal, int yal, int zal);
extern float * allocfloat (int  val);
extern float ** allocdfloat (int xal, int yal);
extern float *** alloctfloat (int xal, int yal, int zal);
extern double * allocdouble (int val);
extern double ** allocddouble (int xal, int yal);
extern double *** alloctdouble (int xal, int yal, int zal);
extern double **** allocqdouble (int wal, int xal, int yal, int zal);

extern GLuint * allocgluint (int val);
extern GLuint ** allocdgluint (int xal, int yal);
extern GLfloat ** allocdGLfloat (int xal, int yal);

extern gchar ** duplicate_strings (int num, gchar ** old_val);
extern int * duplicate_int (int num, int * old_val);
extern gboolean * duplicate_bool (int num, gboolean * old_val);
extern float * duplicate_float (int num, float * old_val);
extern double * duplicate_double (int num, double * old_val);
extern ColRGBA * duplicate_color (int num, ColRGBA * col);

extern void combo_text_append (GtkWidget * combo, gchar * text);
extern void combo_text_prepend (GtkWidget * combo, gchar * text);
extern GtkWidget * create_combo ();
extern GtkTextBuffer * add_buffer (GCallback handler, gpointer data, gchar * text);
extern GtkWidget * create_text_view (int dimx, int dimy, int edit, int mono,
                                     GCallback handler, gpointer data, gchar * text);

extern GdkPixbuf * convert_to_pixbuf (cairo_surface_t * surf);

extern void text_view_set_monospace (GtkWidget * view);
extern void gtk_label_align (GtkWidget * lab, float ax, float ay);

extern gchar * prepare_for_title (gchar * init);

extern void show_the_widgets (GtkWidget * widg);
extern gboolean is_the_widget_visible (GtkWidget * widg);
extern void widget_set_sensitive (GtkWidget * widg, gboolean sensitive);

extern GtkWidget * new_gtk_window ();
extern GtkWidget * dialogmodal (gchar * str, GtkWindow * parent);
extern GtkWidget * message_dialogmodal (gchar * message, gchar * title, GtkMessageType mtype, GtkButtonsType buttons, GtkWidget * parent);
extern GtkWidget * dialog_cancel_apply (gchar * title, GtkWidget * parent, gboolean resiz);
extern void run_this_gtk_native_dialog (GtkNativeDialog * dial, GCallback handler, gpointer data);
extern void run_this_gtk_dialog (GtkWidget * dial, GCallback handler, gpointer data);
extern void resize_this_window (GtkWidget * window, int x, int y);

extern GtkWidget * create_hscale (float min, float max, float delta,
                                  float val, int pos, int round, int size,
                                  GCallback handler, GCallback scroll_handler, gpointer data);
extern GtkWidget * create_vscale (float min, float max, float delta,
                                  float val, int pos, int round, int size,
                                  GCallback handler, GCallback scroll_handler, gpointer data);

extern void add_container_child (int type, GtkWidget * widg, GtkWidget * child);
extern void add_box_child_end (GtkWidget * widg, GtkWidget * child, gboolean expand, gboolean fill, int padding);
extern void add_box_child_start (int orientation, GtkWidget * widg, GtkWidget * child, gboolean expand, gboolean fill, int padding);
extern void add_menu_child (GtkWidget * widg, GtkWidget * child);
extern void add_item_child (GtkWidget * widg, GtkWidget * child);
extern GtkWidget * create_hsep ();
extern GtkWidget * create_hpaned ();
extern GtkWidget * create_vbox (int spacing);
extern GtkWidget * create_hbox (int spacing);
extern GtkWidget * dialog_get_content_area (GtkWidget * widg);
extern void layout_add_widget (GtkWidget * layout, GtkWidget * child, int x_pos, int y_pos);
extern GtkWidget * add_vbox_to_layout (GtkWidget * layout, int size_x, int size_y);
extern GtkWidget * create_layout (int x, int y);
#ifdef GTK3
extern GtkWidget * create_menu_item (gboolean add_mnemo, gchar * action);
extern GtkWidget * create_menu_item_from_widget (GtkWidget * widg, gboolean check, gboolean radio, gboolean status);
extern GtkWidget * menu_item_new_with_submenu (gchar * name, gboolean active, GtkWidget * sub_menu);
extern void add_menu_separator (GtkWidget * menu);
#endif

extern const gchar * entry_get_text (GtkEntry * entry);
extern GtkWidget * create_entry (GCallback handler, int dim, int cdim, gboolean key_release, gpointer data);


extern GtkWidget * stock_image (const gchar * stock_id);
extern void set_image_from_icon_name (GtkWidget * widg, gchar * icon);
extern GtkWidget * create_image_from_data (int format, gpointer item_image);

extern GtkWidget * gtk3_menu_item (GtkWidget * menu, gchar * name,
                                   int icon_format, gpointer item_icon,
                                   GCallback handler, gpointer data,
                                   gboolean accel, guint key, GdkModifierType mod,
                                   gboolean check, gboolean radio, gboolean status);
extern GtkWidget * gtk3_image_menu_item (gchar * name, int icon_format, gpointer item_icon,
                                         GCallback handler, gpointer data, gchar * accel,
                                         gboolean check, gboolean radio, gboolean status);

extern GtkWidget * add_advanced_item (GtkWidget * menu, GCallback handler, gpointer data, gboolean accel, guint key, GdkModifierType mod);

extern GtkWidget * markup_label (gchar * text,
                                 int dimx, int dimy,
                                 float ax, float ay);

extern ColRGBA gdkrgba_to_rgba (GdkRGBA colgdk);
extern GdkRGBA colrgba_togtkrgba (ColRGBA col);
extern ColRGBA get_button_color (GtkColorChooser * colob);
extern ColRGBA get_window_color (GtkWidget * color_win);
extern void set_color_chooser_color (GtkWidget * color_win, ColRGBA col);
extern void set_renderer_color (int tocol, GtkCellRenderer * renderer, ColRGBA col);

extern void button_set_image (GtkButton * but, gchar * text, int format, gpointer image);

extern GtkWidget * color_button (ColRGBA col,
                                 gboolean alpha,
                                 int dimx, int dimy,
                                 GCallback handler, gpointer data);

extern GtkWidget * font_button (gchar * font,
                                int dimx, int dimy,
                                GCallback handler, gpointer data);

extern GtkWidget * spin_button (GCallback handler_a,
                                double value, double start, double end, double step, int digits,
                                int dim,
                                gpointer data);

extern GtkWidget * check_button (gchar * text,
                                 int dimx, int dimy,
                                 gboolean state,
                                 GCallback handler,
                                 gpointer data);

extern GtkWidget * radio_button (gchar * text,
                                 int dimx, int dimy,
                                 gboolean state,
                                 GCallback handler,
                                 gpointer data);

extern GtkWidget * create_button (gchar * text,
                                  int image_format,
                                  gchar * image,
                                  int dimx, int dimy,
                                  int relief,
                                  GCallback handler,
                                  gpointer data);

extern GtkWidget * abox (GtkWidget * box, char * lab, int vspace);
extern GtkWidget * bbox (GtkWidget * box, char * lab);
extern GtkWidget * cbox (GtkWidget * box, char * lab);
extern GtkWidget * fbox (GtkWidget * box, char * lab);
extern GtkWidget * create_scroll (GtkWidget * box, int dimx, int dimy, int shadow);
extern GtkWidget * create_expander (gchar * name, gchar * file_img, int i);

#ifdef GTK4
extern void add_widget_gesture_and_key_action (GtkWidget * widget,
                                               gchar * cp_name, GCallback cp_handler, gpointer cp_data,
                                               gchar * cr_name, GCallback cr_handler, gpointer cr_data,
                                               gchar * kp_name, GCallback kp_handler, gpointer kp_data,
                                               gchar * mo_name, GCallback mo_handler, gpointer mo_data,
                                               gchar * sc_name, GCallback sc_handler, gpointer sc_data);
#endif
extern GtkWidget * create_win (gchar * str, GtkWidget * parent, gboolean modal, gboolean resiz);

extern void widget_add_action (GSimpleActionGroup * action_group, const gchar * act, GCallback handler, gpointer data,
                               gboolean check, gboolean status, gboolean radio, const gchar * stat);
extern void append_menu_item (GMenu * menu, const gchar * label, const gchar * action, const gchar * accel,
                              const gchar * custom, int format, const gchar * icon,
                              gboolean check, gboolean status, gboolean radio, const gchar * rstatus);

extern void check_menu_item_set_active (gpointer item, gboolean active);
extern gboolean check_menu_item_get_active (gpointer item);

#ifdef GTK4
extern void menu_item_set_submenu (GMenu * menu, GMenu * sub_menu);
#else
extern void menu_item_set_submenu (GtkWidget * item, GtkWidget * sub_menu);
#endif
extern GtkWidget * destroy_this_widget (GtkWidget * widg);
extern void destroy_this_dialog (GtkDialog * dialog);
extern void destroy_this_native_dialog (GtkNativeDialog * dialog);
extern G_MODULE_EXPORT void run_destroy_dialog (GtkDialog * dialog, gint response_id, gpointer data);

#ifdef GTK4
extern G_MODULE_EXPORT gboolean destroy_this_window (GtkWindow * win, gpointer data);
extern G_MODULE_EXPORT gboolean hide_this_window (GtkWindow * win, gpointer data);
#else
extern G_MODULE_EXPORT gboolean destroy_this_window (GtkWidget * win, GdkEvent * event, gpointer data);
extern G_MODULE_EXPORT gboolean hide_this_window (GtkWidget * win, GdkEvent * event, gpointer data);
#endif
extern void add_gtk_close_event (GtkWidget * widg, GCallback handler, gpointer data);

#ifdef GTK4
GListModel * file_chooser_get_file_names (GtkFileChooser * chooser);
#else
GSList * file_chooser_get_file_names (GtkFileChooser * chooser);
#endif
gchar * file_chooser_get_file_name (GtkFileChooser * chooser);
gchar * file_chooser_get_current_folder (GtkFileChooser * chooser);
extern gboolean file_chooser_set_file_name (GtkFileChooser * chooser, gchar * filename);
extern void file_chooser_set_current_folder (GtkFileChooser * chooser);
#ifdef GTK4
extern GtkFileChooserNative * create_file_chooser (const gchar * title, GtkWindow * parent, GtkFileChooserAction act, const gchar * act_name);
extern void pop_menu_at_pointer (GtkWidget * pop, double x, double y);
#else
extern GtkWidget * create_file_chooser (const gchar * title, GtkWindow * parent, GtkFileChooserAction act, const gchar * act_name);
extern void pop_menu_at_pointer (GtkWidget * widg, GdkEvent * event);
#endif

extern GtkWidget * get_top_level (GtkWidget * widg);

extern void provide_gtk_css (gchar * css);
extern double get_calc_time (struct timespec start, struct timespec stop);
extern gchar * calculation_time (gboolean modelv, double ctime);

typedef struct {
  GCallback handler;
  gpointer data;
} focus_data;

#ifdef GTK4
extern void update_menu_bar (glwin * view);
#endif

#endif  // GLOBAL_H_
