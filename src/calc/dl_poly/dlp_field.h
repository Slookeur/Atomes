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
* This header file: 'dlp_field.h'
*
*  Contains: 

*  Called by: 

  gui/bdcall.c
  opengl/ogl_draw.c
  opengl/ogl_utils.c
  project/read_field.c
  project/save_field.c
  calc/dl_poly/dlp_active.c
  calc/dl_poly/dlp_atom.c
  calc/dl_poly/dlp_comp.c
  calc/dl_poly/dlp_control.c
  calc/dl_poly/dlp_copy.c
  calc/dl_poly/dlp_edit.c
  calc/dl_poly/dlp_ff_match.c
  calc/dl_poly/dlp_field.c
  calc/dl_poly/dlp_init.c
  calc/dl_poly/dlp_mol.c
  calc/dl_poly/dlp_print.c
  calc/dl_poly/dlp_viz.c
  calc/force_fields/force_fields.c
  calc/lammps/la_print.c
  opengl/draw/d_atoms.c
  opengl/draw/d_bonds.c
  opengl/draw/d_label.c
  opengl/draw/d_poly.c
  opengl/draw/d_selection.c

*/

#ifndef DLP_FIELD_H_
#define DLP_FIELD_H_

#include "global.h"

// See DL_POLY user manual for the meaning
// of the different parameters, key and values.

#define N_FIELDS 21

extern char * field_acro[N_FIELDS];

#define DL_ENERGY 5
#define DL_BONDS 11
#define DL_ANGLES 15
#define DL_DIHEDRAL 8
#define DL_INVERS 6
#define DL_TETH 3
#define DL_VDW 21
#define DL_METALS 9
#define DL_TERSOFFS 2
#define DL_THREEBODY 6
#define DL_FOURBODY 3
#define DL_EXTERNAL 13

#define LA_ENERGY 1
#define LA_BONDS 16
#define LA_ANGLES 19
#define LA_DIHEDRAL 14
#define LA_INVERS 11
#define LA_TETH 0
#define LA_VDW 0
#define LA_METALS 0
#define LA_TERSOFFS 0
#define LA_THREEBODY 0
#define LA_FOURBODY 0
#define LA_EXTERNAL 0

#define FTETH max(DL_TETH,LA_TETH)
#define FBONDS max(DL_BONDS,LA_BONDS)
#define FANGLES max(DL_ANGLES,LA_ANGLES)
#define FDIHEDRAL max(DL_DIHEDRAL,LA_DIHEDRAL)
#define FINVERS max(DL_INVERS,LA_INVERS)
#define FVDW max(DL_VDW,LA_VDW)
#define FMETALS max(DL_METALS,LA_METALS)
#define FTERSOFFS max(DL_TERSOFFS,LA_TERSOFFS)
#define FTHREEBODY max(DL_THREEBODY,LA_THREEBODY)
#define FFOURBODY max(DL_FOURBODY,LA_FOURBODY)
#define FEXTERNAL max(DL_EXTERNAL,LA_EXTERNAL)

#define DL_BONDS_P 4
#define DL_ANGLES_P 6
#define DL_DIHEDRAL_P 5
#define DL_INVERS_P 3
#define DL_TETH_P 3
#define DL_VDW_P 7
#define DL_METALS_P 9
#define DL_TERSOFFS_P 16
#define DL_THREEBODY_P 5
#define DL_FOURBODY_P 3
#define DL_EXTERNAL_P 6

#define LA_BONDS_P 5
#define LA_ANGLES_P 6
#define LA_DIHEDRAL_P 11
#define LA_INVERS_P 5
#define LA_TETH_P 0
#define LA_VDW_P 0
#define LA_METALS_P 0
#define LA_TERSOFFS_P 0
#define LA_THREEBODY_P 0
#define LA_FOURBODY_P 0
#define LA_EXTERNAL_P 0

#define FTETH_P max(DL_TETH_P,LA_TETH_P)
#define FBONDS_P max(DL_BONDS_P,LA_BONDS_P)
#define FANGLES_P max(DL_ANGLES_P,LA_ANGLES_P)
#define FDIHEDRAL_P max(DL_DIHEDRAL_P,LA_DIHEDRAL_P)
#define FINVERS_P max(DL_INVERS_P,LA_INVERS_P)
#define FVDW_P max(DL_VDW_P,LA_VDW_P)
#define FMETALS_P max(DL_METALS_P,LA_METALS_P)
#define FTERSOFFS_P max(DL_TERSOFFS_P,LA_TERSOFFS_P)
#define FTHREEBODY_P max(DL_THREEBODY_P,LA_THREEBODY_P)
#define FFOURBODY_P max(DL_FOURBODY_P,LA_FOURBODY_P)
#define FEXTERNAL_P max(DL_EXTERNAL_P,LA_EXTERNAL_P)

#define MOLIMIT 15
#define SEXTERN 20

#define AMBER94     0
#define AMBER96     1
#define AMBER98     2
#define AMBER99     3
#define CHARMM22P   4
#define CHARMM22M   5
#define CHARMM35E   6
#define CHARMM36C   7
#define CHARMM36G   8
#define CHARMM36L   9
#define CHARMM36N  10
#define CHARMM36P  11
#define CHARMM36M  12
#define CHARMMSI   13
#define CVFF       14
#define CVFF_AUG   15
#define CFF91      16
#define PCFF       17
#define COMPASS    18
#define OPLSAAP    19
#define OPLSAAR    20

enum dlp_atom_types {
  SPEC_ONLY = 0,
  TOTAL_COORD_AND_SPEC = 1,   // Total coord + spec
  PARTIAL_COORD_AND_SPEC = 2, // Partial coord + spec
  OTHER = 3
};

struct field_prop {
  int pid;
  int fpid;
  int key;
  int * aid;
  float * val;
  gboolean show;
  gboolean use;
  struct field_prop * next;
  struct field_prop * prev;
};

struct field_atom {
  int id;
  int fid;  // Id among force field
  int afid; // Atoms id among force field atoms
  // Type is Among dlp_atom_types
  int type;
  char * name;
  int num;
  int sp;
  float mass;
  float charge;
  int frozen;
  gboolean * frozen_id;
  gboolean show;
  // ID of atoms in project file
  int * list;
  // ID of atoms for the DL_POLY molecule
  int * list_id;
  struct field_atom * prev;
  struct field_atom * next;
};

struct field_shell {
  int id;
  // 0 = select the field atom to CS
  // 1 = 0= all, 1 = selection
  int ia[2];
  float z;
  float m;
  float k2;
  float k4;
  gboolean vdw;
  gboolean show;
  gboolean use;
  struct field_shell * prev;
  struct field_shell * next;
};

struct field_constraint {
  int id;
  int ia[2];
  float av;
  float length;
  gboolean show;
  gboolean use;
  struct field_constraint * prev;
  struct field_constraint * next;
};

struct field_pmf {
  int id;
  float av;
  float length;
  int num[2];
  int * list[2];
  float * weight[2];
  gboolean show;
  gboolean use;
  struct field_pmf * prev;
  struct field_pmf * next;
};

struct field_rigid {
  int id;
  int num;
  int * list;
  gboolean show;
  gboolean use;
  struct field_rigid * prev;
  struct field_rigid * next;
};

struct field_tethered {
  int id;
  int num;
  int key;
  float * val;
  gboolean show;
  gboolean use;
  struct field_tethered * prev;
  struct field_tethered * next;
};

// See field molecule for details
struct field_struct {
  // Struct id (0 = Bonds, 2 = Angles, 4 = dihedrals ...)
  int st;
  int id;
  int num;
  int * aid;  // Field atom id
  float av;
  struct field_prop * def;
  struct field_prop * other;
  struct field_struct * prev;
  struct field_struct * next;
};

// Non-bonded interactions follow
// N body (Vdw, Metal, Tersoff, 3b, 4b)
struct field_nth_body {
  // Body Id 0=Vdw, 1= Metal ...
  int bd;
  int id;
  int * fpid;
  int * na;  // atom a num
  int ** ma; // atom a mol id's
  int ** a;  // field atom a id's in mol
  int key;
  float * val;
  gboolean show;
  gboolean use;
  struct field_nth_body * prev;
  struct field_nth_body * next;
};

struct field_external{
  int id;
  int key;
  float * val;
  gboolean use;
  struct field_external * prev;
  struct field_external * next;
};

struct field_neighbor{
  int id;
  int num;
  int * vois;
  struct field_neighbor * prev;
  struct field_neighbor * next;
};

struct field_molecule{
  int id;
  gchar * name;
  struct molecule * mol;
  int multi;
  int * fragments;
  dint ** atoms_id;
  int atoms;
  struct field_atom * first_atom;
  int shells;
  struct field_shell * first_shell;
  int constraints;
  struct field_constraint * first_constraint;
  int pmfs;
  struct field_pmf * first_pmf;
  int rigids;
  struct field_rigid * first_rigid;
  int tethered;
  struct field_tethered * first_tethered;
  // 0 = bonds
  // 1 = bonds restraints
  // 2 = angles
  // 3 = angles restraints
  // 4 = diehdrals
  // 5 = impropers
  // 6 = torsional restraints
  // 7 = inversions
  int nstruct[8];
  struct field_struct * first_struct[8];
  gboolean show;
  gboolean show_id;
  struct field_molecule * next;
  struct field_molecule * prev;
};

extern GtkWidget * field_assistant;
extern gchar * celemts[MAXDATC];
extern GtkWidget * vbox_control (int f);

extern int selected_aspec;
extern gchar * fkeysw[2][16][21];
extern gchar * fnames[2][16][21];
extern int fvalues[2][15][21];
extern char * fvars_teth[2][FTETH][FTETH_P];
extern char * fvars_bond[2][FBONDS][FBONDS_P];
extern char * fvars_angle[2][FANGLES][FANGLES_P];
extern char * fvars_dihedral[2][FDIHEDRAL][FDIHEDRAL_P];
extern char * fvars_inversion[2][FINVERS][FINVERS_P];
extern char * fvars_vdw[2][FVDW][FVDW_P];
extern char * fvars_met[2][FMETALS][FMETALS_P];
extern char * fvars_ters[2][FTERSOFFS][FTERSOFFS_P];
extern char * fvars_tbd[2][FTHREEBODY][FTHREEBODY_P];
extern char * fvars_fbd[2][FFOURBODY][FFOURBODY_P];
extern char * fvars_fext[2][FEXTERNAL][FEXTERNAL_P];
extern float internal_to_other[5];

extern classical_field * tmp_field;
extern glwin * tmp_view;
extern coord_info * tmp_coord;
extern struct project * tmp_proj;
extern struct field_molecule * tmp_fmol;
extern struct molecule * tmp_mol;
extern struct field_atom * tmp_fat, * tmp_fbt, * tmp_fct, * tmp_fdt;
extern struct field_shell * tmp_fshell;
extern struct field_constraint * tmp_fcons;
extern struct field_pmf * tmp_fpmf;
extern struct field_rigid * tmp_frig;
extern struct field_tethered * tmp_ftet;
extern struct field_prop * tmp_fprop;
extern struct field_struct * tmp_fstr;
extern struct field_nth_body * tmp_fbody;
extern struct field_nth_body * comp_fbody;
extern struct field_external * tmp_fext;


extern int row_id;
extern tint toviz;
extern int field_v[MAXDATA];

extern GtkWidget * mol_box[MOLIMIT-1];
extern GtkWidget * combo_mol[MOLIMIT-1];
extern GtkTreeStore * field_model[MAXDATA];
extern GtkWidget * field_tree[MAXDATA];
extern GtkWidget * mol_num_label;
extern GtkTreeIter field_iter;

extern gboolean afp[MAXDATA];
extern int field_object;
extern gboolean field_color;
extern int num_field_objects;
extern int saved_label_format[2];

extern int struct_id (int f);
extern int body_at (int b);

// Init field elements
extern int prepare_field_struct (int ids, int sid, int yes_no_num, int * aid);
extern int test_for_bonds (struct field_atom * at, struct field_atom * bt);
extern int test_for_angles (struct field_atom * at,
                            struct field_atom * bt,
                            struct field_atom * ct);
extern int test_for_dihedrals (struct field_atom * at,
                               struct field_atom * bt,
                               struct field_atom * ct,
                               struct field_atom * dt);
extern void clean_field_struct_list (struct field_struct * stru);

// Create force field data structure
extern void set_mol_num_label (void);
extern G_MODULE_EXPORT void changed_mol_box (GtkComboBox * box, gpointer data);
extern void update_field_trees ();
extern GtkWidget * create_combo_mol (int f);
extern void fill_field_model (GtkTreeStore * store, int f, int m);
extern GtkWidget * create_field_tree (int f);
extern classical_field * create_force_field_data_structure (int ai);

// Duplicate field element:
extern int * duplicate_int (int num, int * old_val);
extern gboolean * duplicate_bool (int num, gboolean * old_val);
extern float * duplicate_float (int num, float * old_val);
extern struct field_atom * duplicate_field_atom (struct field_atom * old_fat);
extern struct field_neighbor * duplicate_field_neighbor (struct field_neighbor * old_ngb);
extern struct field_shell * duplicate_field_shell (struct field_shell * old_shell);
extern struct field_constraint * duplicate_field_constraint (struct field_constraint * old_cons);
extern struct field_rigid * duplicate_field_rigid (struct field_rigid * old_rig);
extern struct field_tethered * duplicate_field_tethered (struct field_tethered * old_tet);
extern struct field_prop * duplicate_field_prop (struct field_prop * old_prop, int ti);
extern void duplicate_other_prop (int oid, struct field_struct * old_fstr, struct field_struct * new_fstr);
extern struct field_struct * duplicate_field_struct (struct field_struct * old_fstr);
extern struct field_struct * duplicate_field_struct_list (struct field_struct * list_str, gboolean init);
extern struct field_nth_body * duplicate_field_nth_body (struct field_nth_body * old_nth_body);
extern struct field_molecule * duplicate_field_molecule (struct field_molecule * old_fmol);
extern classical_field * duplicate_dlpoly_field (classical_field * init_field);

// Init field element:
extern struct field_atom * init_field_atom (int id, int type, int at, int nat, int coo, int * list);
extern struct field_shell * init_field_shell (int id, int ia, int ib);
extern struct field_constraint * init_field_constraint (int id, int ia, int ib);
extern struct field_pmf * init_field_pmf (int id, int num[2], int * list[2], float * w[2]);
extern struct field_rigid * init_field_rigid (int id, int num, int * list);
extern struct field_tethered * init_field_tethered (int id, int num);
extern struct field_prop * init_field_prop (int ti, int key, gboolean show, gboolean use);
extern struct field_struct * init_field_struct (int st, int ai, int an, int * aid);
extern struct field_nth_body * init_field_nth_body (int bi, int bd, int * na, int ** ma, int ** ba);
extern struct field_external * init_field_external (int bi);
extern void init_all_field_struct (gboolean init);
extern int init_vdw (gboolean init);
extern void setup_field_molecule_neighbors (int i, struct project * this_proj);

// Get active field element:
extern int get_position_in_field_atom_from_model_id (int fat, int at);
extern int get_field_atom_id_from_model_id (struct field_molecule * fmol, int at);
extern struct field_molecule * get_active_field_molecule_from_model_id (struct project * this_proj, int aid);
extern int get_fragment_atom_id_from_model_id (struct field_molecule * fmol, int at);
extern struct field_molecule * get_active_field_molecule (int a);
extern struct field_nth_body * get_active_body (int a, int b);
extern struct field_external * get_active_external (int a);
extern struct field_atom * get_active_atom (int a, int b);
extern struct field_shell * get_active_shell (int a, int b);
extern struct field_constraint * get_active_constraint (int a, int b);
extern struct field_pmf * get_active_pmf (int a, int b);
extern struct field_rigid * get_active_rigid (int a, int b);
extern struct field_tethered * get_active_tethered (int a, int b);
extern struct field_prop * get_active_prop (struct  field_prop * pr, int a);
extern struct field_prop * get_active_prop_using_atoms (struct  field_prop * pr, int ti, int * ids);
extern struct field_struct * get_active_struct (int s, int a, int b);
extern int get_struct_id_from_atom_id (int ids, int * aid);

// Field objects visualization:
extern int get_field_objects (int id, int jd);
extern gboolean show_field_object (int id, int jd, int kd);
extern void visualize_object (int id, int jd, int kd);
extern G_MODULE_EXPORT void on_toggle_visualize_or_select_object (GtkCellRendererToggle * cell_renderer,
                                                                  gchar * string_path,
                                                                  gpointer data);
extern G_MODULE_EXPORT void visualize_or_select_all_elements (GtkTreeViewColumn * col, gpointer data);

// Field molecule callbacks
extern void adjust_field_prop (int fil, int sti, struct field_prop * tmp, int * ids, int key);
extern gchar * remove_text (int i, int j, gchar * str);
extern G_MODULE_EXPORT void select_atom_id_from_fied_molecule (GtkButton * but, gpointer data);
extern G_MODULE_EXPORT void edit_field_prop (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void add_field_prop (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void remove_field_prop (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void add_molecule_to_field (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void remove_molecule_from_field (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void remove_atom_from_field_molecule (GSimpleAction * action, GVariant * parameter, gpointer data);

// OGL utils
extern struct distance distance_3d (cell_info * cell, int mdstep, struct atom * at, struct atom * bt);
extern struct angle angle_3d (cell_info * cell, int mdstep, struct atom * at, struct atom * bt, struct atom * ct);
extern struct angle dihedral_3d (cell_info * cell, int mdstep, struct atom * at, struct atom * bt, struct atom * ct, struct atom * dt);
extern struct angle inversion_3d (cell_info * cell, int mdstep, struct atom * at, struct atom * bt, struct atom * ct, struct atom * dt);

// Print
extern gchar * parameters_info (int obj, int key,  gchar ** words, float * data);

extern void print_dlp_field (GtkTextBuffer * buf);
extern void print_dlp_config (GtkTextBuffer * buf);
extern void print_dlp_control (GtkTextBuffer * buf);
extern void print_dlp_improper_inversion (int di, GtkTextBuffer * buf,
                                          struct field_struct * dh, int fi,
                                          GtkTreeStore * store, GtkTreeIter * iter);
extern void print_dlp_dihedral (int di, GtkTextBuffer * buf,
                                struct field_struct * dh, int fi,
                                GtkTreeStore * store, GtkTreeIter * iter);
extern void print_dlp_angle (int ai, GtkTextBuffer * buf,
                             struct field_struct * an, int fi,
                             GtkTreeStore * store, GtkTreeIter * iter);
extern void print_dlp_bond (int bi, GtkTextBuffer * buf,
                            struct field_struct * bd, int fi,
                            GtkTreeStore * store, GtkTreeIter * iter);

extern G_MODULE_EXPORT void setup_this_force_field (int id);

struct object_match {
  int id;
  int obj;
  int oid;
  int type;
  gboolean use;
  struct object_match * next;
  struct object_match * prev;
};

extern struct object_match * field_objects_id[6];
extern struct object_match * tmp_obj_id;
extern float get_force_field_atom_mass (int sp, int num);

struct field_data{
  int key;
  int ** atoms_z;
  int ** atoms_id;
  int npar;
  float ** param;
  char ** info;
};

extern int ff_unit;
extern struct field_data * ff_bonds[3];
extern struct field_data * ff_angles[2];
extern struct field_data * ff_dih[2];
extern struct field_data * ff_imp;
extern struct field_data * ff_inv;
extern struct field_data * ff_vdw;
extern int ** extraz_id;
#endif
