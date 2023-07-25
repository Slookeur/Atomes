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
* This header file: 'interface.h'
*
*  Contains: 

*  Called by: 

  calc/calc.c
  curve/cedit.c
  curve/curve.c
  curve/cwidget.c
  curve/datab.c
  curve/tab-1.c
  curve/tab-2.c
  curve/tab-3.c
  curve/tab-4.c
  curve/w_data.c
  curve/w_img.c
  gui/bdcall.c
  gui/calc_menu.c
  gui/callbacks.c
  gui/chainscall.c
  gui/edit_menu.c
  gui/grcall.c
  gui/gtk-misc.c
  gui/gui.c
  gui/main.c
  gui/msdcall.c
  gui/ringscall.c
  gui/spcall.c
  gui/sqcall.c
  gui/tools.c
  gui/work_menu.c
  gui/xmlrw.c
  lic/valid.c
  opengl/glview.c
  opengl/ogl_shading.c
  opengl/ogl_utils.c
  opengl/selection.c
  project/close_p.c
  project/init_p.c
  project/open_p.c
  project/project.c
  project/update_p.c
  workspace/modelinfo.c
  workspace/workinfo.c
  workspace/workspace.c
  calc/cp2k/cp2k_files.c
  calc/cp2k/cp2k_init.c
  calc/cp2k/cp2k_mol.c
  calc/cp2k/cp2k_print.c
  calc/cpmd/cpmd_atoms.c
  calc/cpmd/cpmd_init.c
  calc/cpmd/cpmd_nose.c
  calc/cpmd/cpmd_print.c
  calc/cpmd/cpmd_restart.c
  calc/dl_poly/dlp_atom.c
  calc/dl_poly/dlp_comp.c
  calc/dl_poly/dlp_control.c
  calc/dl_poly/dlp_edit.c
  calc/dl_poly/dlp_ff_match.c
  calc/dl_poly/dlp_field.c
  calc/dl_poly/dlp_init.c
  calc/dl_poly/dlp_mol.c
  calc/dl_poly/dlp_print.c
  calc/force_fields/force_fields.c
  calc/lammps/la_print.c
  opengl/draw/d_label.c
  opengl/draw/image.c
  opengl/draw/movie.c
  opengl/edit/cbuild_action.c
  opengl/edit/cbuild_edit.c
  opengl/edit/cbuild_info.c
  opengl/win/color_box.c
  opengl/win/initchain.c
  opengl/win/initcoord.c
  opengl/win/initmol.c
  opengl/win/m_coord.c
  opengl/win/menu_bar.c
  opengl/win/m_map.c
  opengl/win/m_poly.c
  opengl/win/m_quality.c
  opengl/win/m_rep.c
  opengl/win/popup.c
  opengl/win/w_advance.c
  opengl/win/w_atoms.c
  opengl/win/w_axis.c
  opengl/win/w_bonds.c
  opengl/win/w_box.c
  opengl/win/w_chains.c
  opengl/win/w_colors.c
  opengl/win/w_coord.c
  opengl/win/w_crystal.c
  opengl/win/w_cutoffs.c
  opengl/win/w_encode.c
  opengl/win/w_labels.c
  opengl/win/w_library.c
  opengl/win/w_rings.c
  opengl/win/w_sequencer.c
  opengl/win/w_volumes.c
  project/readers/read_c3d.c
  project/readers/read_cif.c
  project/readers/read_coord.c
  project/readers/read_hist.c
  project/readers/read_npt.c
  project/readers/read_pdb.c
  project/readers/read_trj.c
  project/readers/read_vas.c
  project/readers/read_xyz.c

*/

#ifndef INTERFACE_H_
#define INTERFACE_H_
G_MODULE_EXPORT void create_about_dialog (GtkWidget * widg, gpointer data);

void show_info (char * information, int val, GtkWidget * win);
void show_info_ (double * valdij);
void show_warning (char * warning, GtkWidget * win);
void show_warning_ (char * warning, char * sub, char * tab);
void show_error (char * error, int val, GtkWidget * win);
void show_error_ (char * error, char * sub, char * tab);
gboolean ask_yes_no (gchar * title, gchar * text, int type, GtkWidget * widg);
gchar * exact_name (gchar * name);
GtkWidget * show_pop (char * pop, GtkWidget * pwin);
#ifdef GTK4
G_MODULE_EXPORT gboolean leaving_question (GtkWindow * widget, gpointer data);
#else
G_MODULE_EXPORT gboolean leaving_question (GtkWidget * widget, GdkEvent * event, gpointer data);
#endif
int dummy_ask_ (char * question);
int iask (char * question, char * lab, int id, GtkWidget * win);
gchar * cask (char * question,  char * lab, int id, char * old, GtkWidget * win);

void print_info  (gchar * str, gchar * stag, GtkTextBuffer * buffer);
gchar * textcolor (int i);

void set_progress_ (gdouble * prog);

gchar * env_name (struct project * this_proj, int g, int s, int f, GtkTextBuffer * buffer);
void init_data_ (int * nats, int * nspc, int * stps, int * cid);
void update_after_calc (int calc);

// In init.c:

void prepostcalc (GtkWidget * widg, gboolean status, int run, int adv, double opc);
void prep_calc_actions ();
void initcwidgets ();
void initbonds ();
void initcnames (int w, int s);
#endif
