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
* This header file: 'project.h'
*
*  Contains: 

*  Called by: 

  calc/calc.c
  curve/cedit.c
  curve/curve.c
  curve/datab.c
  curve/tab-1.c
  curve/tab-2.c
  curve/tab-3.c
  curve/tab-4.c
  gui/bdcall.c
  gui/calc_menu.c
  gui/callbacks.c
  gui/chainscall.c
  gui/edit_menu.c
  gui/grcall.c
  gui/gui.c
  gui/initc.c
  gui/interface.c
  gui/main.c
  gui/msdcall.c
  gui/ringscall.c
  gui/spcall.c
  gui/sqcall.c
  gui/tools.c
  gui/work_menu.c
  opengl/glview.c
  project/close_p.c
  project/debugio.c
  project/init_p.c
  project/open_p.c
  project/read_bond.c
  project/read_curve.c
  project/read_field.c
  project/read_mol.c
  project/read_opengl.c
  project/read_qm.c
  project/save_bond.c
  project/save_curve.c
  project/save_field.c
  project/save_mol.c
  project/save_opengl.c
  project/save_p.c
  project/save_qm.c
  project/update_p.c
  workspace/workinfo.c
  workspace/workspace.c
  calc/cp2k/cp2k_files.c
  calc/cp2k/cp2k_init.c
  calc/cpmd/cpmd_init.c
  calc/dl_poly/dlp_control.c
  calc/dl_poly/dlp_field.c
  calc/dl_poly/dlp_viz.c
  opengl/draw/image.c
  opengl/draw/movie.c
  opengl/edit/cbuild_action.c
  opengl/edit/cbuild_edit.c
  opengl/win/color_box.c
  opengl/win/glwindow.c
  opengl/win/menu_bar.c
  opengl/win/m_map.c
  opengl/win/m_quality.c
  opengl/win/m_style.c
  opengl/win/popup.c
  opengl/win/w_colors.c
  opengl/win/w_crystal.c
  opengl/win/w_cutoffs.c
  opengl/win/w_library.c
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

#ifndef PROJECT_H_
#define PROJECT_H_

#define IODEBUG FALSE

extern int num_bonds (int i);
extern int num_angles (int i);
extern int num_dihedrals (int i);

// Read
extern int read_atom_a (FILE * fp, struct project * this_proj, int s, int a);
extern int read_atom_b (FILE * fp, struct project * this_proj, int s, int a);
extern int read_opengl_image (FILE * fp, struct project * this_proj, image * img, int sid);
extern int read_project_curve (FILE * fp, int wid, int pid);
extern int read_mol (FILE * fp);
extern int read_bonding (FILE * fp);
extern int read_dlp_field_data (FILE * fp, struct project * this_proj);
extern int read_lmp_field_data (FILE * fp, struct project * this_proj);
extern int read_cpmd_data (FILE * fp, int cid, struct project * this_proj);
extern int read_cp2k_data (FILE * fp, int cid, struct project * this_proj);
extern gchar * read_this_string (FILE * fp);
extern void alloc_proj_data (struct project * this_proj,  int cid);
extern int open_project (FILE * fp, int wid);

// Save
extern int save_atom_a (FILE * fp, struct project * this_proj, int s, int a);
extern int save_opengl_image (FILE * fp, struct project * this_proj, image * img, int sid);
extern int save_project_curve (FILE * fp, int wid, struct project * this_proj, int rid, int cid);
extern int save_dlp_field_data (FILE * fp, struct project * this_proj);
extern int save_lmp_field_data (FILE * fp, struct project * this_proj);
extern int save_cpmd_data (FILE * fp, int cid, struct project * this_proj);
extern int save_cp2k_data (FILE * fp, int cid, struct project * this_proj);
extern int save_this_string (FILE * fp, gchar * string);
extern int save_mol (FILE * fp, struct project * this_proj);
extern int save_bonding (FILE * fp, struct project * this_proj);
extern int save_project (FILE * fp, struct project * this_proj, int wid);

extern G_MODULE_EXPORT void set_color_map (GtkWidget * widg, gpointer data);
extern int * save_color_map (glwin * view);
extern void restore_color_map (glwin * view, int * colm);
extern colormap * allocate_color_map (int pts, struct project * this_proj);
extern gboolean setup_custom_color_map (float * data, struct project * this_proj, gboolean init);
#ifdef GTK4
extern G_MODULE_EXPORT gboolean edit_tab (GtkWidget * widget, GdkEvent * event, gpointer fdata);
#else
extern G_MODULE_EXPORT gboolean edit_tab (GtkWidget * widget, GdkEventButton * event, gpointer fdata);
#endif
extern void init_curves_and_calc (struct project * this_proj);
extern void init_project (gboolean alloc_box);
extern int update_project ();
extern void clean_view ();
extern void view_buffer (GtkTextBuffer * buffer);
extern void update_insert_combos ();
extern void close_project (struct project * to_close);
extern G_MODULE_EXPORT void to_close_this_project (int to_activate, struct project * this_proj);
extern void hide_current_project (struct project * to_hide);
extern void active_project_changed (int id);
extern void opengl_project_changed (int id);
extern G_MODULE_EXPORT void on_close_activate (GtkWidget * widg, gpointer cdata);
extern void add_project ();

extern void debugiocurve (struct project * this_proj, gboolean win, int rid, int cid, gchar * iost);
extern void debugioproj (struct project * this_proj, gchar * iost);
#endif
