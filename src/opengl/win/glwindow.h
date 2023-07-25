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
* This header file: 'glwindow.h'
*
*  Contains: 

*  Called by: 

  calc/calc.c
  gui/callbacks.c
  gui/edit_menu.c
  project/init_p.c
  workspace/workspace.c
  calc/cp2k/cp2k_files.c
  calc/cp2k/cp2k_init.c
  calc/cpmd/cpmd_init.c
  calc/dl_poly/dlp_comp.c
  calc/dl_poly/dlp_control.c
  calc/dl_poly/dlp_edit.c
  calc/dl_poly/dlp_ff_match.c
  calc/dl_poly/dlp_field.c
  calc/dl_poly/dlp_viz.c
  opengl/draw/image.c
  opengl/draw/movie.c
  opengl/win/color_box.c
  opengl/win/initchain.c
  opengl/win/initcoord.c
  opengl/win/initmol.c
  opengl/win/initring.c
  opengl/win/m_anim.c
  opengl/win/m_atoms.c
  opengl/win/m_axis.c
  opengl/win/m_back.c
  opengl/win/m_bonds.c
  opengl/win/m_box.c
  opengl/win/m_clones.c
  opengl/win/m_coord.c
  opengl/win/m_map.c
  opengl/win/m_poly.c
  opengl/win/m_proj.c
  opengl/win/m_quality.c
  opengl/win/m_render.c
  opengl/win/m_rep.c
  opengl/win/m_style.c
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
  opengl/win/w_labels.c
  opengl/win/w_library.c
  opengl/win/w_rings.c
  opengl/win/w_sequencer.c

*/

extern void prep_model (int p);

extern void append_opengl_item (glwin * view, GMenu * menu, const gchar * name, gchar * key, int mpop, int item_id,
                                gchar * accel, int image_format, gpointer icon,
                                gboolean custom, GCallback handler, gpointer data,
                                gboolean check, gboolean status, gboolean radio, gboolean sensitive);

#ifdef GTK4
extern G_MODULE_EXPORT void to_reset_view (GSimpleAction * action, GVariant * parameter, gpointer data);
#else
extern G_MODULE_EXPORT void to_reset_view (GtkWidget * widg, gpointer data);
#endif
