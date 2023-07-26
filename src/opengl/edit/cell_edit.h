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
* This header file: 'cell_edit.h'
*
*  Contains: 

*/

#include "global.h"
#include "bind.h"
#include "callbacks.h"
#include "interface.h"
#include "project.h"
#include "workspace.h"
#include "glview.h"
#include "glwindow.h"

#ifndef CELL_EDIT_H_
#define CELL_EDIT_H_

extern gchar * axis[3];
extern char * box_prop[2][3];
extern void allocatoms (struct project * this_proj);
extern void center_molecule (struct project * this_proj);
extern gchar * edit_names[7];
extern gchar * mot[2][2];
extern GtkWidget * advanced_coord_properties (glwin * view, int page);
extern gboolean run_distance_matrix (GtkWidget * widg, int calc, int up_ngb);
extern void translate (struct project * this_proj, int status, int axis, vec3_t trans);
extern void process_selected_atom (struct project * this_proj, glwin * view, int id, int ac, int se, int pi);
extern void preserve_ogl_selection (glwin * view);
extern void restore_ogl_selection (glwin * view);
extern void create_slab_lists (struct project * this_proj);
extern int is_selected;
extern int selected_aspec;

GtkWidget * create_cell_entries (struct project * this_proj, int i);
GtkWidget * create_shift_box (struct project * this_proj);
void display_density (cell_edition * cell, double vol, double dens, double adens);
void shift_it (vec3_t shift, int refresh, int proj);
void sens_superbut (struct project * this_proj);
void invert_selection (struct project * this_proj);

GtkWidget * shift_center_tab (struct project * this_proj);
GtkWidget * add_extra_cell_tab (glwin * view);
GtkWidget * supercell_tab (glwin * view);
GtkWidget * adjust_density_tab (struct project * this_proj);
GtkWidget * cut_in_model (struct project * this_proj);
GtkWidget * pixels_tab (struct project * this_proj);
extern void wrapping (glwin * view);
extern G_MODULE_EXPORT void super_cell_but (GtkButton * but, gpointer data);
#ifdef GTK4
extern G_MODULE_EXPORT void super_cell (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void edition_win (GSimpleAction * action, GVariant * parameter, gpointer data);
#else
extern G_MODULE_EXPORT void super_cell (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void edition_win (GtkWidget * widg, gpointer data);
#endif
extern atom_search * allocate_atom_search (int proj, int action, int searchid, int tsize);
#endif
