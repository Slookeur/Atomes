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
* This header file: 'readers.h'
*
*  Contains: 

*  Called by: 

  gui/callbacks.c
  opengl/edit/cbuild_action.c
  opengl/edit/cbuild_edit.c
  project/readers/read_c3d.c
  project/readers/read_cif.c
  project/readers/read_hist.c
  project/readers/read_pdb.c
  project/readers/read_trj.c
  project/readers/read_vas.c
  project/readers/read_xyz.c

*/

#ifndef READERS_H_

#define READERS_H_

extern double get_z_from_periodic_table (gchar * lab);
extern void allocatoms (struct project * this_proj);
extern chemical_data * alloc_chem_data (int spec);
extern FILE * coordf;

extern coord_file * this_reader;
extern gchar * this_line;
extern char * this_word;
extern gchar ** coord_line;

extern struct line_node * head;
extern struct line_node * tail;

extern void add_reader_info (gchar * info);
extern void reader_info (gchar * type, gchar * sinf, int val);
extern void format_error (int stp, int ato, gchar * mot, int line);
extern void check_for_species (double v, int ato);
#endif
