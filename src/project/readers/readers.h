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

*/

#ifndef READERS_H_

#define READERS_H_

#include "read_isaacs.h"

extern int set_v_dummy (gchar * this_word);

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

extern void add_reader_info (gchar * info, int mid);
extern void reader_info (gchar * type, gchar * sinf, int val);
extern void format_error (int stp, int ato, gchar * mot, int line);
extern void check_for_species (double v, int ato);
#endif
