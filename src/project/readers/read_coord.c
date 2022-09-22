/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include "global.h"
#include "glview.h"
#include "callbacks.h"
#include "interface.h"
#include "project.h"
#include "gui.h"
#include "bind.h"
#include <omp.h>
#include "cbuild_edit.h"

extern int open_xyz_file (int linec);
extern int open_c3d_file (int linec);
extern int open_pdb_file (int linec);
extern int open_trj_file (int linec);
extern int open_vas_file (int linec);
extern int open_cif_file (int linec);
extern int open_hist_file (int linec);
extern void allocatoms (struct project * this_proj);
extern chemical_data * alloc_chem_data (int spec);
extern int build_crystal (gboolean visible, struct project * this_proj, gboolean to_wrap, gboolean show_clones, cell_info * cell, GtkWidget * widg);
extern const gchar * dfi[2];

FILE * coordf;
coord_file * this_reader;
gchar ** coord_line = NULL;
gchar * this_line = NULL;
char * this_word;
struct line_node * head = NULL;
struct line_node * tail = NULL;

void add_reader_info (gchar * info)
{
  this_reader -> info = (this_reader -> info) ? g_strdup_printf ("%s\n%s", this_reader -> info, info) : g_strdup_printf ("%s", info);
}

void reader_info (gchar * type, gchar * sinf, int val)
{
  g_print ("Reading coordinates [%s]: %s = %d\n", type, sinf, val);
}

void format_error (int stp, int ato, gchar * mot, int line)
{
  gchar * str;
  if (ato < 0)
  {
    str = g_strdup_printf ("Wrong file format: error at step %d !\n"
                           "Wrong file format: record <b>%s</b> on line <b>%d</b> is corrupted !",
                           stp+1, mot, line);
  }
  else
  {
    str = g_strdup_printf ("Wrong file format: error at step %d, atom %d !\n"
                           "Wrong file format: record <b>%s</b> on line <b>%d</b> is corrupted !",
                           stp+1, ato+1, mot, line);
  }
  add_reader_info (str);
  g_free (str);
}

void check_for_species (double v, int ato)
{
  int i;
  gboolean add_spec;
  add_spec = TRUE;
  if (this_reader -> nspec)
  {
    for (i=0; i<this_reader -> nspec; i++)
    {
      if (this_reader -> z[i] == v)
      {
        if (this_reader -> cartesian)
        {
          active_project -> atoms[0][ato].sp = i;
        }
        else
        {
          this_reader -> lot[ato] = i;
        }
        this_reader -> nsps[i] ++;
        add_spec = FALSE;
        break;
      }
    }
  }
  if (add_spec)
  {
    if (this_reader -> nspec)
    {
      this_reader -> z = g_realloc (this_reader -> z, (this_reader -> nspec+1)*sizeof*this_reader -> z);
      this_reader -> nsps = g_realloc (this_reader -> nsps, (this_reader -> nspec+1)*sizeof*this_reader -> nsps);
    }
    if (this_reader -> cartesian)
    {
      active_project -> atoms[0][ato].sp = this_reader -> nspec;
    }
    else
    {
      this_reader -> lot[ato] = this_reader -> nspec;
    }
    this_reader -> nsps[this_reader -> nspec] = 1;
    this_reader -> z[this_reader -> nspec] = v;
    this_reader -> nspec ++;
  }
}

int open_coord_file (gchar * filename, int fti)
{
  int res;
#ifdef OPENMP
  struct stat status;
  res = stat (filename, & status);
  if (res == -1)
  {
    add_reader_info ("Error - cannot get file statistics !");
    return 1;
  }
  int fsize = status.st_size;
#endif
  coordf = fopen (filename, dfi[0]);
  if (! coordf)
  {
    add_reader_info ("Error - cannot open coordinates file !");
    return 1;
  }
  int i, j, k;
#ifdef OPENMP
  gchar * coord_content = g_malloc0(fsize*sizeof*coord_content);
  fread (coord_content, fsize, 1, coordf);
  fclose (coordf);
  int linecount = 0;
  for (j=0; j<fsize; j++) if (coord_content[j] == '\n') linecount ++;
  coord_line = g_malloc0 (linecount*sizeof*coord_line);
  coord_line[0] = & coord_content[0];
  i = 1;
  for (j=0; j<fsize; j++)
  {
    if (coord_content[j] == '\n')
    {
      coord_content[j] = '\0';
      if (i < linecount)
      {
        coord_line[i] = & coord_content[j+1];
        i ++;
      }
    }
  }
#else
  gchar * buf = g_malloc0(LINE_SIZE*sizeof*buf);
  head = NULL;
  tail = NULL;
  i = 0;
  while (fgets(buf, LINE_SIZE, coordf))
  {
    if (head == NULL)
    {
      head = g_malloc0 (sizeof*head);
      tail = g_malloc0 (sizeof*tail);
      tail = head;
    }
    else
    {
      tail -> next = g_malloc0 (sizeof*tail -> next);
      if (fti == 9)
      {
        tail -> next -> prev = g_malloc0 (sizeof*tail -> next -> prev);
        tail -> next -> prev = tail;
      }
      tail = tail -> next;
    }
    tail -> line = g_strdup_printf ("%s", buf);
    tail -> line = substitute_string (tail -> line, "\n", "\0");
    i ++;
  }
  g_free (buf);
  fclose (coordf);
#endif
  if (i)
  {
    this_reader -> cartesian = TRUE;
    if (fti < 2)
    {
      res = open_xyz_file (i);
    }
    else if (fti == 2)
    {
      res = open_c3d_file (i);
    }
    else if (fti < 5)
    {
      res = open_trj_file (i);
    }
    else if (fti < 7)
    {
      res = open_vas_file (i);
    }
    else if (fti > 6 && fti < 9)
    {
      res = open_pdb_file (i);
    }
    else if (fti == 9)
    {
      this_reader -> cartesian = FALSE;
      res = open_cif_file (i);
    }
    else if (fti == 10)
    {
      res = open_hist_file (i);
    }
  }
  else
  {
    res = 1;
  }
#ifndef OPENMP
  if (tail) g_free (tail);
#endif
  if (! res)
  {
    if (fti == 9)
    {
      if (! this_reader -> cartesian)
      {
        // this_reader -> lattice.sp_group -> sid = 2;
        // get_origin (this_reader -> lattice.sp_group);
        if (! build_crystal (FALSE, active_project, TRUE, FALSE, & this_reader -> lattice, MainWindow))
        {
          add_reader_info ("Error trying to build crystal using the CIF file parameters !");
          res = 3;
        }
      }
    }
    for (i=0; i<active_project -> steps; i++)
    {
      for (j=0; j<active_project -> natomes; j++)
      {
        active_project -> atoms[i][j].id = j;
        active_project -> atoms[i][j].show[0] = TRUE;
        active_project -> atoms[i][j].show[1] = TRUE;
        active_project -> atoms[i][j].label[0] = FALSE;
        active_project -> atoms[i][j].label[1] = FALSE;
        active_project -> atoms[i][j].pick[0] = FALSE;
        active_project -> atoms[i][j].cloned = FALSE;
      }
    }
    if (fti != 9 || this_reader -> cartesian)
    {
      active_project -> nspec = this_reader -> nspec;
      active_project -> chemistry = alloc_chem_data (active_project -> nspec);
      active_project_changed (activep);
      k = 0;
      reader_info (file_ext[fti], "Number of species", active_project -> nspec);
      for (i=0; i<active_project -> nspec; i++)
      {
        j = (int)this_reader -> z[i];
        active_chem -> label[i] = g_strdup_printf ("%s", periodic_table_info[j].lab);
        active_chem -> element[i] = g_strdup_printf ("%s", periodic_table_info[j].name);
        active_chem -> nsps[i] = this_reader -> nsps[i];
        g_print ("Reading coordinates [%s]:\t %s, nsps[%d]= %d\n", file_ext[fti], active_chem -> label[i], i+1, active_chem -> nsps[i]);
        active_chem -> chem_prop[CHEM_Z][i] = this_reader -> z[i];
        active_chem -> chem_prop[CHEM_M][i] = set_mass_ (& j);
        active_chem -> chem_prop[CHEM_R][i] = set_radius_ (& j, & k);
        active_chem -> chem_prop[CHEM_N][i] = set_neutron_ (& j);
      }
    }
    else
    {
      reader_info (file_ext[fti], "Number of species", active_project -> nspec);
      for (i=0; i<active_project -> nspec; i++)
      {
        g_print ("Reading coordinates [%s]:\t %s, nsps[%d]= %d\n", file_ext[fti], active_chem -> label[i], i+1, active_chem -> nsps[i]);
      }
    }
  }
  return res;
}
