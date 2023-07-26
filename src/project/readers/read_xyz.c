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
* This file: 'read_xyz.c'
*
*  Contains:
*

 -  - The subroutines to read XYZ atomic coordinates

*
*  List of subroutines:

  int xyz_get_atom_coordinates ();
  int open_xyz_file (int linec);

*/

#include "global.h"
#include "glview.h"
#include "callbacks.h"
#include "interface.h"
#include "project.h"
#include "bind.h"
#include <omp.h>
#include "readers.h"

/*
*  int xyz_get_atom_coordinates ()
*
*  Usage: get the atomic coordinates from the XYZ file
*/
int xyz_get_atom_coordinates ()
{
  int i, j, k;
  double v;
  gchar * lia[4] = {"a", "b", "c", "d"};
  this_reader -> nspec = 0;
  active_project -> steps = this_reader -> steps;
  active_project -> natomes = this_reader -> natomes;
  allocatoms (active_project);
  this_reader -> z = allocdouble (1);
  this_reader -> nsps = allocint (1);
#ifdef OPENMP
  int res;
  int numth = omp_get_max_threads ();
  gboolean doatoms =  FALSE;
  gchar * saved_line;
  if (this_reader -> steps < numth)
  {
    if (numth >= 2*(this_reader -> steps-1))
    {
      doatoms = TRUE;
    }
    else
    {
      numth = this_reader -> steps;
    }
  }

  if (doatoms)
  {
    // OpenMP on atoms
    res = 0;
    for (i=0; i<this_reader -> steps; i++)
    {
      k = i*(this_reader -> natomes + 2) + 2;
      #pragma omp parallel for num_threads(numth) private(j,v,this_line,saved_line,this_word) shared(i,k,lia,coord_line,this_reader,active_project,res)
      for (j=0; j<this_reader -> natomes; j++)
      {
        if (res == 2) goto enda;
        this_line = g_strdup_printf ("%s", coord_line[k+j]);
        saved_line = g_strdup_printf ("%s", this_line);
        this_word = strtok_r (this_line, " ", & saved_line);
        if (! this_word)
        {
          format_error (i+1, j+1, lia[0], k+j);
          res = 2;
          goto enda;
        }
        v = get_z_from_periodic_table (this_word);
        if (v)
        {
          if (! i)
          {
            #pragma omp critical
            check_for_species (v, j);
          }
          this_word = strtok_r (NULL, " ", & saved_line);
          if (! this_word)
          {
            format_error (i+1, j+1, lia[1], k+j);
            res = 2;
            goto enda;
          }
          active_project -> atoms[i][j].x = atof(this_word);
          this_word = strtok_r (NULL, " ", & saved_line);
          if (! this_word)
          {
            format_error (i+1, j+1, lia[2], k+j);
            res = 2;
            goto enda;
          }
          active_project -> atoms[i][j].y = atof(this_word);
          this_word = strtok_r (NULL, " ", & saved_line);
          if (! this_word)
          {
            format_error (i+1, j+1, lia[3], k+j);
            res = 2;
            goto enda;
          }
          active_project -> atoms[i][j].z = atof(this_word);
        }
        else
        {
          format_error (i+1, j+1, lia[0], k+j);
          res = 2;
          goto enda;
        }
        g_free (this_line);
        enda:;
      }
      if (res == 2) break;
    }
  }
  else
  {
    res = 0;
    #pragma omp parallel for num_threads(numth) private(i,j,k,v,this_line,saved_line,this_word) shared(lia,coord_line,this_reader,active_project,res)
    for (i=0; i<this_reader -> steps; i++)
    {
      if (res == 2) goto ends;
      k = i*(this_reader -> natomes + 2) + 2;
      for (j=0; j<this_reader -> natomes; j++)
      {
        this_line = g_strdup_printf ("%s", coord_line[k+j]);
        saved_line = g_strdup_printf ("%s", this_line);
        this_word = strtok_r (this_line, " ", & saved_line);
        if (! this_word)
        {
          format_error (i+1, j+1, lia[0], k+j);
          res = 2;
          goto ends;
        }
        v = get_z_from_periodic_table (this_word);
        if (v)
        {
          if (! i) check_for_species (v, j);
          this_word = strtok_r (NULL, " ", & saved_line);
          if (! this_word)
          {
            format_error (i+1, j+1, lia[1], k+j);
            res = 2;
            goto ends;
          }
          active_project -> atoms[i][j].x = atof(this_word);
          this_word = strtok_r (NULL, " ", & saved_line);
          if (! this_word)
          {
            format_error (i+1, j+1, lia[1], k+j);
            res = 2;
            goto ends;
          }
          active_project -> atoms[i][j].y = atof(this_word);
          this_word = strtok_r (NULL, " ", & saved_line);
          if (! this_word)
          {
            format_error (i+1, j+1, lia[2], k+j);
            res = 2;
            goto ends;
          }
          active_project -> atoms[i][j].z = atof(this_word);
        }
        else
        {
          format_error (i+1, j+1, lia[3], k+j);
          res = 2;
          goto ends;
        }
        g_free (this_line);
      }
      ends:;
    }
  }
  g_free (coord_line);
  if (res == 2) return 2;
#else
  struct line_node * tmp_line;
  tail = head;
  k = 0;
  for (i=0; i<active_project -> steps; i++)
  {
    for (j=0; j<2; j++)
    {
      tmp_line = tail;
      tail = tail -> next;
      g_free (tmp_line);
      k ++;
    }
    for (j=0; j<active_project -> natomes; j++)
    {
      this_line = g_strdup_printf ("%s", tail -> line);
      this_word = strtok (this_line, " ");
      if (! this_word)
      {
        format_error (i+1, j+1, lia[0], k+j);
        return 2;
      }
      v = get_z_from_periodic_table (this_word);
      if (v)
      {
        if (! i) check_for_species (v, j);
        this_word = strtok (NULL, " ");
        if (! this_word)
        {
          format_error (i+1, j+1, lia[1], k+j);
          return 2;
        }
        active_project -> atoms[i][j].x = atof(this_word);
        this_word = strtok (NULL, " ");
        if (! this_word)
        {
          format_error (i+1, j+1, lia[2], k+j);
          return 2;
        }
        active_project -> atoms[i][j].y = atof(this_word);
        this_word = strtok (NULL, " ");
        if (! this_word)
        {
          format_error (i+1, j+1, lia[3], k+j);
          return 2;
        }
        active_project -> atoms[i][j].z = atof(this_word);
      }
      else
      {
        format_error (i+1, j+1, lia[0], k+j);
        return 2;
      }
      tmp_line = tail;
      tail = tail -> next;
      g_free (tmp_line);
      k ++;
    }
  }
#endif
  for (i=1; i<active_project -> steps; i++)
  {
    for (j=0; j<active_project -> natomes; j++)
    {
      active_project -> atoms[i][j].sp = active_project -> atoms[0][j].sp;
    }
  }
  return 0;
}

/*
*  int open_xyz_file (int linec)
*
*  Usage: open XYZ file
*
*  int linec : Number of lines in the file
*/
int open_xyz_file (int linec)
{
  int res;
#ifdef OPENMP
  this_line = g_strdup_printf ("%s", coord_line[0]);
  this_word = strtok (this_line, " ");
  if (! this_word)
  {
    add_reader_info ("Wrong file format - cannot find the number of atoms !");
    add_reader_info ("Wrong file format - first line is corrupted !");
    res = 2;
    goto end;
  }
  this_reader -> natomes = (int)atof(this_word);
  reader_info ("xyz", "Number of atoms", this_reader -> natomes);
  g_free (this_line);
  if (linec%(this_reader -> natomes + 2) != 0)
  {
    res = 2;
  }
  else
  {
    this_reader -> steps = linec / (this_reader -> natomes + 2);
    reader_info ("xyz", "Number of steps", this_reader -> steps);
    res = xyz_get_atom_coordinates ();
  }
#else
  this_line = g_strdup_printf ("%s", head -> line);
  this_word = strtok (this_line, " ");
  if (! this_word)
  {
    add_reader_info ("Wrong file format - cannot find the number of atoms !");
    add_reader_info ("Wrong file format - the first line is corrupted !");
    res = 2;
    goto end;
  }
  this_reader -> natomes = (int)atof(this_word);
  reader_info ("xyz", "Number of atoms", this_reader -> natomes);
  g_free (this_line);
  if (linec%(this_reader -> natomes + 2) != 0)
  {
    res = 2;
  }
  else
  {
    this_reader -> steps = linec / (this_reader -> natomes + 2);
    reader_info ("xyz", "Number of steps", this_reader -> steps);
    res = xyz_get_atom_coordinates ();
  }
#endif
  end:
  return res;
}
