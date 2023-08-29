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
* This file: 'atom_geo.c'
*
*  Contains:
*

 - The subroutines to insert a new atom coordination type during model edition

*
*  List of subroutines:

  int new_geo (int id, coord_info * obj, int * old_z, int old_geo, int old_sp, int new_sp, coord_info * coord, double * new_z);

  gboolean is_in_atom_list (int aid, struct atom * new_list);

  void sort_partial_geo (int ** geom, int num_a);
  void check_coord_modification (struct project * this_proj, int * old_id, struct atom * new_list,
                                 struct insert_object * this_object, gboolean movtion, gboolean passivating);
*/

#include "atom_edit.h"

/*
*  void sort_partial_geo (int ** geom, int num_a)
*
*  Usage: sort partial geometries
*
*  int ** geom : the data to sort
*  int num_a   : the number of data point
*/
void sort_partial_geo (int ** geom, int num_a)
{
  int i, j, k, l;

  for(i=0;i<num_a;i++)
  {
    for(j=i+1;j<num_a;j++)
    {
      if(geom[i][0] > geom[j][0])
      {
        for (k=0; k<2; k++)
        {
          l = geom[i][k];
          geom[i][k] = geom[j][k];
          geom[j][k] = l;
        }
      }
    }
  }
}

/*
*  int new_geo (int id, coord_info * obj, int * old_z, int old_geo, int old_sp, int new_sp, coord_info * coord, double * new_z)
*
*  Usage: create a new geometry, for coordination type 'id' and chemical species 'new_sp'
*
*  int id             : the new coordination type (0 = total, 1 = partial)
*  coord_info * obj   : the new coordination info
*  int * old_z        : old Z list
*  int old_geo        : the old coordination id
*  int old_sp         : the old chemical species id
*  int new_sp         : the new chemical species id
*  coord_info * coord : the old coordination info
*  double * new_z     : new Z list
*/
int new_geo (int id, coord_info * obj, int * old_z, int old_geo, int old_sp, int new_sp, coord_info * coord, double * new_z)
{
  int i, j, k, l, m, n;
  int ** n_part, ** o_part;
  i = coord -> ntg[id][new_sp];
  for (j=0; j<i; j++)
  {
    k = obj -> geolist[id][old_sp][old_geo];
    if (coord -> geolist[id][new_sp][j] == k)
    {
      if (! id)
      {
        return j;
      }
      k = 0;
      for (l=0; l<obj -> species; l++)
      {
        if (obj -> partial_geo[old_sp][old_geo][l]) k ++;
      }
      l = 0;
      for (m=0; m<coord -> species; m++)
      {
        if (coord -> partial_geo[new_sp][j][m]) l ++;
      }
      if (k == l)
      {
        // Same number of atoms in the coordination and num of spec
        // 2 structure de l size, [0]= Z, [1] = nato
        // Tri par Z
        // Comp
        n_part = allocdint (l, 2);
        o_part = allocdint (l, 2);
        l = 0;
        for (k=0; k<coord -> species; k++)
        {
          if (coord -> partial_geo[new_sp][j][k])
          {
            n_part[l][0] = (int)new_z[k];
            n_part[l][1] = coord -> partial_geo[new_sp][j][k];
            l ++;
          }
        }
        if (l > 1) sort_partial_geo (n_part, l);
        l = 0;
        for (k=0; k<obj -> species; k++)
        {
          if (obj -> partial_geo[old_sp][old_geo][k])
          {
            o_part[l][0] = old_z[k];
            o_part[l][1] = obj -> partial_geo[old_sp][old_geo][k];
            l ++;
          }
        }
        if (l > 1) sort_partial_geo (o_part, l);
        n = 1;
        for (k=0; k<l; k++)
        {
          if (o_part[k][0] != n_part[k][0] || o_part[k][1] != n_part[k][1])
          {
            n = 0;
            break;
          }
        }
        g_free (n_part);
        g_free (o_part);
        if (n)
        {
          return j;
        }
      }
    }
  }

  int * tmpgeol = allocint (i+1);
  for (j=0; j<i; j++) tmpgeol[j] = coord -> geolist[id][new_sp][j];
  tmpgeol[i] = obj -> geolist[id][old_sp][old_geo];
  if (id)
  {
    int ** tmp_part = NULL;
    if (coord -> ntg[id][new_sp])
    {
      tmp_part = g_malloc0 (coord -> ntg[id][new_sp]*sizeof*tmp_part);
      for (j=0; j<i; j++)
      {
        tmp_part[j] = duplicate_int (coord -> species, coord -> partial_geo[new_sp][j]);
      }
      g_free (coord -> partial_geo[new_sp]);
    }
    coord -> partial_geo[new_sp] = g_malloc0 ((coord -> ntg[id][new_sp]+1)*sizeof*coord -> partial_geo[new_sp]);
    if (tmp_part)
    {
      for (j=0; j<i; j++)
      {
        coord -> partial_geo[new_sp][j] = duplicate_int (coord -> species, tmp_part[j]);
      }
      g_free (tmp_part);
    }
    coord -> partial_geo[new_sp][i] = allocint (coord -> species);
    for (j=0; j<obj -> species; j++)
    {
      if (obj -> partial_geo[old_sp][old_geo][j] && old_z[j])
      {
        l = find_spec_id (coord -> species, old_z[j], new_z);
        coord -> partial_geo[new_sp][i][l] = obj -> partial_geo[old_sp][old_geo][j];
      }
    }
  }

  if (coord -> geolist[id][new_sp] != NULL) g_free (coord -> geolist[id][new_sp]);
  coord -> ntg[id][new_sp] ++;
  coord -> totcoord[id] ++;
  coord -> geolist[id][new_sp] = duplicate_int (coord -> ntg[id][new_sp], tmpgeol);
  g_free (tmpgeol);
  return i;
}

/*
*  gboolean is_in_atom_list (int aid, struct atom * new_list)
*
*  Usage: is atom in list ?
*
*  int aid                : atom id
*  struct atom * new_list : the atom list to check
*/
gboolean is_in_atom_list (int aid, struct atom * new_list)
{
  struct atom * tmp_new;
  tmp_new = new_list;
  while (tmp_new)
  {
    if (tmp_new -> id == aid) return TRUE;
    tmp_new = tmp_new -> next;
  }
  return FALSE;
}

/*
*  void check_coord_modification (struct project * this_proj, int * old_id, struct atom * new_list,
*                                 struct insert_object * this_object, gboolean movtion, gboolean passivating)
*
*  Usage: check atom coordination modification on edition
*
*  struct project * this_proj         : the target project
*  int * old_id                       : the old atom id list, if any
*  struct atom * new_list             : the new atom(s) list
*  struct insert_object * this_object : the object to insert, if any
*  gboolean movtion                   : move or remove = 1, else : 0
*  gboolean passivating               : passivate
*/
void check_coord_modification (struct project * this_proj, int * old_id, struct atom * new_list,
                               struct insert_object * this_object, gboolean movtion, gboolean passivating)
{
  struct atom * tmp_new;
  int g, h, i, j, k, l, m, n;
  gboolean correct_it;
  int * new_z = allocint (this_proj -> nspec);
  double * old_z;
  if (this_object) old_z = duplicate_double (this_proj -> nspec, this_proj -> chemistry -> chem_prop[CHEM_Z]);
  int * new_old_id;
  if (passivating) new_old_id = duplicate_int (this_proj -> natomes, old_id);
  for (i=0; i<this_proj -> nspec; i++)
  {
    new_z[i] = (int)this_proj -> chemistry -> chem_prop[CHEM_Z][i];
  }
  coord_info * new_coord = g_malloc0 (sizeof*new_coord);
  for (i=0; i<2; i++)
  {
    new_coord -> totcoord[i] = 1;
    new_coord -> ntg[i] = allocint (this_proj -> nspec);
    for (j=0; j<this_proj -> nspec; j++) new_coord -> ntg[i][j] = 1;
    new_coord -> geolist[i] = allocdint (this_proj -> nspec, 1);
  }
  new_coord -> species = this_proj -> nspec;
  new_coord -> partial_geo = alloctint (this_proj -> nspec, 1, this_proj -> nspec);
  g = (passivating) ? 2 : 1;
  for (h=0; h<g; h++)
  {
    tmp_new = new_list;
    while (tmp_new)
    {
      i = tmp_new -> sp;
      for (j=0; j<2; j++)
      {
        k = tmp_new -> coord[j];
        new_coord -> geolist[j][i][0] = this_proj -> coord -> geolist[j][i][k];
      }
      for (j=0; j<this_proj -> nspec; j++)
      {
        new_coord -> partial_geo[i][0][j] = this_proj -> coord -> partial_geo[i][k][j];
      }
      correct_it = FALSE;
      j = tmp_new -> id;
      for (k=0; k<tmp_new -> numv; k++)
      {
        l = tmp_new -> vois[k];
        if ((movtion && ((old_id[j] > 0 && old_id[l] < 0) || (old_id[j] < 0 && old_id[l] > 0)))
             || (! movtion && ! is_in_atom_list (l, new_list)))
        {
          correct_it = TRUE;
          if (! passivating || h)
          {
            m = this_proj -> atoms[0][l].sp;
            new_coord -> partial_geo[i][0][m] --;
            for (n=0; n<2; n++) new_coord -> geolist[n][i][0] --;
          }
        }
      }
      if (correct_it)
      {
        if (passivating && ! h)
        {
          if (old_id[j])
          {
            switch (this_proj -> modelgl -> search_widg[8] -> filter)
            {
              case 0:
                l = i;
                break;
              case 1:
                l = tmp_new -> numv;
                break;
              case 2:
                l = tmp_new -> coord[1];
                for (m=0; m<i; m++) l += this_proj -> coord -> ntg[1][m];
                break;
              default:
                l = tmp_new -> coord[this_proj -> modelgl -> search_widg[8] -> filter-1];
                break;
            }
            if (get_insert_object_by_origin (this_proj -> modelgl -> atom_win -> to_be_inserted[3], l, 0))  new_old_id[j] = abs(old_id[j]);
          }
        }
        else
        {
          for (j=0; j<2; j++)
          {
            if (this_object)
            {
              tmp_new -> coord[j] = new_geo (j, new_coord, new_z, 0, i, i, this_object -> coord, old_z);
            }
            else
            {
              tmp_new -> coord[j] = new_geo (j, new_coord, new_z, 0, i, i, this_proj -> modelgl -> atom_win -> coord, this_proj -> modelgl -> atom_win -> new_z);
            }
          }
        }
        correct_it = FALSE;
      }
      tmp_new = tmp_new -> next;
    }
    if (passivating && ! h)
    {
      for (i=0; i<this_proj -> natomes; i++) old_id[i]= new_old_id[i];
      g_free (new_old_id);
    }
  }
  if (this_object) g_free (old_z);
  g_free (new_coord);
}
