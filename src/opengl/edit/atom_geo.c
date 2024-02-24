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

  int is_this_a_new_geo (int id, coord_info * obj, int * old_z, int old_geo, int old_sp, int new_sp, coord_info * coord, double * new_z);

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
*  int is_this_a_new_geo (int gid, coord_info * obj, int * old_z, int old_geo, int old_sp, int new_sp, coord_info * coord, double * new_z)
*
*  Usage: if required create a new geometry, stored in coord, for coordination type 'gid' and chemical species 'new_sp', return geometry id
*
*  int gid            : the new coordination type (0 = total, 1 = partial)
*  coord_info * obj   : the new coordination info to update
*  int * old_z        : old Z list
*  int old_geo        : the old coordination id
*  int old_sp         : the old chemical species id
*  int new_sp         : the new chemical species id
*  coord_info * coord : the old coordination info
*  double * new_z     : new Z list
*/
int is_this_a_new_geo (int gid, coord_info * obj, int * old_z, int old_geo, int old_sp, int new_sp, coord_info * coord, double * new_z)
{
  int i, j, k, l, m, n, o;
  int ** n_part, ** o_part;

  // Number of coord of type id for spec new_sp
  i = coord -> ntg[gid][new_sp];
  // Using j to store the total number of neighbor(s) for the coordination to test
  // Using k to store the number of type(s) of neighbor(s) for the coordination to test
  j = k = 0;
  for (l=0; l<obj -> species; l++)
  {
    if (obj -> partial_geo[old_sp][old_geo][l])
    {
      k ++;
      j += obj -> partial_geo[old_sp][old_geo][l];
    }
  }
  // Then comparing with already stored data in coord
  for (l=0; l<i; l++)
  {
    switch (gid)
    {
      case 0:
        if (coord -> geolist[gid][new_sp][l] == obj -> geolist[gid][old_sp][old_geo]) return l;
        break;
      case 1:
        m = 0;
        n = 0;
        for (o=0; o<coord -> species; o++)
        {
          if (coord -> partial_geo[new_sp][l][o])
          {
            m ++;
            n += coord -> partial_geo[new_sp][l][o];
          }
        }
        if (j == n && k == m)
        {
          // Same number of atoms in the coordination for the spec
          // 2 structures of size l: [0]= Z, [1] = nato
          // Sort by Z, then comparing
          n_part = allocdint (m, 2);
          o_part = allocdint (m, 2);
          m = 0;
          for (n=0; n<coord -> species; n++)
          {
            if (coord -> partial_geo[new_sp][l][n])
            {
              n_part[m][0] = (int)new_z[n];
              n_part[m][1] = coord -> partial_geo[new_sp][l][n];
              m ++;
            }
          }
          if (m > 1) sort_partial_geo (n_part, m);
          m = 0;
          for (n=0; n<obj -> species; n++)
          {
            if (obj -> partial_geo[old_sp][old_geo][n])
            {
              o_part[m][0] = old_z[n];
              o_part[m][1] = obj -> partial_geo[old_sp][old_geo][n];
              m ++;
            }
          }
          if (m > 1) sort_partial_geo (o_part, m);
          n = 1;
          for (o=0; o<m; o++)
          {
            if (o_part[o][0] != n_part[o][0] || o_part[o][1] != n_part[o][1])
            {
              n = 0;
              break;
            }
          }
          g_free (n_part);
          g_free (o_part);
          if (n)  return l;
        }
        break;
    }
  }

  // If we keep going then this is a new type of coordination sphere
  int * tmpgeol = NULL;
  if (! gid)
  {
    tmpgeol = allocint (coord -> ntg[gid][new_sp]+1);
    for (j=0; j<coord -> ntg[gid][new_sp]; j++)
    {
      tmpgeol[j] = coord -> geolist[gid][new_sp][j];
    }
    tmpgeol[j] = obj -> geolist[gid][old_sp][old_geo];
  }
  else
  {
    int ** tmp_part = NULL;
    tmp_part = g_malloc0 (coord -> ntg[1][new_sp]*sizeof*tmp_part);
    for (j=0; j<coord -> ntg[1][new_sp]; j++)
    {
      tmp_part[j] = duplicate_int (coord -> species, coord -> partial_geo[new_sp][j]);
    }
    g_free (coord -> partial_geo[new_sp]);
    coord -> partial_geo[new_sp] = g_malloc0 ((coord -> ntg[1][new_sp]+1)*sizeof*coord -> partial_geo[new_sp]);
    if (tmp_part)
    {
      for (j=0; j<coord -> ntg[1][new_sp]; j++)
      {
        coord -> partial_geo[new_sp][j] = duplicate_int (coord -> species, tmp_part[j]);
      }
      g_free (tmp_part);
    }
    coord -> partial_geo[new_sp][j] = allocint (coord -> species);
    for (k=0; k<obj -> species; k++)
    {
      if (old_z[k])
      {
        l = find_spec_id (coord -> species, old_z[k], new_z);
        coord -> partial_geo[new_sp][j][l] = obj -> partial_geo[old_sp][old_geo][k];
      }
    }
  }
  coord -> ntg[gid][new_sp] ++;
  coord -> totcoord[gid] ++;
  if (! gid)
  {
    if (coord -> geolist[gid][new_sp] != NULL) g_free (coord -> geolist[gid][new_sp]);
    coord -> geolist[gid][new_sp] = duplicate_int (coord -> ntg[gid][new_sp], tmpgeol);
    g_free (tmpgeol);
  }
  return i;
}

/*
*  int find_this_geo_id (int id, coord_info * obj, int * old_z, int old_geo, int old_sp, int new_sp, coord_info * coord, double * new_z)
*
*  Usage: if required create a new geometry, stored in coord, for coordination type 'id' and chemical species 'new_sp', return geometry id
*
*  int id             : the new coordination type (0 = total, 1 = partial)
*  coord_info * obj   : the new coordination info to update
*  int * old_z        : old Z list
*  int old_geo        : the old coordination id for this coordination type
*  int old_sp         : the old chemical species id
*  int new_sp         : the new chemical species id
*  coord_info * coord : the old coordination info
*  double * new_z     : new Z list
*/
int find_this_geo_id (int gid, coord_info * obj, int * old_z, int old_geo, int old_sp, int new_sp, coord_info * coord, double * new_z)
{
  int i, j, k, l, m, n, o;
  int ** n_part, ** o_part;

  // Number of coord of type id for spec new_sp
  i = coord -> ntg[gid][new_sp];
  if (gid)
  {
    // Using j to store the total number of neighbor(s) for the coordination to test
    // Using k to store the number of type(s) of neighbor(s) for the coordination to test
    j = k = 0;
    for (l=0; l<obj -> species; l++)
    {
      if (obj -> partial_geo[old_sp][old_geo][l])
      {
        k ++;
        j += obj -> partial_geo[old_sp][old_geo][l];
      }
    }
  }

  // Then comparing with already stored data in coord
  for (l=0; l<coord -> ntg[gid][new_sp]; l++)
  {
    switch (gid)
    {
      case 0:
        if (coord -> geolist[0][new_sp][l] == obj -> geolist[0][old_sp][old_geo]) return l;
        break;
      case 1:
        m = 0;
        n = 0;
        for (o=0; o<coord -> species; o++)
        {
          if (coord -> partial_geo[new_sp][l][o])
          {
            m ++;
            n += coord -> partial_geo[new_sp][l][o];
          }
        }
        if (j == n && k == m)
        {
          // Same number of atoms in the coordination for the spec
          // 2 structures of size l: [0]= Z, [1] = nato
          // Sort by Z, then comparing
          n_part = allocdint (m, 2);
          o_part = allocdint (m, 2);
          m = 0;
          for (n=0; n<coord -> species; n++)
          {
            if (coord -> partial_geo[new_sp][l][n])
            {
              n_part[m][0] = (int)new_z[n];
              n_part[m][1] = coord -> partial_geo[new_sp][l][n];
              m ++;
            }
          }
          if (m > 1) sort_partial_geo (n_part, m);
          m = 0;
          for (n=0; n<obj -> species; n++)
          {
            if (obj -> partial_geo[old_sp][old_geo][n])
            {
              o_part[m][0] = old_z[n];
              o_part[m][1] = obj -> partial_geo[old_sp][old_geo][n];
              m ++;
            }
          }
          if (m > 1) sort_partial_geo (o_part, m);
          n = 1;
          for (o=0; o<m; o++)
          {
            if (o_part[o][0] != n_part[o][0] || o_part[o][1] != n_part[o][1])
            {
              n = 0;
              break;
            }
          }
          g_free (n_part);
          g_free (o_part);
          if (n) return l;
        }
        break;
    }
  }

  // If we keep going then this is a new type of coordination sphere
  int * tmpgeol = NULL;
  tmpgeol = allocint (coord -> ntg[gid][new_sp]+1);
  for (j=0; j<coord -> ntg[gid][new_sp]; j++)
  {
    tmpgeol[j] = coord -> geolist[gid][new_sp][j];
  }
  tmpgeol[j] = obj -> geolist[gid][old_sp][old_geo];

  if (gid)
  {
    int ** tmp_part = NULL;
    tmp_part = g_malloc0 (coord -> ntg[1][new_sp]*sizeof*tmp_part);
    for (j=0; j<coord -> ntg[1][new_sp]; j++)
    {
      tmp_part[j] = duplicate_int (coord -> species, coord -> partial_geo[new_sp][j]);
    }
    g_free (coord -> partial_geo[new_sp]);
    coord -> partial_geo[new_sp] = g_malloc0 ((coord -> ntg[1][new_sp]+1)*sizeof*coord -> partial_geo[new_sp]);
    if (tmp_part)
    {
      for (j=0; j<coord -> ntg[1][new_sp]; j++)
      {
        coord -> partial_geo[new_sp][j] = duplicate_int (coord -> species, tmp_part[j]);
      }
      g_free (tmp_part);
    }
    coord -> partial_geo[new_sp][j] = allocint (coord -> species);
    for (k=0; k<obj -> species; k++)
    {
      if (old_z[k])
      {
        l = find_spec_id (coord -> species, old_z[k], new_z);
        coord -> partial_geo[new_sp][j][l] = obj -> partial_geo[old_sp][old_geo][k];
      }
    }
  }
  coord -> ntg[gid][new_sp] ++;
  coord -> totcoord[gid] ++;
  if (coord -> geolist[gid][new_sp] != NULL) g_free (coord -> geolist[gid][new_sp]);
  coord -> geolist[gid][new_sp] = duplicate_int (coord -> ntg[gid][new_sp], tmpgeol);
  g_free (tmpgeol);
  return i;
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
  int * nvois = allocint (this_proj -> nspec);
  if (passivating) new_old_id = duplicate_int (this_proj -> natomes, old_id);
  for (i=0; i<this_proj -> nspec; i++)
  {
    new_z[i] = (int)this_proj -> chemistry -> chem_prop[CHEM_Z][i];
  }

  // first create a dummy coord structure to store an atom individual data
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
      // Fill the dummy coord with the new atom information
      for (j=0; j<2; j++) new_coord -> geolist[j][i][0] = tmp_new -> numv;
      k = tmp_new -> coord[1];
      for (j=0; j<this_proj -> nspec; j++)
      {
        new_coord -> partial_geo[i][0][j] = this_proj -> coord -> partial_geo[i][k][j];
      }
      j = tmp_new -> id;
      correct_it = FALSE;
      if (movtion)
      {
        for (k=0; k<tmp_new -> numv; k++)
        {
          l = tmp_new -> vois[k];
          if ((old_id[j] > 0 && old_id[l] < 0) || (old_id[j] < 0 && old_id[l] > 0))
          {
            correct_it = TRUE;
            // This neighbor will be moved / removed
            if (! passivating || h)
            {
              m = this_proj -> atoms[0][l].sp;
              // For the atom studied reduce the number of neighbors of that species:
              new_coord -> partial_geo[i][0][m] --;
              for (n=0; n<2; n++) new_coord -> geolist[n][i][0] --;
            }
          }
        }
      }
      else
      {
        for (k=0; k<this_proj -> nspec; k++) nvois[k]=0;
        for (k=0; k<tmp_new -> numv; k++)
        {
          l = tmp_new -> vois[k];
          m = this_object -> at_list[l].sp;
          nvois[m] ++;
        }
        l = 0;
        for (k=0; k<this_proj -> nspec; k++)
        {
          if (nvois[k] != new_coord -> partial_geo[i][0][k])
          {
            correct_it = TRUE;
            new_coord -> partial_geo[i][0][k] = nvois[k];
          }
          l += nvois[k];
        }
        for (k=0; k<2; k++) new_coord -> geolist[k][i][0] = l;
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
              tmp_new -> coord[j] = find_this_geo_id (j, new_coord, new_z, 0, i, i, this_object -> coord, old_z);
            }
            else
            {
              tmp_new -> coord[j] = find_this_geo_id (j, new_coord, new_z, 0, i, i, this_proj -> modelgl -> atom_win -> coord, this_proj -> modelgl -> atom_win -> new_z);
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
  g_free (nvois);
}
