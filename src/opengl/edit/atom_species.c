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
* This file: 'atom_species.c'
*
*  Contains:
*

 - The subroutines to look for new chemical species and modify the chemical information accordingly

*
*  List of subroutines:

  int find_spec_id (int s, int z, double * list_z);
  int search_for_new_spec (atom_edition * edit, struct insert_object * object);

  chemical_data * duplicate_chemical_data (int spec, chemical_data * chem);

*/

#include "atom_edit.h"

/*
*  chemical_data * duplicate_chemical_data (int spec, chemical_data * chem)
*
*  Usage: duplicate chemical data information
*
*  int spec             : the number of chemical species
*  chemical_data * chem : the chemical data to duplicate
*/
chemical_data * duplicate_chemical_data (int spec, chemical_data * chem)
{
  chemical_data * newchem = g_malloc0 (sizeof*chem);
  newchem -> label = g_malloc (spec*sizeof*newchem -> label);
  newchem -> element = g_malloc (spec*sizeof*newchem -> element);
  int i, j;
  for (i=0; i<spec; i++)
  {
    newchem -> label[i] = g_strdup_printf ("%s", chem -> label[i]);
    newchem -> element[i] = g_strdup_printf ("%s", chem -> element[i]);
  }
  newchem -> nsps = duplicate_int (spec, chem -> nsps);
  newchem -> formula = duplicate_int (spec, chem -> formula);
  newchem -> cutoffs = allocddouble (spec, spec);
  newchem -> grtotcutoff = chem -> grtotcutoff;
  newchem -> chem_prop = allocddouble (CHEM_PARAMS, spec);
  for (i=0; i<spec; i++)
  {
    for (j=0; j<spec; j++) newchem -> cutoffs[i][j] = chem -> cutoffs[i][j];
    for (j=0; j<CHEM_PARAMS; j++) newchem -> chem_prop[j][i] = chem -> chem_prop[j][i];
  }
  return newchem;
}

/*
*  int find_spec_id (int s, int z, double * list_z)
*
*  Usage: find species id based on Z
*
*  int s           : the number of chemical species
*  int z           : the target Z
*  double * list_z : the list of Z values
*/
int find_spec_id (int s, int z, double * list_z)
{
  int i;
  if (list_z)
  {
    for (i=0; i<s; i++)
    {
      if (list_z[i] == (double)z)
      {
        return i;
      }
    }
  }
  return -1;
}

/*
*  int search_for_new_spec (atom_edition * edit, struct insert_object * object)
*
*  Usage: search for new chemical species
*
*  atom_edition * edit           : the edition window
*  struct insert_object * object : the target insert object
*/
int search_for_new_spec (atom_edition * edit, struct insert_object * object)
{
  double * tmpnzid;

  coord_info * coord = edit -> coord;
  int i, j, k, l, m;
  i = 0;
  for (j=0; j<object -> species; j++)
  {
    if (object -> old_z[j] > 0)
    {
      k = find_spec_id (coord -> species, object -> old_z[j], edit -> new_z);
      if (k < 0)
      {
        i ++;
        tmpnzid = allocdouble (coord -> species+i);
        for (l=0; l<coord -> species+i-1; l++) tmpnzid[l] = edit -> new_z[l];
        tmpnzid[l] = (double)object -> old_z[j];
        if (edit -> new_z) g_free (edit -> new_z);
        edit -> new_z = duplicate_double (coord -> species+i, tmpnzid);
        g_free (tmpnzid);
      }
    }
  }

  if (i)
  {
    coord_info * tmp = duplicate_coord_info (edit -> coord);
    for (j=0; j<2; j++)
    {
      if (coord -> species)
      {
        g_free (coord -> ntg[j]);
        g_free (coord -> geolist[j]);
      }
      coord -> ntg[j] = allocint (coord -> species + i);
      coord -> geolist[j] = g_malloc0 ((coord -> species + i)*sizeof* coord -> geolist[j]);
      if (j)
      {
        if (coord -> species) g_free (coord -> partial_geo);
        coord -> partial_geo = g_malloc0 ((coord -> species + i)*sizeof*coord -> partial_geo);
      }
      for (k=0; k<coord -> species; k++)
      {
        coord -> ntg[j][k] = tmp -> ntg[j][k];
        coord -> geolist[j][k] = duplicate_int (tmp -> ntg[j][k], tmp -> geolist[j][k]);
        if (j)
        {
          coord -> partial_geo[k] = allocdint (coord -> ntg[j][k], coord -> species + i);
          for (l=0; l<tmp -> ntg[j][k]; l++)
          {
            for (m=0; m<coord -> species; m++)
            {
              coord -> partial_geo[k][l][m] = tmp -> partial_geo[k][l][m];
            }
          }
        }
      }
    }
    coord -> species += i;
    g_free (tmp);
  }
  return i;
}
