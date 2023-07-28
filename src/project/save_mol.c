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
* This file: 'save_mol.c'
*
*  Contains:
*

 - The subroutines to write molecules information in the atomes project file format

*
*  List of subroutines:

  int save_atom_m (FILE * fp, struct project * this_proj, int s, int a);
  int save_this_mol (FILE * fp, struct project * this_proj, struct molecule * tmp);
  int save_mol (FILE * fp, struct project * this_proj);

*/

#include "global.h"
#include "project.h"

/*struct molecule {
  int id;                                 // Molecule id number
  int md;                                 // MD step
  int multiplicity;                       // Multiplicity
  int * fragments;                        // Fragments list
  int natoms;                             // Number of atoms
  int nspec;                              // Number of chemical species
  int * species;                          // Number of atom by species
  struct atom ** atoms;                   // The list of all atoms
  int nbonds;                             // Number of chemical bonds
  int ** pbonds;                          // Number of chemical bonds by geometries
  int nangles;                            // Number of bond angles
  int *** pangles;                        // Number of bond angles by geometries
  int ** lgeo;                            // list of coordination spheres (by species)
  struct molecule * next;
  struct molecule * prev;
};*/

/*
*  int save_atom_m (FILE * fp, struct project * this_proj, int s, int a)
*
*  Usage: save atom data to file
*
*  FILE * fp                  : the file pointer
*  struct project * this_proj : the target project
*  int s                      : the MD step
*  int a                      : the atom number
*/
int save_atom_m (FILE * fp, struct project * this_proj, int s, int a)
{
  if (fwrite (& this_proj -> atoms[s][a].coord[2], sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& this_proj -> atoms[s][a].coord[3], sizeof(int), 1, fp) != 1) return ERROR_RW;
  return OK;
}

/*
*  int save_this_mol (FILE * fp, struct project * this_proj, struct molecule * tmp)
*
*  Usage: save this molecule data to file
*
*  FILE * fp                  : the file pointer
*  struct project * this_proj : the target project
*  struct molecule * tmp      : the molecule that contains the data
*/
int save_this_mol (FILE * fp, struct project * this_proj, struct molecule * tmp)
{
  if (fwrite (& tmp -> id, sizeof(int), 1, fp) != 1) return 0;
  if (fwrite (& tmp -> md, sizeof(int), 1, fp) != 1) return 0;
  if (fwrite (& tmp -> multiplicity, sizeof(int), 1, fp) != 1) return 0;
  if (fwrite (tmp -> fragments, sizeof(int), tmp -> multiplicity, fp) != tmp -> multiplicity) return 0;
  if (fwrite (& tmp -> natoms, sizeof(int), 1, fp) != 1) return 0;
  if (fwrite (& tmp -> nspec, sizeof(int), 1, fp) != 1) return 0;
  if (fwrite (tmp -> species, sizeof(int), this_proj -> nspec, fp) != this_proj -> nspec) return 0;
  return 1;
}

/*
*  int save_mol (FILE * fp, struct project * this_proj)
*
*  Usage: save molecule information to file
*
*  FILE * fp                  : the file pointer
*  struct project * this_proj : the target project
*/
int save_mol (FILE * fp, struct project * this_proj)
{
  int i, j;
  i = 0;
  if (! this_proj -> modelfc)
  {
    if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_MOL;
    return OK;
  }
  i = 1;
  if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_MOL;
  for (i=1; i<4; i++)
  {
    if (fwrite (& this_proj -> coord -> totcoord[i], sizeof(int), 1, fp) != 1) return ERROR_MOL;
  }
  if (fwrite (this_proj -> modelfc -> mol_by_step, sizeof(int), this_proj -> steps, fp) != this_proj -> steps) return ERROR_MOL;
  for (i=0; i<this_proj -> steps; i++)
  {
    for (j=0; j<this_proj -> modelfc -> mol_by_step[i]; j++)
    {
      if (! save_this_mol (fp, this_proj, & this_proj -> modelfc -> mols[i][j])) return ERROR_MOL;
    }
  }
  for (i=0; i<this_proj -> steps; i++)
  {
    for (j=0; j< this_proj -> natomes; j++)
    {
      if (save_atom_m (fp, this_proj, i, j) != OK) return ERROR_MOL;
    }
  }
  return OK;
}
