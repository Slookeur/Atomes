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
* This file: 'save_qm.c'
*
*  Contains:
*

 - Subroutines to save ab-initio calculation (CPMD/CP2K) to atomes project file

*
*  List of subroutines:

  int save_thermo (FILE * fp, struct thermostat * thermo);
  int save_fixed_atoms (FILE * fp, int fixatoms, int * fixlist, int ** fixcoord);
  int save_cpmd_data (FILE * fp, int cid, struct project * this_proj);
  int save_cp2k_data (FILE * fp, int cid, struct project * this_proj);

*/

#include "global.h"
#include "project.h"

/*
struct thermostat {
  int id;
  // For CPMD: 0 = none, 2 = controlled, 3 = nose
  // For CP2K: 0 = none, 2 = langevin, 3 = csvr, 4 = gle, 5 = nose
  int type;
  // For CPMD: 0 = global, 1 = local
  // For CP2K: 0 = global, 1 = local, 2 = molecule
  int sys;
  gboolean show;
  double params[4];
  int natoms;
  int * list;
  struct thermostat * next;
  struct thermostat * prev;
};

struct dummy_atom {
  // 0 = type1, 1 = type2, ...
  int id;
  int type;
  gboolean show;
  double xyz[3];
  int coord[4];
  int natoms;
  int * list;
  struct dummy_atom * next;
  struct dummy_atom * prev;
};

typedef struct {
  int calc_type;
  int restart[10];
  int thermostats;
  struct thermostat * ions_thermostat;
  struct thermostat * elec_thermostat;
  int fixat;
  int * fixlist;
  int ** fixcoord;
  int dummies;
  struct dummy_atom * dummy;
  double default_opts[17];
  double calc_opts[24];
  int ** pp;
  gchar * info;
} cpmd;

typedef struct {
  int input_type;
  double opts[42];
  double extra_opts[3][4];
  int thermostats;
  struct thermostat * ions_thermostat;
  int fixat[2];
  int * fixlist[2];
  int ** fixcoord[2];
  gchar * files[5];
  gchar *** spec_files;
  int ** spec_data;
  gchar * info;
} cp2k;
*/

/*
*  int save_thermo (FILE * fp, struct thermostat * thermo)
*
*  Usage: save thermostat to file
*
*  FILE * fp                  : the file pointer
*  struct thermostat * thermo : the thermostat to save
*/
int save_thermo (FILE * fp, struct thermostat * thermo)
{
  if (fwrite (& thermo -> id, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& thermo -> type, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& thermo -> sys, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& thermo -> show, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
  if (fwrite (thermo -> params, sizeof(double), 4, fp) != 4) return ERROR_RW;
  if (fwrite (& thermo -> natoms, sizeof(int), 1, fp) != 1) return ERROR_RW;
  return OK;
}

/*
*  int save_fixed_atoms (FILE * fp, int fixatoms, int * fixlist, int ** fixcoord)
*
*  Usage: save fixed atom(s) to file
*
*  FILE * fp       : the file pointer
*  int fixatoms    : the number of fixed atom(s)
*  int * fixlist   : the list of fixed atom(s)
*  int ** fixcoord : the list of fixed coordinate(s) for the fix atom(s)
*/
int save_fixed_atoms (FILE * fp, int fixatoms, int * fixlist, int ** fixcoord)
{
  int i;
  if (fwrite (& fixatoms, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fixatoms)
  {
    if (fixlist)
    {
      if (fwrite (fixlist, sizeof(int), fixatoms, fp) != fixatoms) return ERROR_RW;
    }
    else
    {
      i = 0;
      if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    }
    if (fixcoord)
    {
      for (i=0; i<fixatoms; i++) if (fwrite (fixcoord[i], sizeof(int), 3, fp) != 3) return ERROR_RW;
    }
    else
    {
      i = 0;
      if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    }
  }
  return OK;
}

/*
*  int save_cpmd_data (FILE * fp, int cid, struct project * this_proj)
*
*  Usage: save CPMD data to file
*
*  FILE * fp                  : the file pointer
*  int cid                    : the CPMD id (0 = ab-initio, 1 = QM-MM)
*  struct project * this_proj : the target project
*/
int save_cpmd_data (FILE * fp, int cid, struct project * this_proj)
{
  int i;
  if (this_proj -> cpmd_input[cid] == NULL)
  {
    i = 0;
    if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    return OK;
  }
  i = 1;
  if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& this_proj -> cpmd_input[cid] -> calc_type, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (this_proj -> cpmd_input[cid] -> default_opts, sizeof(double), 17, fp) != 17) return ERROR_RW;
  if (fwrite (this_proj -> cpmd_input[cid] -> calc_opts, sizeof(double), 24, fp) != 24) return ERROR_RW;
  if (fwrite (& this_proj -> cpmd_input[cid] -> thermostats, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (this_proj -> cpmd_input[cid] -> thermostats)
  {
    struct thermostat * thermo = this_proj -> cpmd_input[cid] -> ions_thermostat;
    i = 0;
    while (thermo)
    {
      if (save_thermo (fp, thermo) != OK) return ERROR_RW;
      i ++;
      thermo = thermo -> next;
    }
    i = (this_proj -> cpmd_input[cid] -> elec_thermostat) ? 1 : 0;
    if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (this_proj -> cpmd_input[cid] -> elec_thermostat)
    {
      if (save_thermo (fp, this_proj -> cpmd_input[cid] -> elec_thermostat) != OK) return ERROR_RW;
    }
  }
  if (save_fixed_atoms (fp, this_proj -> cpmd_input[cid] -> fixat, this_proj -> cpmd_input[cid] -> fixlist, this_proj -> cpmd_input[cid] -> fixcoord) != OK)
  {
    return ERROR_RW;
  }
  if (fwrite (& this_proj -> cpmd_input[cid] -> dummies, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (this_proj -> cpmd_input[cid] -> dummies)
  {
    struct dummy_atom * dummy = this_proj -> cpmd_input[cid] -> dummy;
    while (dummy)
    {
      if (fwrite (& dummy -> id, sizeof(int), 1, fp) != 1) return ERROR_RW;
      if (fwrite (& dummy -> type, sizeof(int), 1, fp) != 1) return ERROR_RW;
      if (fwrite (& dummy -> show, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
      if (fwrite (dummy -> xyz, sizeof(double), 3, fp) != 3) return ERROR_RW;
      if (fwrite (dummy -> coord, sizeof(int), 4, fp) != 4) return ERROR_RW;
      if (fwrite (& dummy -> natoms, sizeof(int), 1, fp) != 1) return ERROR_RW;
      if (dummy -> natoms)
      {
        if (fwrite (dummy -> list, sizeof(int), dummy -> natoms, fp) != dummy -> natoms) return ERROR_RW;
      }
      dummy = dummy -> next;
    }
  }
  for (i=0; i<this_proj -> nspec; i++)
  {
    if (fwrite (this_proj -> cpmd_input[cid] -> pp[i], sizeof(int), 2, fp) != 2) return ERROR_RW;
  }
  return save_this_string (fp, this_proj -> cpmd_input[cid] -> info);
}

/*
*  int save_cp2k_data (FILE * fp, int cid, struct project * this_proj)
*
*  Usage: save CP2K data to file
*
*  FILE * fp                  : the file pointer
*  int cid                    : the CP2K id (0 = ab-initio, 1 = QM-MM)
*  struct project * this_proj : the target project
*/
int save_cp2k_data (FILE * fp, int cid, struct project * this_proj)
{
  int i, j;
  if (this_proj -> cp2k_input[cid] == NULL)
  {
    i = 0;
    if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    return OK;
  }
  i = 1;
  if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& this_proj -> cp2k_input[cid] -> input_type, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (this_proj -> cp2k_input[cid] -> opts, sizeof(double), 42, fp) != 42) return ERROR_RW;
  for (i=0; i<3; i++)
  {
    if (fwrite (this_proj -> cp2k_input[cid] -> extra_opts[i], sizeof(double), 4, fp) != 4) return ERROR_RW;
  }
  if (fwrite (& this_proj -> cp2k_input[cid] -> thermostats, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (this_proj -> cp2k_input[cid] -> thermostats)
  {
    struct thermostat * thermo = this_proj -> cp2k_input[cid] -> ions_thermostat;
    while (thermo)
    {
      if (save_thermo (fp, thermo) != OK) return ERROR_RW;
      thermo = thermo -> next;
    }
  }
  for (i=0; i<2; i++)
  {
    if (save_fixed_atoms (fp, this_proj -> cp2k_input[cid] -> fixat[i], this_proj -> cp2k_input[cid] -> fixlist[i], this_proj -> cp2k_input[cid] -> fixcoord[i]) != OK)
    {
      return ERROR_RW;
    }
  }
  for (i=0; i<this_proj -> nspec; i++)
  {
    if (fwrite (this_proj -> cp2k_input[cid] -> spec_data[i], sizeof(int), 2, fp) != 2) return ERROR_RW;
    for (j=0; j<2; j++)
    {
      if (save_this_string (fp, this_proj -> cp2k_input[cid] -> spec_files[i][j]) != OK) return ERROR_RW;
    }
  }
  for (i=0; i<5; i++)
  {
    if (save_this_string (fp, this_proj -> cp2k_input[cid] -> files[i]) != OK) return ERROR_RW;
  }
  return save_this_string (fp, this_proj -> cp2k_input[cid] -> info);
}
