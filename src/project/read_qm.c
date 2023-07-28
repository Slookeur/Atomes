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
* This file: 'read_qm.c'
*
*  Contains:
*

 - Subroutines to read ab-intio calculation (CPMD/CP2K) parameters from atomes project file

*
*  List of subroutines:

  int read_thermo (FILE * fp, struct thermostat * thermo);
  int read_fixed_atoms_cpmd (FILE * fp, cpmd * cpmd_input);
  int read_fixed_atoms_cp2k (FILE * fp, cp2k * cp2k_input, int idf);
  int read_cpmd_data (FILE * fp, int cid, struct project * this_proj);
  int read_cp2k_data (FILE * fp, int cid, struct project * this_proj);

*/

#include "global.h"
#include "project.h"

/*
*  int read_thermo (FILE * fp, struct thermostat * thermo)
*
*  Usage: read thermostat information from file
*
*  FILE * fp                  : the file pointer
*  struct thermostat * thermo : the thermostat to store the data
*/
int read_thermo (FILE * fp, struct thermostat * thermo)
{
  if (fread (& thermo -> id, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fread (& thermo -> type, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fread (& thermo -> sys, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fread (& thermo -> show, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
  if (fread (thermo -> params, sizeof(double), 4, fp) != 4) return ERROR_RW;
  if (fread (& thermo -> natoms, sizeof(int), 1, fp) != 1) return ERROR_RW;
  return OK;
}

/*
*  int read_fixed_atoms_cpmd (FILE * fp, cpmd * cpmd_input)
*
*  Usage: read fixed CPMD atom(s) from file
*
*  FILE * fp         : the file pointer
*  cpmd * cpmd_input : the CPMD input structure to store the data
*/
int read_fixed_atoms_cpmd (FILE * fp, cpmd * cpmd_input)
{
  int i;
  if (fread (& cpmd_input -> fixat, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (cpmd_input -> fixat)
  {
    if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (i)
    {
      cpmd_input -> fixlist = allocint (cpmd_input -> fixat);
      if (fread (cpmd_input -> fixlist, sizeof(int), cpmd_input -> fixat, fp) != cpmd_input -> fixat) return ERROR_RW;
    }
    if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (i)
    {
      cpmd_input -> fixcoord = allocdint (cpmd_input -> fixat, 3);
      for (i=0; i<cpmd_input -> fixat; i++) if (fread (cpmd_input -> fixcoord[i], sizeof(int), 3, fp) != 3) return ERROR_RW;
    }
  }
  return OK;
}

/*
*  int read_fixed_atoms_cp2k (FILE * fp, cp2k * cp2k_input, int idf)
*
*  Usage: read fixed CP2K from file
*
*  FILE * fp         : the file pointer
*  cp2k * cp2k_input : the CP2K input structure to store the data
*  int idf           : the fixed atom(s) format
*/
int read_fixed_atoms_cp2k (FILE * fp, cp2k * cp2k_input, int idf)
{
  int i;
  if (fread (& cp2k_input -> fixat[idf], sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (cp2k_input -> fixat[idf])
  {
    if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (i)
    {
      cp2k_input -> fixlist[idf] = allocint (cp2k_input -> fixat[idf]);
      if (fread (cp2k_input -> fixlist[idf], sizeof(int), cp2k_input -> fixat[idf], fp) != cp2k_input -> fixat[idf]) return ERROR_RW;
    }
    if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (i)
    {
      cp2k_input -> fixcoord[idf] = allocdint (cp2k_input -> fixat[idf], 3);
      for (i=0; i<cp2k_input -> fixat[idf]; i++) if (fread (cp2k_input -> fixcoord[idf][i], sizeof(int), 3, fp) != 3) return ERROR_RW;
    }
  }
  return OK;
}

/*
*  int read_cpmd_data (FILE * fp, int cid, struct project * this_proj)
*
*  Usage: read CPMD data from file
*
*  FILE * fp                  : the file pointer
*  int cid                    : CPMD id (0 = ab-initio, 1 = QM-MM)
*  struct project * this_proj : the target project
*/
int read_cpmd_data (FILE * fp, int cid, struct project * this_proj)
{
  int i;
  if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (! i) return OK;
  this_proj -> cpmd_input[cid] = g_malloc0 (sizeof*this_proj -> cpmd_input[cid]);
  if (fread (& this_proj -> cpmd_input[cid] -> calc_type, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fread (this_proj -> cpmd_input[cid] -> default_opts, sizeof(double), 17, fp) != 17) return ERROR_RW;
  if (fread (this_proj -> cpmd_input[cid] -> calc_opts, sizeof(double), 24, fp) != 24) return ERROR_RW;
  if (fread (& this_proj -> cpmd_input[cid] -> thermostats, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (this_proj -> cpmd_input[cid] -> thermostats)
  {
    this_proj -> cpmd_input[cid] -> ions_thermostat = g_malloc0 (sizeof*this_proj -> cpmd_input[cid] -> ions_thermostat);
    struct thermostat * thermo = this_proj -> cpmd_input[cid] -> ions_thermostat;
    for (i=0; i<this_proj -> cpmd_input[cid] -> thermostats; i++)
    {
      if (read_thermo (fp, thermo) != OK) return ERROR_RW;
      if (i < this_proj -> cpmd_input[cid] -> thermostats - 1)
      {
        thermo -> next = g_malloc0 (sizeof*thermo -> next);
        thermo -> next -> prev = thermo;
        thermo = thermo -> next;
      }
    }
    if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (i)
    {
      this_proj -> cpmd_input[cid] -> elec_thermostat = g_malloc0 (sizeof*this_proj -> cpmd_input[cid] -> elec_thermostat);
      if (read_thermo (fp, this_proj -> cpmd_input[cid] -> elec_thermostat) != OK) return ERROR_RW;
    }
  }
  if (read_fixed_atoms_cpmd (fp, this_proj -> cpmd_input[cid]) != OK)
  {
    return ERROR_RW;
  }
  if (fread (& this_proj -> cpmd_input[cid] -> dummies, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (this_proj -> cpmd_input[cid] -> dummies)
  {
    this_proj -> cpmd_input[cid] -> dummy = g_malloc0 (sizeof*this_proj -> cpmd_input[cid] -> dummy);
    struct dummy_atom * dummy = this_proj -> cpmd_input[cid] -> dummy;
    for (i=0; i < this_proj -> cpmd_input[cid] -> dummies; i ++)
    {
      if (fread (& dummy -> id, sizeof(int), 1, fp) != 1) return ERROR_RW;
      if (fread (& dummy -> type, sizeof(int), 1, fp) != 1) return ERROR_RW;
      if (fread (& dummy -> show, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
      if (fread (dummy -> xyz, sizeof(double), 3, fp) != 3) return ERROR_RW;
      if (fread (dummy -> coord, sizeof(int), 4, fp) != 4) return ERROR_RW;
      if (fread (& dummy -> natoms, sizeof(int), 1, fp) != 1) return ERROR_RW;
      if (dummy -> natoms)
      {
        dummy -> list = allocint (dummy -> natoms);
        if (fread (dummy -> list, sizeof(int), dummy -> natoms, fp) != dummy -> natoms) return ERROR_RW;
      }
      if (i < this_proj -> cpmd_input[cid] -> dummies - 1)
      {
        dummy -> next = g_malloc0 (sizeof*dummy -> next);
        dummy -> next -> prev = dummy;
        dummy = dummy -> next;
      }
    }
  }
  this_proj -> cpmd_input[cid] -> pp = allocdint (this_proj -> nspec, 2);
  for (i=0; i<this_proj -> nspec; i++)
  {
    if (fread (this_proj -> cpmd_input[cid] -> pp[i], sizeof(int), 2, fp) != 2) return ERROR_RW;
  }
  this_proj -> cpmd_input[cid] -> info = read_this_string (fp);
  /*g_debug (" ********************* CPMD INFO *********************");
  g_debug ("\n%s\n", this_proj -> cpmd_input[cid] -> info);
  g_debug (" *****************************************************");*/
  if (this_proj -> cpmd_input[cid] -> info == NULL) return ERROR_RW;
  return OK;
}

/*
*  int read_cp2k_data (FILE * fp, int cid, struct project * this_proj)
*
*  Usage: read CP2K data from file
*
*  FILE * fp                  : the file pointer
*  int cid                    : CP2K id (0 = ab-initio, 1 = QM-MM)
*  struct project * this_proj : the target project
*/
int read_cp2k_data (FILE * fp, int cid, struct project * this_proj)
{
  int i, j;
  if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (! i) return OK;
  this_proj -> cp2k_input[cid] = g_malloc0 (sizeof*this_proj -> cp2k_input[cid]);
  if (fread (& this_proj -> cp2k_input[cid] -> input_type, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fread (this_proj -> cp2k_input[cid] -> opts, sizeof(double), 42, fp) != 42) return ERROR_RW;
  for (i=0; i<3; i++)
  {
    if (fread (this_proj -> cp2k_input[cid] -> extra_opts[i], sizeof(double), 4, fp) != 4) return ERROR_RW;
  }
  if (fread (& this_proj -> cp2k_input[cid] -> thermostats, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (this_proj -> cp2k_input[cid] -> thermostats)
  {
    this_proj -> cp2k_input[cid] -> ions_thermostat = g_malloc0 (sizeof*this_proj -> cp2k_input[cid] -> ions_thermostat);
    struct thermostat * thermo = this_proj -> cp2k_input[cid] -> ions_thermostat;
    for (i=0; i<this_proj -> cp2k_input[cid] -> thermostats; i++)
    {
      if (read_thermo (fp, thermo) != OK) return ERROR_RW;
      if (i < this_proj -> cp2k_input[cid] -> thermostats - 1)
      {
        thermo -> next = g_malloc0 (sizeof*thermo -> next);
        thermo -> next -> prev = thermo;
        thermo = thermo -> next;
      }
    }
  }
  for (i=0; i<2; i++)
  {
    if (read_fixed_atoms_cp2k (fp, this_proj -> cp2k_input[cid], i) != OK)
    {
      return ERROR_RW;
    }
  }
  this_proj -> cp2k_input[cid] -> spec_data = allocdint (this_proj -> nspec, 2);
  this_proj -> cp2k_input[cid] -> spec_files = g_malloc0 (this_proj -> nspec*sizeof*this_proj -> cp2k_input[cid] -> spec_files);
  for (i=0; i<this_proj -> nspec; i++)
  {
    if (fread (this_proj -> cp2k_input[cid] -> spec_data[i], sizeof(int), 2, fp) != 2) return ERROR_RW;
    this_proj -> cp2k_input[cid] -> spec_files[i] = g_malloc0 (2*sizeof*this_proj -> cp2k_input[cid] -> spec_files[i]);
    for (j=0; j<2; j++)
    {
      this_proj -> cp2k_input[cid] -> spec_files[i][j] = read_this_string (fp);
    }
  }
  for (i=0; i<5; i++)
  {
    this_proj -> cp2k_input[cid] -> files[i] = read_this_string (fp);
  }
  this_proj -> cp2k_input[cid] -> info = read_this_string (fp);
  if (this_proj -> cp2k_input[cid] -> info == NULL) return ERROR_RW;
  return OK;
}
