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
* This file: 'dlp_copy.c'
*
*  Contains:
*

 - The subroutines to copy force field and related data structures

*
*  List of subroutines:

  gboolean check_this_other_prop (int oid, int nat, struct field_prop * other);

  void duplicate_other_prop (int oid, struct field_struct * old_fstr, struct field_struct * new_fstr);
  void duplicate_nbody_params (struct field_nth_body * new_fbody, struct field_nth_body * old_fbody);

  struct field_atom * duplicate_field_atom (struct field_atom * old_fat);
  struct field_shell * duplicate_field_shell (struct field_shell * old_shell);
  struct field_constraint * duplicate_field_constraint (struct field_constraint * old_cons);
  struct field_pmf * duplicate_field_pmf (struct field_pmf * old_pmf);
  struct field_rigid * duplicate_field_rigid (struct field_rigid * old_rig);
  struct field_tethered * duplicate_field_tethered (struct field_tethered * old_tet);
  struct field_prop * duplicate_field_prop (struct field_prop * old_prop, int ti);
  struct field_struct * duplicate_field_struct (struct field_struct * old_fstr);
  struct field_struct * duplicate_field_struct_list (struct field_struct * list_str, gboolean init);
  struct field_nth_body * duplicate_field_nth_body (struct field_nth_body * old_fbody);
  struct field_external * duplicate_field_external (struct field_external * old_fext);
  struct field_molecule * duplicate_field_molecule (struct field_molecule * old_fmol);

  classical_field * duplicate_classical_field (classical_field * init_field);

*/

#include "dlp_field.h"
#include "global.h"

extern void print_all_field_struct (struct field_molecule * mol, int str);

/*
*  struct field_atom * duplicate_field_atom (struct field_atom * old_fat)
*
*  Usage: create copy of a field atom data structure
*
*  struct field_atom * old_fat : the field atom to duplicate
*/
struct field_atom * duplicate_field_atom (struct field_atom * old_fat)
{
  struct field_atom * new_fat;
  new_fat = g_malloc0 (sizeof*new_fat);
  new_fat -> id = old_fat -> id;
  new_fat -> fid = old_fat -> fid;
  new_fat -> afid = old_fat -> afid;
  new_fat -> name = g_strdup_printf ("%s", old_fat -> name);
  new_fat -> num = old_fat -> num;
  new_fat -> sp = old_fat -> sp;
  new_fat -> type = old_fat -> type;
  new_fat -> mass = old_fat -> mass;
  new_fat -> charge = old_fat -> charge;
  new_fat -> frozen = old_fat -> frozen;
  new_fat -> show = FALSE;
  new_fat -> list = duplicate_int (old_fat -> num, old_fat -> list);
  /* g_debug ("Debug copy atom: %d", new_fat -> id);
  int i;
  for (i=0; i<new_fat -> num; i++) g_debug ("old_fat -> list[%d]= %d", i, old_fat -> list[i]);
  for (i=0; i<new_fat -> num; i++) g_debug ("new_fat -> list[%d]= %d", i, new_fat -> list[i]); */
  new_fat -> list_id = duplicate_int (old_fat -> num, old_fat -> list_id);
  new_fat -> frozen_id = duplicate_bool (old_fat -> num, old_fat -> frozen_id);
  new_fat -> prev = NULL;
  new_fat -> next = NULL;
  return new_fat;
}

/*
*  struct field_shell * duplicate_field_shell (struct field_shell * old_shell)
*
*  Usage: create copy of a field shell data structure
*
*  struct field_shell * old_shell : the field shell to duplicate
*/
struct field_shell * duplicate_field_shell (struct field_shell * old_shell)
{
  struct field_shell * new_shell;
  new_shell = g_malloc0 (sizeof*new_shell);
  new_shell -> id = old_shell -> id;
  new_shell -> ia[0] = old_shell -> ia[0];
  new_shell -> ia[1] = old_shell -> ia[1];
  new_shell -> m = old_shell -> m;
  new_shell -> z = old_shell -> z;
  new_shell -> k2 = old_shell -> k2;
  new_shell -> k4 = old_shell -> k4;
  new_shell -> vdw = old_shell -> vdw;
  new_shell -> use = old_shell -> use;
  new_shell -> show = old_shell -> show;
  new_shell -> prev = NULL;
  new_shell -> next = NULL;
  return new_shell;
}

/*
*  struct field_constraint * duplicate_field_constraint (struct field_constraint * old_cons)
*
*  Usage: create copy of a field constraint data structure
*
*  struct field_constraint * old_cons : the field constraint to duplicate
*/
struct field_constraint * duplicate_field_constraint (struct field_constraint * old_cons)
{
  struct field_constraint * new_cons;
  new_cons = g_malloc0 (sizeof*new_cons);
  new_cons -> id = old_cons -> id;
  new_cons -> ia[0] = old_cons -> ia[0];
  new_cons -> ia[1] = old_cons -> ia[1];
  new_cons -> length = old_cons -> length;
  new_cons -> use = old_cons -> use;
  new_cons -> show = old_cons -> show;
  new_cons -> prev = NULL;
  new_cons -> next = NULL;
  return new_cons;
}

/*
*  struct field_pmf * duplicate_field_pmf (struct field_pmf * old_pmf)
*
*  Usage: create copy of a field PMF data structure
*
*  struct field_pmf * old_pmf : the field PMF to duplicate
*/
struct field_pmf * duplicate_field_pmf (struct field_pmf * old_pmf)
{
  struct field_pmf * new_pmf;
  new_pmf = g_malloc0 (sizeof*new_pmf);
  new_pmf -> id = old_pmf -> id;
  new_pmf -> length = old_pmf -> length;
  int i;
  for (i=0; i<2; i++)
  {
    new_pmf -> num[i] = old_pmf -> num[i];
    new_pmf -> list[i] = duplicate_int (old_pmf -> num[i], old_pmf -> list[i]);
    new_pmf -> weight[i] = duplicate_float (old_pmf -> num[i], old_pmf -> weight[i]);
  }
  new_pmf -> show = old_pmf -> show;
  new_pmf -> use = old_pmf -> use;
  new_pmf -> next = NULL;
  new_pmf -> prev = NULL;
  return new_pmf;
}

/*
*  struct field_rigid * duplicate_field_rigid (struct field_rigid * old_rig)
*
*  Usage: create copy of a field rigid data structure
*
*  struct field_rigid * old_rig : the field rigid to duplicate
*/
struct field_rigid * duplicate_field_rigid (struct field_rigid * old_rig)
{
  struct field_rigid * new_rig;
  new_rig = g_malloc0 (sizeof*new_rig);
  new_rig -> id = old_rig -> id;
  new_rig -> num =  old_rig -> num;
  new_rig -> list = duplicate_int (old_rig -> num, old_rig -> list);
  new_rig -> show = old_rig -> show;
  new_rig -> use = old_rig -> use;
  new_rig -> next = NULL;
  new_rig -> prev = NULL;
  return new_rig;
}

/*
*  struct field_tethered * duplicate_field_tethered (struct field_tethered * old_tet)
*
*  Usage: create copy of a field tethered data structure
*
*  struct field_tethered * old_tet : the field tethered to duplicate
*/
struct field_tethered * duplicate_field_tethered (struct field_tethered * old_tet)
{
  struct field_tethered * new_tet;
  new_tet = g_malloc0 (sizeof*new_tet);
  new_tet -> id = old_tet -> id;
  new_tet -> num =  old_tet -> num;
  new_tet -> show = old_tet -> show;
  new_tet -> use = old_tet -> use;
  new_tet -> next = NULL;
  new_tet -> prev = NULL;
  return new_tet;
}

/*
*  struct field_prop * duplicate_field_prop (struct field_prop * old_prop, int ti)
*
*  Usage: create a copy of a field property
*
*  struct field_prop * old_prop : the field property to duplicate
*  int ti                       : the type of field property
*/
struct field_prop * duplicate_field_prop (struct field_prop * old_prop, int ti)
{
  struct field_prop * new_prop;
  new_prop = g_malloc0 (sizeof*new_prop);
  new_prop -> aid = duplicate_int (struct_id(ti+7), old_prop -> aid);
  new_prop -> key = old_prop -> key;
  new_prop -> pid = old_prop -> pid;
  new_prop -> fpid = old_prop -> fpid;
  new_prop -> val = NULL;
  if (fvalues[activef][ti+1][new_prop -> key] > 0)
  {
    new_prop -> val = duplicate_float (fvalues[activef][ti+1][new_prop -> key], old_prop -> val);
  }
  new_prop -> show = old_prop -> show;
  new_prop -> use = old_prop -> use;
  new_prop -> next = NULL;
  new_prop -> prev = NULL;
  return new_prop;
}

/*
*  gboolean check_this_other_prop (int oid, int nat, struct field_prop * other)
*
*  Usage: check if field atom already in field property
*
*  int oid                   : the field atom id to search for
*  int nat                   : the number of atoms in the field property
*  struct field_prop * other : the field structural property to check
*/
gboolean check_this_other_prop (int oid, int nat, struct field_prop * other)
{
  if (oid > -1)
  {
    int i;
    for (i=0; i<nat; i++)
    {
      if (tmp_fmol -> atoms_id[other -> aid[i]][0].a == oid) return FALSE;
    }
  }
  return TRUE;
}

/*
*  void duplicate_other_prop (int oid, struct field_struct * old_fstr, struct field_struct * new_fstr)
*
*  Usage: create copy of a field property 'other' list
*
*  int oid                        : the target field atom id
*  struct field_struct * old_fstr : the field property to duplicate
*  struct field_struct * new_fstr : the field property to store the results
*/
void duplicate_other_prop (int oid, struct field_struct * old_fstr, struct field_struct * new_fstr)
{
  int i = struct_id(new_fstr -> st+7);
  struct field_prop * tmp_new;
  struct field_prop * tmp_old = old_fstr -> other;
  while (tmp_old)
  {
    if (check_this_other_prop(oid, i, tmp_old))
    {
      if (new_fstr -> other)
      {
        tmp_new -> next = duplicate_field_prop (tmp_old, old_fstr -> st);
        tmp_new = tmp_new -> next;
      }
      else
      {
        new_fstr -> other = duplicate_field_prop (tmp_old, old_fstr -> st);
        tmp_new = new_fstr -> other;
      }
    }
    tmp_old = tmp_old -> next;
  }
}

/*
*  struct field_struct * duplicate_field_struct (struct field_struct * old_fstr)
*
*  Usage: create copy of a field structural element
*
*  struct field_struct * old_fstr : the field structural element to duplicate
*/
struct field_struct * duplicate_field_struct (struct field_struct * old_fstr)
{
  struct field_struct * new_fstr;
  new_fstr = g_malloc0 (sizeof*new_fstr);
  new_fstr -> st = old_fstr -> st;
  new_fstr -> id = old_fstr -> id;
  new_fstr -> num = old_fstr -> num;
  new_fstr -> aid = duplicate_int (struct_id(old_fstr -> st+7), old_fstr -> aid);
  new_fstr -> av = old_fstr -> av;
  new_fstr -> def = duplicate_field_prop (old_fstr -> def, old_fstr -> st);
  new_fstr -> other = NULL;
  if (old_fstr -> other != NULL) duplicate_other_prop (-1, old_fstr, new_fstr);
  new_fstr -> prev = NULL;
  new_fstr -> next = NULL;
  return new_fstr;
}

/*
*  struct field_struct * duplicate_field_struct_list (struct field_struct * list_str, gboolean init)
*
*  Usage: create copy of list of field structural element(s)
*
*  struct field_struct * list_str : the list of field structural element(s) to duplicate
*  gboolean init                  :
*/
struct field_struct * duplicate_field_struct_list (struct field_struct * list_str, gboolean init)
{
  struct field_struct * str_list = duplicate_field_struct (list_str);
  if (init) str_list -> def -> use = FALSE;
  struct field_struct * tmp_str = str_list;
  struct field_struct * tmp_fst = list_str;
  while (tmp_fst -> next)
  {
    tmp_str -> next = duplicate_field_struct (tmp_fst -> next);
    tmp_str -> next -> prev = tmp_str;
    if (init) tmp_str -> next -> def -> use = FALSE;
    tmp_str = tmp_str -> next;
    tmp_fst = tmp_fst -> next;
  }
  return str_list;
}

/*
*  void duplicate_nbody_params (struct field_nth_body * new_fbody, struct field_nth_body * old_fbody)
*
*  Usage: copy field body parameter list
*
*  struct field_nth_body * new_fbody : the body parameters to fill
*  struct field_nth_body * old_fbody : the body parameters to duplicate
*/
void duplicate_nbody_params (struct field_nth_body * new_fbody, struct field_nth_body * old_fbody)
{
  new_fbody -> key = old_fbody -> key;
  new_fbody -> val = duplicate_float (fvalues[activef][9+new_fbody -> bd][new_fbody -> key], old_fbody -> val);
  new_fbody -> show = old_fbody -> show;
  new_fbody -> use = old_fbody -> use;
}

/*
*  struct field_nth_body * duplicate_field_nth_body (struct field_nth_body * old_fbody)
*
*  Usage: create copy of a field body property
*
*  struct field_nth_body * old_fbody : the field body property to duplicate
*/
struct field_nth_body * duplicate_field_nth_body (struct field_nth_body * old_fbody)
{
  struct field_nth_body * new_fbody;
  int i, j;
  new_fbody = g_malloc0 (sizeof*new_fbody);
  new_fbody -> id = old_fbody -> id;
  new_fbody -> bd = old_fbody -> bd;
  if (old_fbody -> fpid) new_fbody -> fpid = duplicate_int (2, old_fbody -> fpid);
  j = body_at (old_fbody -> bd);
  new_fbody -> na = duplicate_int (j, old_fbody -> na);
  new_fbody -> ma = NULL;
  new_fbody -> ma = g_malloc (j*sizeof*new_fbody -> ma);
  new_fbody -> a = NULL;
  new_fbody -> a = g_malloc (j*sizeof*new_fbody -> a);

  for (i=0; i<j; i++)
  {
    if (old_fbody -> ma[i] != NULL)
    {
      new_fbody -> ma[i] = duplicate_int (old_fbody -> na[i], old_fbody -> ma[i]);
    }
    else
    {
      new_fbody -> ma[i] = NULL;
    }
    if (old_fbody -> a[i] != NULL)
    {
      new_fbody -> a[i] = duplicate_int (old_fbody -> na[i], old_fbody -> a[i]);
    }
    else
    {
      new_fbody -> a[i] = NULL;
    }
  }
  duplicate_nbody_params (new_fbody, old_fbody);
  new_fbody -> prev = NULL;
  new_fbody -> next = NULL;
  return new_fbody;
}

/*
*  struct field_external * duplicate_field_external (struct field_external * old_fext)
*
*  Usage: create copy of a field external property
*
*  struct field_external * old_fext : the field external property to duplicate
*/
struct field_external * duplicate_field_external (struct field_external * old_fext)
{
  struct field_external * new_fext;
  new_fext = g_malloc0 (sizeof*new_fext);
  new_fext -> id = old_fext -> id;
  new_fext -> key = old_fext -> key;
  new_fext -> val = NULL;
  if (old_fext -> val != NULL)
  {
    new_fext -> val = duplicate_float (fvalues[activef][SEXTERN-6][new_fext -> key], old_fext -> val);
  }
  new_fext -> use = old_fext -> use;
  new_fext -> next = NULL;
  new_fext -> prev = NULL;
  return new_fext;
}

/*
*  struct field_molecule * duplicate_field_molecule (struct field_molecule * old_fmol)
*
*  Usage: create copy of a field molecule
*
*  struct field_molecule * old_fmol : the feld molecule to duplicate
*/
struct field_molecule * duplicate_field_molecule (struct field_molecule * old_fmol)
{
  int i, j;
  struct field_molecule * new_fmol;
  new_fmol = g_malloc0 (sizeof*new_fmol);
  new_fmol -> id = old_fmol -> id;
  new_fmol -> multi = old_fmol -> multi;
  new_fmol -> name = g_strdup_printf ("%s", old_fmol -> name);
  new_fmol -> show = old_fmol -> show;
  new_fmol -> show_id = old_fmol -> show_id;
  new_fmol -> atoms = old_fmol -> atoms;
  new_fmol -> first_atom = NULL;
  new_fmol -> tethered = old_fmol -> tethered;
  new_fmol -> first_tethered = NULL;
  new_fmol -> rigids = old_fmol -> rigids;
  new_fmol -> first_rigid = NULL;
  new_fmol -> pmfs = old_fmol -> pmfs;
  new_fmol -> first_pmf = NULL;
  new_fmol -> first_constraint = NULL;
  new_fmol -> constraints = old_fmol -> constraints;
  new_fmol -> first_shell = NULL;
  new_fmol -> shells = old_fmol -> shells;
  new_fmol -> mol = g_malloc0 (sizeof*new_fmol -> mol);
  new_fmol -> mol = & tmp_proj -> modelfc -> mols[0][old_fmol -> mol -> id];
  // Duplicating atoms
  new_fmol -> fragments = NULL;
  new_fmol -> fragments = allocint(new_fmol -> multi);
  for (i=0; i<new_fmol -> multi; i++) new_fmol -> fragments[i] = old_fmol -> fragments[i];

  new_fmol -> first_atom = duplicate_field_atom (old_fmol -> first_atom);
  struct field_atom * tmp_fat = new_fmol -> first_atom;
  struct field_atom * tmp_fa = old_fmol -> first_atom;

  for (i=1; i<new_fmol -> atoms; i++)
  {
    tmp_fat -> next = duplicate_field_atom (tmp_fa -> next);
    tmp_fat -> next -> prev = tmp_fat;
    tmp_fat = tmp_fat -> next;
    tmp_fa = tmp_fa -> next;
  }

  new_fmol -> atoms_id = g_malloc (new_fmol -> mol -> natoms*sizeof*new_fmol -> atoms_id);
  for (i=0; i<new_fmol -> mol -> natoms; i++)
  {
    new_fmol -> atoms_id[i] = g_malloc0 (new_fmol -> multi*sizeof*new_fmol -> atoms_id[i]);
    for (j=0; j<new_fmol -> multi; j++)
    {
      new_fmol -> atoms_id[i][j].a = old_fmol -> atoms_id[i][j].a;
      new_fmol -> atoms_id[i][j].b = old_fmol -> atoms_id[i][j].b;
    }
  }

  for (i=0; i<8; i++)
  {
    // Duplicating bonds / br / angles / ar / diherdrals / impropers / tr / inversions
    new_fmol -> nstruct[i] = old_fmol -> nstruct[i];
    new_fmol -> first_struct[i] = NULL;
    if (old_fmol -> nstruct[i] > 0)
    {
      new_fmol -> first_struct[i] = duplicate_field_struct (old_fmol -> first_struct[i]);
      struct field_struct * tmp_str = new_fmol -> first_struct[i];
      struct field_struct * tmp_fst = old_fmol -> first_struct[i];
      for (j=1; j<new_fmol -> nstruct[i]; j++)
      {
        tmp_str -> next = duplicate_field_struct (tmp_fst -> next);
        tmp_str -> next -> prev = tmp_str;
        tmp_str = tmp_str -> next;
        tmp_fst = tmp_fst -> next;
      }
    }
  }
  new_fmol -> next = NULL;
  new_fmol -> prev = NULL;
  return new_fmol;
}

/*
*  classical_field * duplicate_classical_field (classical_field * init_field)
*
*  Usage: create copy of a force field
*
*  classical_field * init_field : the force field to duplicate
*/
classical_field * duplicate_classical_field (classical_field * init_field)
{
  classical_field * new_field = NULL;
  new_field = g_malloc (sizeof*new_field);

  int i, j, k;

  // All
  for (i=0; i<MAXDATC+MAXDATA; i++) new_field -> afp[i] = init_field -> afp[i];
  for (i=0; i<2; i++)  new_field -> prepare_file[i] = init_field -> prepare_file[i];

  // Control
  new_field  -> ensemble = init_field -> ensemble;
  new_field  -> thermostat = init_field -> thermostat;
  for (i=0; i<17; i++) new_field -> sys_opts[i] = init_field -> sys_opts[i];
  for (i=0; i<23; i++) new_field -> io_opts[i] = init_field -> io_opts[i];
  for (i=0; i<17; i++) new_field -> ana_opts[i] = init_field -> ana_opts[i];
  for (i=0; i<11; i++) new_field -> elec_opts[i] = init_field -> elec_opts[i];
  for (i=0; i<6; i++) new_field -> vdw_opts[i] = init_field -> vdw_opts[i];
  for (i=0; i<2; i++) new_field -> met_opts[i] = init_field -> met_opts[i];
  for (i=0; i<17; i++) new_field -> equi_opts[i] = init_field -> equi_opts[i];
  for (i=0; i<10; i++) new_field -> thermo_opts[i] = init_field -> thermo_opts[i];
  for (i=0; i<20; i++) new_field -> md_opts[i] = init_field -> md_opts[i];
  for (i=0; i<31; i++) new_field -> out_opts[i] = init_field -> out_opts[i];

  // Field
  new_field -> energy_unit = init_field -> energy_unit;
  new_field -> atom_init = init_field -> atom_init;

  new_field -> molecules = init_field -> molecules;
  if (new_field -> molecules)
  {
    struct field_molecule * fmol;
    new_field -> first_molecule = duplicate_field_molecule (init_field -> first_molecule);
    fmol = new_field -> first_molecule;
    tmp_fmol = init_field -> first_molecule;
    for (i=1; i<init_field -> molecules; i++)
    {
      fmol -> next = duplicate_field_molecule (tmp_fmol -> next);
      fmol -> next -> prev = fmol;
      fmol = fmol -> next;
      tmp_fmol = tmp_fmol -> next;
    }

    // Duplicating nth_body
    for (i=0; i<5; i++)
    {
      new_field -> first_body[i] = NULL;
      new_field -> nbody[i] = init_field -> nbody[i];
      if (init_field -> nbody[i] > 0)
      {
        new_field -> first_body[i] = duplicate_field_nth_body (init_field -> first_body[i]);
        struct field_nth_body * tmp_fbod = new_field -> first_body[i];
        struct field_nth_body * tmp_fbo = init_field -> first_body[i];
        for (j=1; j<new_field -> nbody[i]; j++)
        {
          tmp_fbod -> next = duplicate_field_nth_body (tmp_fbo -> next);
          tmp_fbod -> next -> prev = tmp_fbod;
          tmp_fbod = tmp_fbod -> next;
          tmp_fbo = tmp_fbo -> next;
        }
      }
    }
    // Tersoff cross terms
    new_field -> cross = NULL;
    if (init_field -> cross != NULL)
    {
      new_field -> cross = g_malloc (tmp_field -> nbody[2]*sizeof*new_field -> cross);
      for (i=0; i<tmp_field -> nbody[2]; i++)
      {
        new_field -> cross[i] = g_malloc (tmp_field -> nbody[2]*sizeof*new_field -> cross[i]);
        for (j=0; j<tmp_field -> nbody[2]; k++) new_field -> cross[i][j] = duplicate_double (3, init_field -> cross[i][j]);
      }
    }

    // Duplicating external fields
    new_field -> first_external = NULL;
    new_field -> extern_fields = init_field -> extern_fields;
    if (init_field -> extern_fields > 0)
    {
      new_field -> first_external = duplicate_field_external (init_field -> first_external);
      struct field_external * tmp_fext = new_field -> first_external;
      struct field_external * tmp_ftxt = init_field -> first_external;
      for (i=1; i<new_field -> extern_fields; i++)
      {
        tmp_fext -> next = duplicate_field_external (tmp_ftxt -> next);
        tmp_fext -> next -> prev = tmp_fext;
        tmp_fext = tmp_fext -> next;
        tmp_ftxt = tmp_ftxt -> next;
      }
    }
  }
  return new_field;
}
