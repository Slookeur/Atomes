/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include "dlp_field.h"

struct field_molecule * get_active_field_molecule_from_model_id (struct project * this_proj, int aid)
{
  int i;
  struct field_molecule * fmol = this_proj -> force_field[activef] -> first_molecule;
  struct field_atom * fat;
  while (fmol)
  {
    fat = fmol -> first_atom;
    while (fat)
    {
      for (i=0; i<fat -> num; i++)
      {
        if (fat -> list[i] == aid) return fmol;
      }
      fat = fat -> next;
    }
    fmol = fmol -> next;
  }
  return NULL;
}

struct field_molecule * get_active_field_molecule (int a)
{
  int i;
  struct field_molecule * tfmol = tmp_field -> first_molecule;
  for (i=0; i<a; i++)
  {
    if (tfmol -> next != NULL) tfmol = tfmol -> next;
  }
  return tfmol;
}

struct field_nth_body * get_active_body (int a, int b)
{
  int i;
  struct field_nth_body * body;
  body = tmp_field -> first_body[b];
  for (i=0; i<a; i++)
  {
    if (body -> next != NULL) body = body -> next;
  }
  return body;
}

struct field_external * get_active_external (int a)
{
  int i;
  struct field_external * external;
  external = tmp_field -> first_external;
  for (i=0; i<a; i++)
  {
    if (external -> next != NULL) external = external -> next;
  }
  return external;
}

struct field_atom * get_active_atom (int a, int b)
{
  int i;
  struct field_atom * ato;
  ato = get_active_field_molecule (a) -> first_atom;
  for (i=0; i<b; i++)
  {
    if (ato -> next != NULL) ato = ato -> next;
  }
  return ato;
}

struct field_shell * get_active_shell (int a, int b)
{
  int i;
  struct field_shell * shl;
  shl = get_active_field_molecule (a) -> first_shell;
  for (i=0; i<b; i++)
  {
    if (shl -> next != NULL) shl = shl -> next;
  }
  return shl;
}

struct field_constraint * get_active_constraint (int a, int b)
{
  int i;
  struct field_constraint * cons;
  cons = get_active_field_molecule (a) -> first_constraint;
  for (i=0; i<b; i++)
  {
    if (cons -> next != NULL) cons = cons -> next;
  }
  return cons;
}

struct field_pmf * get_active_pmf (int a, int b)
{
  int i;
  struct field_pmf * pmf;
  pmf = get_active_field_molecule (a) -> first_pmf;
  for (i=0; i<b; i++)
  {
    if (pmf -> next != NULL) pmf = pmf -> next;
  }
  return pmf;
}

struct field_rigid * get_active_rigid (int a, int b)
{
  int i;
  struct field_rigid * rig;
  rig = get_active_field_molecule (a) -> first_rigid;
  for (i=0; i<b; i++)
  {
    if (rig -> next != NULL) rig = rig -> next;
  }
  return rig;
}

struct field_tethered * get_active_tethered (int a, int b)
{
  int i;
  struct field_tethered * tet;
  tet = get_active_field_molecule (a) -> first_tethered;
  for (i=0; i<b; i++)
  {
    if (tet -> next != NULL) tet = tet -> next;
  }
  return tet;
}

struct field_prop * get_active_prop (struct  field_prop * pr, int a)
{
  struct field_prop * prop;
  prop = pr;
  int i;
  for (i=0; i<a; i++)
  {
    if (prop -> next != NULL) prop = prop -> next;
  }
  return prop;
}

struct field_prop * get_active_prop_using_atoms (struct  field_prop * pr, int ti, int * ids)
{
  struct field_prop * prop = NULL;
  prop = pr;
  gboolean done;
  int i;
  while (prop != NULL)
  {
    done = TRUE;
    for (i=0; i<ti; i++)
    {
      if (prop -> aid[i] != ids[i])
      {
        done = FALSE;
        break;
      }
    }
    if (done) break;
    prop = prop -> next;
  }
  return prop;
}

struct field_struct * get_active_struct (int s, int a, int b)
{
  int i;
  struct field_struct * str;
  str = get_active_field_molecule (a) -> first_struct[s];
  for (i=0; i<b; i++)
  {
    if (str -> next != NULL) str = str -> next;
  }
  return str;
}
