/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include "atom_edit.h"

double get_object_dim (struct insert_object * object)
{
  double dmax = 0.0;
  vec3_t at, bt;
  vec3_t dist;
  int i, j;
  for (i=0; i<object -> atoms-1; i++)
  {
    at = vec3 (object -> at_list[i].x, object -> at_list[i].y, object -> at_list[i].z);
    for (j=i+1; j<object -> atoms; j++)
    {
      bt = vec3 (object -> at_list[j].x, object -> at_list[j].y, object -> at_list[j].z);
      dist = v3_sub(at, bt);
      dmax = max (dmax, v3_length(dist));
    }
  }
  return dmax+1.0;
}

void correct_pos_and_get_dim (struct insert_object * object, gboolean adjust)
{
  int i;
  if (object -> baryc) g_free(object -> baryc);
  object -> baryc = allocdouble(3);
  for (i=0; i<object -> atoms; i++)
  {
    object -> baryc[0] += object -> at_list[i].x;
    object -> baryc[1] += object -> at_list[i].y;
    object -> baryc[2] += object -> at_list[i].z;
  }
  for (i=0; i<3; i++) object -> baryc[i] /= object -> atoms;
  if (adjust)
  {
    for (i=0; i<object -> atoms; i++)
    {
      object -> at_list[i].x -= object -> baryc[0];
      object -> at_list[i].y -= object -> baryc[1];
      object -> at_list[i].z -= object -> baryc[2];
    }
  }
  object -> dim = get_object_dim (object);
}

gboolean rebuild_atom_neighbors (struct project * this_proj, int step, struct insert_object * object, int target, int aid, struct atom * at, gboolean * checked_at)
{
  int i, j;
  struct distance dist;
  for (i=0; i<at -> numv; i++)
  {
    j = at -> vois[i];
    dist = distance_3d (& this_proj -> cell, step, at, & object -> at_list[j]);
    if (dist.pbc && ! checked_at[j])
    {
      object -> at_list[j].x = at -> x - dist.x;
      object -> at_list[j].y = at -> y - dist.y;
      object -> at_list[j].z = at -> z - dist.z;
    }
  }
  checked_at[aid] = TRUE;
  i = 0;
  for (j=0; j<object -> atoms; j++) if (checked_at[j]) i ++;
  if (i == target) return TRUE;
  for (i=0; i<at -> numv; i++)
  {
    j = at -> vois[i];
    if (! checked_at[j])
    {
      if (rebuild_atom_neighbors (this_proj, step, object, target, j, & object -> at_list[j], checked_at)) return TRUE;
    }
  }
  return FALSE;
}

void reconstruct_coordinates (struct project * this_proj, struct insert_object * this_object, gboolean upcoord)
{
  int h, i, j, k;
  int * tmp_multi = allocint (this_proj -> coord -> totcoord[2]);
  for (i=0; i<this_object -> atoms; i++)
  {
    j = this_object -> at_list[i].coord[2];
    tmp_multi[j] = 1;
  }
  gboolean * checked_at = allocbool (this_object -> atoms);
  h = (this_proj -> cell.npt) ? this_proj -> modelgl -> anim -> last -> img -> step : 0;
  for (i=0; i<this_proj -> coord -> totcoord[2]; i++)
  {
    if (tmp_multi[i])
    {
      k = 0;
      for (j=0; j<this_object -> atoms; j++)
      {
        checked_at[j] = FALSE;
        if (this_object -> at_list[j].coord[2] == i)
        {
          k ++;
        }
      }
      for (j=0; j<this_object -> atoms; j++)
      {
        if (this_object -> at_list[j].coord[2] == i)
        {
          rebuild_atom_neighbors (this_proj, h, this_object, k, j, & this_object -> at_list[j], checked_at);
        }
      }
    }
  }
  g_free (checked_at);
  correct_pos_and_get_dim (this_object, TRUE);
  if (upcoord)
  {
    for (i=0; i<this_object -> atoms; i++)
    {
      j = this_object -> at_list[i].id;
      this_proj -> atoms[0][j].x = this_object -> at_list[i].x + this_object -> baryc[0];
      this_proj -> atoms[0][j].y = this_object -> at_list[i].y + this_object -> baryc[1];
      this_proj -> atoms[0][j].z = this_object -> at_list[i].z + this_object -> baryc[2];
      this_proj -> atoms[0][j].cloned = FALSE;
    }
  }
}

void reconstruct_coordinates_for_object (struct insert_object * this_object, struct project * this_proj, gboolean upcoord)
{
  int i, j, k;
  reconstruct_coordinates (this_proj, this_object, upcoord);

  if (upcoord)
  {
    if (this_proj -> modelgl -> atom_win)
    {
      if (this_proj -> modelgl -> atom_win -> win)
      {
        for (j=0; j<2; j++) this_proj -> modelgl -> saved_coord[j] = save_coordinates (this_proj, j);
      }
    }
    else if (this_proj -> modelgl -> mode == EDITION)
    {
      if (this_proj -> modelgl -> saved_coord[2])
      {
        for (i=0; i< this_object -> atoms; i++)
        {
          k = this_object -> at_list[i].id;
          this_proj -> modelgl -> saved_coord[2][k][0] = this_object -> at_list[i].x + this_object -> baryc[0];
          this_proj -> modelgl -> saved_coord[2][k][1] = this_object -> at_list[i].y + this_object -> baryc[1];
          this_proj -> modelgl -> saved_coord[2][k][2] = this_object -> at_list[i].z + this_object -> baryc[2];
        }
      }
    }
  }
}

void reconstruct_this_object_from_project (struct insert_object * this_object, struct project * this_proj, gboolean upcoord)
{
  int i, j, k, l;
  int ** old_bid = NULL;
  old_bid = allocdint (this_object -> bonds, 2);
  for (i=0; i<this_object -> bonds; i++)
  {
    for (j=0; j<2; j++) old_bid[i][j] = this_object -> ibonds[i][j];
  }
  g_free (this_object -> ibonds);
  this_object -> ibonds = allocdint (this_object -> bonds + this_object -> ifcl, 2);
  for (i=0; i<this_object -> bonds; i++)
  {
    for (j=0; j<2; j++) this_object -> ibonds[i][j] = old_bid[i][j];
  }
  if (old_bid) g_free (old_bid);
  old_bid = NULL;
  for (i=0; i<this_object -> ifcl; i++)
  {
    j = this_object -> bcid[i];
    k = this_object -> bonds;
    for (l=0; l<2; l++)
    {
      this_object -> ibonds[k][l] = j;
    }
    this_object -> bonds ++;
  }
  reconstruct_coordinates_for_object (this_object, this_proj, upcoord);
}

void correct_coordinates_for_object (struct project * this_proj, struct insert_object * this_object, gboolean upcoord, gboolean reconstruct)
{
  if (this_object -> ifcl)
  {
    if (reconstruct)
    {
      reconstruct_this_object_from_project (this_object, this_proj, upcoord);
      g_free (this_object -> bcid);
      this_object -> bcid = NULL;
      this_object -> ifcl = 0;
    }
    else
    {
      correct_pos_and_get_dim (this_object, FALSE);
    }
  }
  else
  {
    correct_pos_and_get_dim (this_object, TRUE);
  }
}

tint ulam_coord (glwin * view)
{
  tint pos;
  int m;
  pos.a = pos.b = pos.c = 0;
  if (view -> builder_win) return pos;
  int p = view -> nth_copy;
  if (p > 0)
  {
    pos.c = p/9;
    m = p - pos.c*9;
    pos.b = m/3;
    pos.a = p - pos.c*9 - pos.b*3;
  }
  return pos;
}

int being_copied;
struct insert_object * lib_object;

struct insert_object * duplicate_insert_object (struct insert_object * old_obj)
{
  struct insert_object * new_obj = g_malloc0 (sizeof*new_obj);
  new_obj -> origin = old_obj -> origin;
  new_obj -> type = old_obj -> type;
  new_obj -> dim = old_obj -> dim;
  new_obj -> name = g_strdup_printf ("%s", old_obj -> name);
  new_obj -> atoms = old_obj -> atoms;
  new_obj -> at_list = g_malloc0 (new_obj -> atoms*sizeof*new_obj -> at_list);
  int i;
  for (i=0; i<new_obj -> atoms; i++)
  {
    new_obj -> at_list[i] = * duplicate_atom (& old_obj -> at_list[i]);
  }
  new_obj -> old_z = duplicate_int (old_obj -> species, old_obj -> old_z);
  new_obj -> coord = duplicate_coord_info (old_obj -> coord);
  new_obj -> baryc = duplicate_double (3, old_obj -> baryc);

  new_obj -> occ = old_obj -> occ;
  new_obj -> species = old_obj -> species;
  if (old_obj -> bonds)
  {
    new_obj -> ibonds = g_malloc0 (old_obj -> bonds*sizeof*new_obj -> ibonds);
    int i;
    for (i=0; i<old_obj -> bonds; i++)
    {
      new_obj -> ibonds[i] = duplicate_int (2, old_obj -> ibonds[i]);
    }
  }
  new_obj -> bonds = old_obj -> bonds;
  return new_obj;
}

void create_object_from_library (int p)
{
  int i, j;
  lib_object = g_malloc0 (sizeof*lib_object);
  struct project * other_proj = get_project_by_id (p);
  i = other_proj -> natomes;
  lib_object -> type = FROM_LIBRARY;
  lib_object -> origin = p;
  lib_object -> name = g_strdup_printf ("%s", other_proj -> name);
  lib_object -> coord = duplicate_coord_info (other_proj -> coord);
  lib_object -> atoms = i;
  lib_object -> at_list = g_malloc0 (lib_object -> atoms*sizeof*lib_object -> at_list);
  lib_object -> occ = 1.0;
  lib_object -> species = other_proj -> nspec;
  lib_object -> old_z = allocint (other_proj -> nspec);
  for (j=0; j<other_proj -> nspec; j++) lib_object -> old_z[j] = (int) other_proj -> chemistry -> chem_prop[CHEM_Z][j];
  for (j=0; j<i; j++)
  {
    lib_object -> at_list[j] = * duplicate_atom (& other_proj -> atoms[0][j]);
  }
  correct_pos_and_get_dim (lib_object, TRUE);
  if (other_proj -> modelgl -> bonds[0][0])
  {
    i = other_proj -> modelgl -> bonds[0][0];
    lib_object -> ibonds = allocdint (i, 2);
    for (j=0; j<i; j++)
    {
      lib_object -> ibonds[j][0] = other_proj -> modelgl -> bondid[0][0][j][0];
      lib_object -> ibonds[j][1] = other_proj -> modelgl -> bondid[0][0][j][1];
    }
    lib_object -> bonds = i;
  }
  // Always one frag from library
  lib_object -> coord -> totcoord[2] = 1;
}

int in_object_bond_list (struct insert_object * object, int aid, int bid)
{
  int i;
  for (i=0; i<object -> bonds; i++)
  {
    if (object -> ibonds[i][0] == aid && object -> ibonds[i][1] == bid) return 1;
    if (object -> ibonds[i][1] == aid && object -> ibonds[i][0] == bid) return 1;
  }
  return 0;
}

void clean_object_vois (struct project * this_proj, struct insert_object * object, int * new_id, gboolean movtion)
{
  int i, j, k, l;
  int * tmpv;

  for (i=0; i<object -> atoms; i++)
  {
    if (object -> at_list[i].numv)
    {
      tmpv = duplicate_int (object -> at_list[i].numv, object -> at_list[i].vois);
      g_free (object -> at_list[i].vois);
      object -> at_list[i].vois = NULL;
      j = 0;
      for (k=0; k<object -> at_list[i].numv; k++)
      {
        l = tmpv[k];
        if (new_id[l])
        {
          j += (! movtion) ? in_object_bond_list(object, i, new_id[l]-1) : 1;
        }
      }
      if (j)
      {
        object -> at_list[i].vois = allocint (j);
        j = 0;
        for (k=0; k<object -> at_list[i].numv; k++)
        {
          l = tmpv[k];
          if (new_id[l])
          {
            if (! movtion ||  in_object_bond_list(object, i, new_id[l]-1))
            {
              object -> at_list[i].vois[j] = new_id[l]-1;
              j ++;
            }
          }
        }
      }
      object -> at_list[i].numv = j;
      g_free (tmpv);
    }
  }
}

void clean_object_bonds (struct project * proj, int o_step, int sid, int cid, struct insert_object * object, int * new_id, gboolean movtion)
{
  int h, i, j, k, l, m;
  gboolean doit;
  int ** tmpibonds = allocdint (proj -> modelgl -> bonds[o_step][0]+proj -> modelgl -> bonds[o_step][1], 2);
  if (proj -> modelgl -> bonds[o_step][1])
  {
    h = (movtion) ? 2 : 1;
  }
  else
  {
    h = 1;
  }
  i = 0;
  int * bcid = NULL;
  if (h == 2) bcid = allocint (proj -> modelgl -> bonds[o_step][1]);
  for (j=0; j<h; j++)
  {
    for (k=0; k<proj -> modelgl -> bonds[o_step][j]; k++)
    {
      l = proj -> modelgl -> bondid[o_step][j][k][0];
      m = proj -> modelgl -> bondid[o_step][j][k][1];
      doit = FALSE;
      if (new_id[l] && new_id[m]) doit = TRUE;
      if (doit)
      {
        tmpibonds[i][0] = new_id[l] - 1;
        tmpibonds[i][1] = new_id[m] - 1;
        i ++;
        if (j)
        {
          bcid[k] = 1;
          object -> ifcl ++;
        }
      }
    }
  }
  if (object -> ifcl)
  {
    object -> bcid = allocint (object -> ifcl);
    k = 0;
    for (j=0; j<proj -> modelgl -> bonds[o_step][1]; j++)
    {
      if (bcid[j])
      {
        object -> bcid[k] = new_id[j];
        k ++;
      }
    }
  }
  if (bcid) g_free (bcid);
  bcid = NULL;
  if (i)
  {
    object -> ibonds = allocdint (i, 2);
    for (j=0; j<i; j++)
    {
      object -> ibonds[j][0] = tmpibonds[j][0];
      object -> ibonds[j][1] = tmpibonds[j][1];
    }
  }
  g_free (tmpibonds);
  object -> bonds = i;
  clean_object_vois (proj, object, new_id, movtion);
}

void add_object_atoms (struct insert_object * this_object, struct project * this_proj,
                       int status, int cid, int o_step, int numa, int * new_id, gboolean alloc_new_id, atom_search * remove)
{
  int i, j, k;
  this_object -> atoms = numa;
  this_object -> at_list = g_malloc0 (this_object -> atoms*sizeof*this_object -> at_list);
  int * tmpsp = allocint (this_proj -> nspec);
  for (i=0; i<this_object -> atoms; i++)
  {
    j = new_id[i]-1;
    if (remove) remove -> todo[j] = 1;
    k = this_proj -> atoms[o_step][j].sp;
    this_object -> old_z[k] = (int) this_proj -> chemistry -> chem_prop[CHEM_Z][k];
    tmpsp[k] = 1;
    this_object -> at_list[i] = * duplicate_atom (& this_proj -> atoms[o_step][j]);
    if (i)
    {
      this_object -> at_list[i].prev = & this_object -> at_list[i-1];
      this_object -> at_list[i-1].next = & this_object -> at_list[i];
    }
  }

  for (i=0; i<this_proj -> nspec; i++) if (tmpsp[i]) this_object -> species ++;
  g_free (tmpsp);
  gboolean movtion = ((object_motion && this_proj -> modelgl -> rebuild[0][0]) || (! object_motion && this_proj -> modelgl -> rebuild[1][0]));
  if (alloc_new_id)
  {
    clean_object_bonds (this_proj, o_step, status, cid, this_object, new_id, movtion);
  }
  g_free (new_id);
  check_coord_modification (this_proj, NULL, & this_object -> at_list[0], this_object, FALSE, FALSE);
  correct_coordinates_for_object (this_proj, this_object, FALSE, movtion);
}

struct insert_object * create_object_from_species (struct project * this_proj, int sid, atom_search * remove)
{
  int i, j;
  struct insert_object * this_object =  g_malloc0 (sizeof*this_object);
  int o_step = this_proj -> modelgl -> anim -> last -> img -> step;
  this_object -> name = g_strdup_printf ("%s", this_proj -> chemistry -> label[sid]);
  i = this_proj -> chemistry -> nsps[sid];
  this_object -> type = FROM_SPEC;
  this_object -> origin = this_proj -> id;
  this_object -> atoms = i;
  this_object -> at_list = g_malloc0 (this_object -> atoms*sizeof*this_object -> at_list);
  this_object -> occ = 1.0;
  this_object -> coord = duplicate_coord_info (this_proj -> coord);
  this_object -> species = 1;
  this_object -> old_z = allocint (this_proj -> nspec);
  this_object -> old_z[sid] = (int) this_proj -> chemistry -> chem_prop[CHEM_Z][sid];
  int * new_id = NULL;
  new_id = allocint (this_proj -> natomes);
  gboolean alloc_new_id = FALSE;
  i = 0;
  for (j=0; j<this_proj -> natomes; j++)
  {
    if (this_proj -> atoms[o_step][j].sp == sid)
    {
      if (this_proj -> atoms[o_step][j].numv) alloc_new_id = TRUE;
      new_id[i] = j+1;
      i ++;
    }
  }
  add_object_atoms (this_object, this_proj, sid, -1, o_step, i, new_id, alloc_new_id, remove);
  return this_object;
}

struct insert_object * create_object_from_selection (struct project * this_proj)
{
  int i, j;
  struct insert_object * this_object =  g_malloc0 (sizeof*this_object);
  int o_step = this_proj -> modelgl -> anim -> last -> img -> step;
  this_object -> name = g_strdup_printf ("From selection");
  i = 0;
  for (j=0; j<this_proj->natomes; j++)
  {
    if (pasted_todo[j]) i++;
  }
  this_object -> type = FROM_DATA;
  this_object -> origin = this_proj -> id;
  this_object -> atoms = i;
  this_object -> at_list = g_malloc0 (this_object -> atoms*sizeof*this_object -> at_list);
  this_object -> occ = 1.0;
  this_object -> coord = duplicate_coord_info (this_proj -> coord);
  this_object -> species = this_proj -> nspec;
  this_object -> old_z = allocint (this_proj -> nspec);
  i = 0;
  int * new_id = NULL;
  new_id = allocint(this_proj -> natomes);
  gboolean alloc_new_id = FALSE;
  for (j=0; j<this_proj -> natomes; j++)
  {
    if (pasted_todo[j])
    {
      new_id[i] = j+1;
      if (this_proj -> atoms[o_step][j].numv) alloc_new_id = TRUE;
      i ++;
    }
  }
  add_object_atoms (this_object, this_proj, -1, -1, o_step, i, new_id, alloc_new_id, NULL);
  if (pasted_todo)
  {
    g_free (pasted_todo);
    pasted_todo = NULL;
  }
  return this_object;
}

struct insert_object * create_object_from_atom_coordination (struct project * this_proj, int coord, int aid, atom_search * remove)
{
  int i, j, k, l, m, n;
  struct insert_object * this_object =  g_malloc0 (sizeof*this_object);
  gchar * str;
  int o_step = this_proj -> modelgl -> anim -> last -> img -> step;
  i = this_proj -> atoms[o_step][aid].numv;
  k = this_proj -> atoms[o_step][aid].coord[coord];
  j = this_proj -> atoms[o_step][aid].sp;
  switch (coord)
  {
    case 0:
      if (i > 0)
      {
        str = g_strdup_printf ("%d-fold", i);
      }
      else
      {
        str = g_strdup_printf ("Isolated");
      }
      break;
    case 1:
      str = g_strdup_printf ("%s", env_name(this_proj, k, j, 1, NULL));
      break;
  }
  this_object -> name = g_strdup_printf ("%s - %s<sub>%d</sub>", str, this_proj -> chemistry -> label[j], aid+1);
  g_free (str);
  this_object -> type = - (coord + 3);
  this_object -> origin = this_proj -> id;
  this_object -> atoms = i+1;
  this_object -> at_list = g_malloc0 (this_object -> atoms*sizeof*this_object -> at_list);
  this_object -> occ = 1.0;
  this_object -> coord = duplicate_coord_info (this_proj -> coord);
  l = 1;
  m = this_proj -> atoms[o_step][aid].coord[1];
  for (k=0; k<this_proj -> nspec; k++)
  {
    if (k != j && this_proj -> coord -> partial_geo[j][m][k]) l ++;
  }
  this_object -> species = l;
  this_object -> old_z = allocint (this_proj -> nspec);
  this_object -> old_z[j] = (int) this_proj -> chemistry -> chem_prop[CHEM_Z][j];
  this_object -> at_list[0] = * duplicate_atom (& this_proj -> atoms[o_step][aid]);
  if (remove) remove_search -> todo[aid] = 1;
  struct distance dist;
  gboolean movtion = (object_motion && this_proj -> modelgl -> rebuild[0][0]) || (! object_motion && this_proj -> modelgl -> rebuild[1][0]);
  if (this_proj -> atoms[o_step][aid].numv)
  {
    int * new_id = NULL;
    new_id = allocint (this_proj -> natomes);
    new_id[aid] = 1;
    for (i=0; i<this_proj -> atoms[o_step][aid].numv; i++)
    {
      j = this_proj -> atoms[o_step][aid].vois[i];
      if (remove) remove_search -> todo[j] = 1;
      new_id[j] = i+2;
      this_object -> at_list[i+1] = * duplicate_atom (& this_proj -> atoms[o_step][j]);
      k = this_proj -> atoms[o_step][j].sp;
      this_object -> old_z[k] = (int) this_proj -> chemistry -> chem_prop[CHEM_Z][k];
      if (this_proj -> modelgl -> bonds[o_step][1])
      {
        dist = distance_3d (& this_proj -> cell, (this_proj -> cell.npt) ? o_step : 0, & this_proj -> atoms[o_step][aid], & this_proj -> atoms[o_step][j]);
        if (dist.pbc) this_object -> ifcl ++;
      }
    }
    this_object -> ibonds = allocdint (i, 2);
    for (j=0; j<i; j++)
    {
      this_object -> ibonds[j][0] = 0;
      this_object -> ibonds[j][1] = j+1;
    }
    this_object -> bonds = i;
    if (this_object -> ifcl)
    {
      this_object -> bcid = allocint (this_object -> ifcl);
      n = 0;
      for (i=0; i<this_proj -> atoms[o_step][aid].numv; i++)
      {
        j = this_proj -> atoms[o_step][aid].vois[i];
        for (k=0; k<this_proj -> modelgl -> bonds[o_step][1]; k++)
        {
          l = this_proj -> modelgl -> bondid[o_step][1][k][0];
          m = this_proj -> modelgl -> bondid[o_step][1][k][1];
          if ((l==aid && m==j) || (l==j && m==aid))
          {
            this_object -> bcid[n] = k;
            n ++;
            break;
          }
        }
        if (n == this_object -> ifcl) break;
      }
      if (n != this_object -> ifcl) this_object -> ifcl = n;
      if (! n)
      {
        g_free (this_object -> bcid);
        this_object -> bcid = NULL;
      }
    }
    clean_object_vois (this_proj, this_object, new_id, movtion);
    if (new_id) g_free (new_id);
  }
  correct_coordinates_for_object (this_proj, this_object, FALSE, movtion);
  return this_object;
}

struct insert_object * create_object_from_overall_coordination (struct project * this_proj, int coord, int aid, atom_search * remove)
{
  int i, j, k, l, m, n;
  struct insert_object * this_object =  g_malloc0 (sizeof*this_object);
  int o_step = this_proj -> modelgl -> anim -> last -> img -> step;
  this_object -> coord = duplicate_coord_info (this_proj -> coord);
  this_object -> old_z = allocint (this_proj -> nspec);
  this_object -> type = - (coord + 3);
  this_object -> origin = this_proj -> id;
  this_object -> occ = 1.0;
  this_object -> species = this_proj -> nspec;
  int * new_id = NULL;
  new_id = allocint(this_proj -> natomes);
  gchar * str;
  gboolean alloc_new_id;
  switch (coord)
  {
    case 0:
      for (k=0; k<this_proj -> natomes; k++)
      {
        if (this_proj -> atoms[o_step][k].coord[0] == aid)
        {
          i = this_proj -> atoms[o_step][k].numv;
          break;
        }
      }
      if (i)
      {
        alloc_new_id = TRUE;
        str = g_strdup_printf ("%d-fold", i);
      }
      else
      {
        alloc_new_id = FALSE;
        str = g_strdup_printf ("Isolated");
      }
      j = 0;
      for (k=0; k<this_proj -> natomes; k++)
      {
        if (this_proj -> atoms[o_step][k].numv == aid)
        {
          new_id[j] = k+1;
          j ++;
        }
      }
      break;
    case 1:
      j = 0;
      alloc_new_id = FALSE;
      for (k=0; k<this_proj -> natomes; k++)
      {
        l = this_proj -> atoms[o_step][k].sp;
        m = this_proj -> atoms[o_step][k].coord[coord-1];
        for (n=0; n<l; n++) m += this_proj -> coord -> ntg[1][n];
        if (m == aid)
        {
          new_id[j] = k+1;
          if (this_proj -> atoms[o_step][k].numv) alloc_new_id = TRUE;
          j ++;
        }
      }
      str = g_strdup_printf ("%s", env_name(this_proj, l, aid, 1, NULL));
      break;
  }
  this_object -> name = g_strdup_printf ("All %s - atom(s)", str);
  g_free (str);
  add_object_atoms (this_object, this_proj, -1, -1, o_step, j, new_id, alloc_new_id, remove);
  return this_object;
}

struct insert_object * create_object_from_frag_mol (struct project * this_proj, int coord, int geo, atom_search * remove)
{
  int i, j;
  struct insert_object * this_object =  g_malloc0 (sizeof*this_object);
  if (coord == 2)
  {
    this_object -> name = g_strdup_printf ("Fragment N°%d", geo+1);
  }
  else
  {
    this_object -> name = g_strdup_printf ("Molecule N°%d", geo+1);
  }
  int o_step = this_proj -> modelgl -> anim -> last -> img -> step;
  this_object -> type = - (coord + 3);
  this_object -> origin = this_proj -> id;
  this_object -> occ = 1.0;
  this_object -> coord = duplicate_coord_info (this_proj -> coord);
  this_object -> old_z = allocint (this_proj -> nspec);
  int * new_id = NULL;
  new_id = allocint(this_proj -> natomes);
  gboolean alloc_new_id = FALSE;
  i = 0;
  for (j=0; j<this_proj->natomes; j++)
  {
    if (this_proj -> atoms[o_step][j].coord[coord] == geo)
    {
      new_id[i] = j+1;
      if (this_proj -> atoms[o_step][j].numv) alloc_new_id = TRUE;
      i++;
    }
  }
  add_object_atoms (this_object, this_proj, geo, coord, o_step, i, new_id, alloc_new_id, remove);
  return this_object;
}

extern int test_this_fragment (int natomes, int fcoord, int fid, struct atom * new_list, int tmpbond[2], int ** tmpbondid[2], int * old_id, gboolean remove);

void adjust_object_frag_coord (struct insert_object * object)
{
  int i, j, k;
  int * corf = allocint (object -> coord -> totcoord[2]);
  int * rorf = allocint (object -> coord -> totcoord[2]);
  for (i=0; i<object -> atoms; i++)
  {
    j = object -> at_list[i].coord[2];
    rorf[j] ++;
  }
  j = k = 0;
  for (i=0; i<object -> coord -> totcoord[2]; i++)
  {
    corf[i] = k;
    if (rorf[i])
    {
      j++;
    }
    else
    {
      k++;
    }
  }
  object -> coord -> totcoord[2] = j;

  for (i=0; i<object -> atoms; i++)
  {
    j = corf[object -> at_list[i].coord[2]];
    object -> at_list[i].coord[2] -= j;
    object -> at_list[i].id = i;
  }
  remove_bonds_from_project (NULL, object, NULL, object -> atoms, & object -> at_list[0], FALSE);
}

int create_object_from_open_project (struct project * this_proj, int p)
{
  int i, j, k;
  lib_object =  g_malloc0 (sizeof*lib_object);
  struct project * other_proj;
  other_proj = get_project_by_id (p);
  int o_step = other_proj -> modelgl -> anim -> last -> img -> step;
  switch (this_proj -> modelgl -> other_status)
  {
    case 0:
      i = other_proj -> natomes - other_proj -> modelgl -> anim -> last -> img -> selected[0] -> selected;
      lib_object -> name = g_strdup_printf ("All non-selected atom(s) from: %s", get_project_by_id(p) -> name);
      break;
    case 1:
      i = other_proj -> modelgl -> anim -> last -> img -> selected[0] -> selected;
      lib_object -> name = g_strdup_printf ("All selected atom(s) from: %s", get_project_by_id(p) -> name);
      break;
    case 2:
      i = other_proj -> natomes;
      lib_object -> name = g_strdup_printf ("All atom(s) from: %s", get_project_by_id(p) -> name);
      break;
  }
  lib_object -> type = FROM_PROJECT;
  lib_object -> origin = p;
  lib_object -> atoms = i;
  lib_object -> at_list = g_malloc0 (lib_object -> atoms*sizeof*lib_object -> at_list);
  lib_object -> occ = 1.0;
  lib_object -> coord = duplicate_coord_info (other_proj -> coord);
  lib_object -> species = other_proj -> nspec;
  lib_object -> old_z = allocint (other_proj -> nspec);
  int * new_id = NULL;
  gboolean alloc_new_id = (this_proj -> modelgl -> other_status != 2 && i > 1 && (other_proj -> modelgl -> bonds[o_step][0] || other_proj -> modelgl -> bonds[o_step][1])) ? TRUE : FALSE;
  if (alloc_new_id) new_id = allocint (other_proj -> natomes);
  if (this_proj -> modelgl -> other_status == 2)
  {
    for (j=0; j<i; j++)
    {
      k = other_proj -> atoms[o_step][j].sp;
      lib_object -> old_z[k] = (int) other_proj -> chemistry -> chem_prop[CHEM_Z][k];
      lib_object -> at_list[j] = * duplicate_atom (& other_proj -> atoms[o_step][j]);
      if (j)
      {
        lib_object -> at_list[j].prev = & lib_object -> at_list[j-1];
        lib_object -> at_list[j-1].next = & lib_object -> at_list[j];
      }
    }
    lib_object -> ifcl = other_proj -> modelgl -> bonds[o_step][1];
    if (lib_object -> ifcl)
    {
      lib_object -> bcid = allocint (lib_object -> ifcl);
      for (j=0; j<lib_object -> ifcl; j++) lib_object -> bcid[j] = j;
    }
  }
  else
  {
    i = 0;
    for (j=0; j<other_proj -> natomes; j++)
    {
      if (other_proj -> atoms[o_step][j].pick[0] == this_proj -> modelgl -> other_status)
      {
        k = other_proj -> atoms[o_step][j].sp;
        if (alloc_new_id) new_id[j] = i+1;
        lib_object -> at_list[i] = * duplicate_atom (& other_proj -> atoms[o_step][j]);
        if (i)
        {
          lib_object -> at_list[i].prev = & lib_object -> at_list[i-1];
          lib_object -> at_list[i-1].next = & lib_object -> at_list[i];
        }
        lib_object -> old_z[k] = (int)other_proj -> chemistry -> chem_prop[CHEM_Z][k];
        i ++;
      }
    }
    if (alloc_new_id)
    {
      clean_object_bonds (other_proj, o_step, this_proj -> modelgl -> other_status, -1, lib_object, new_id, TRUE);
      g_free (new_id);
    }
    check_coord_modification (other_proj, NULL, & lib_object -> at_list[0], lib_object, FALSE, FALSE);
  }
  correct_coordinates_for_object (other_proj, lib_object, FALSE, TRUE);
  if (this_proj -> modelgl -> other_status < 2) adjust_object_frag_coord (lib_object);
  return FROM_PROJECT;
}

void clean_this_object (int orig, int act, struct project * this_proj, atom_search * asearch)
{
  struct insert_object * tmp_object = NULL;
  struct insert_object * object;
  switch (asearch -> action)
  {
    case REPLACE:
      object = this_proj -> modelgl -> atom_win -> to_be_inserted[act];
      break;
    default:
      object = this_proj -> modelgl -> atom_win -> to_be_moved[act];
      break;
  }
  while (object)
  {
    if ((object -> origin > -1 || object -> origin < -2) && object -> origin == orig)
    {
      if (object -> ibonds) g_free (object -> ibonds);
      if (object -> baryc) g_free (object -> baryc);
      if (object -> at_list) g_free (object -> at_list);
      if (object -> coord) g_free (object -> coord);
      if (object -> bcid) g_free (object -> bcid);
      object -> atoms = object -> bonds = 0;
      asearch -> in_selection --;
      if (object -> prev != NULL)
      {
        if (object -> next != NULL)
        {
          object -> next -> prev = object -> prev;
          object -> prev -> next = object -> next;
          tmp_object = object -> next;
          g_free (object);
          object = tmp_object;
        }
        else
        {
          object = object -> prev;
          g_free (object -> next);
          object -> next = NULL;
          break;
        }
      }
      else
      {
        if (object -> next != NULL)
        {
          object = object -> next;
          g_free (this_proj -> modelgl -> atom_win -> to_be_inserted[act]);
          object -> prev = NULL;
          switch (asearch -> action)
          {
            case REPLACE:
              this_proj -> modelgl -> atom_win -> to_be_inserted[act] = object;
              break;
            default:
              this_proj -> modelgl -> atom_win -> to_be_moved[act] = object;
              break;
          }
        }
        else
        {
          g_free (object);
          switch (asearch -> action)
          {
            case REPLACE:
              object = this_proj -> modelgl -> atom_win -> to_be_inserted[act] = NULL;
              break;
            default:
              object = this_proj -> modelgl -> atom_win -> to_be_moved[act] = NULL;
              break;
          }
        }
      }
    }
    else
    {
      object = object -> next;
    }
  }
}

void to_insert_in_project (int stat, int orig, struct project * this_proj, atom_search * asearch, gboolean visible)
{
  int i, j;
  struct insert_object * tmp_object = NULL;
  struct insert_object * object;
  int act;
  if (asearch -> pointer[0].c == 3)
  {
    act = 0;
  }
  else if (asearch -> pointer[0].c == 5)
  {
    act = 1;
  }
  else
  {
    act = asearch -> pointer[0].c - 5;
  }
  if ((! act || act == 3) && this_proj -> modelgl -> atom_win -> to_be_inserted[act])
  {
    clean_this_object (orig, act, this_proj, asearch);
  }

  if (stat == FROM_DATA) lib_object = duplicate_insert_object (copied_object);

  tint ulam;
  tmp_object = NULL;
  vec3_t coor_ins = vec3 (0.0,0.0,0.0);
  if (! visible)
  {
    if (act > 0 && this_proj -> modelgl -> anim && ! this_proj -> modelgl -> builder_win)
    {
      this_proj -> modelgl -> insert_coords = get_insertion_coordinates (this_proj -> modelgl);
      coor_ins = this_proj -> modelgl -> insert_coords;
    }
    else if (! act)
    {
      if (orig < -2 && asearch -> object && ! asearch -> mode)
      {
        int filter = get_asearch_filter (asearch);
        if (! filter)
        {
          tmp_object = create_object_from_species (this_proj, -orig-3, NULL);
        }
        else if (filter < 3)
        {
          tmp_object = create_object_from_atom_coordination (this_proj, filter-1, -orig-3, NULL);
        }
        else
        {
          tmp_object = create_object_from_frag_mol (this_proj, filter-1, -orig-3, NULL);
        }
        coor_ins = vec3 (tmp_object -> baryc[0], tmp_object -> baryc[1], tmp_object -> baryc[2]);
        if (tmp_object) g_free (tmp_object);
      }
      else if (orig > -1)
      {
        coor_ins = vec3 (this_proj -> atoms[0][orig].x, this_proj -> atoms[0][orig].y, this_proj -> atoms[0][orig].z);
      }
    }
  }
  if (stat > 0)
  {
    lib_object = g_malloc0 (sizeof*lib_object);
    lib_object -> type = stat;
    lib_object -> old_z = allocint (1);
    lib_object -> old_z[0] = (stat < 119) ? stat : 0.0;
    lib_object -> at_list = g_malloc0(sizeof*lib_object -> at_list);
    lib_object -> coord = g_malloc0 (sizeof*lib_object -> coord);
    lib_object -> coord -> species = 1;
    for (j=0; j<2; j++)
    {
      lib_object -> coord -> totcoord[j] = 1;
      lib_object -> coord -> ntg[j] = allocint (1);
      lib_object -> coord -> ntg[j][0] = 1;
      lib_object -> coord -> geolist[j] = allocdint (1, 1);
      if (j) lib_object -> coord -> partial_geo = alloctint (1, 1, 1);
    }
    lib_object -> coord -> totcoord[j] = 1;
    lib_object -> baryc = allocdouble (3);
    lib_object -> atoms = 1;
    lib_object -> occ = 1.0;
    lib_object -> species = 1;
    lib_object -> dim = get_object_dim (lib_object);
    if (stat > 119)
    {
      lib_object -> name = g_strdup_printf ("Empty position");
    }
    else
    {
      lib_object -> name = g_strdup_printf ("%s atom", periodic_table_info[stat].lab);
    }
  }

  ulam = ulam_coord (this_proj -> modelgl);

  if (this_proj -> modelgl -> atom_win -> to_be_inserted[act] == NULL)
  {
    this_proj -> modelgl -> atom_win -> to_be_inserted[act] = duplicate_insert_object (lib_object);
    object = this_proj -> modelgl -> atom_win -> to_be_inserted[act];
  }
  else
  {
    object = this_proj -> modelgl -> atom_win -> to_be_inserted[act];
    while (object -> next) object = object -> next;
    object -> next = duplicate_insert_object (lib_object);
    object -> next -> prev = object;
    object = object -> next;
  }
  if (act)
  {
    object -> id = (object -> prev) ? object -> prev -> id + 1 : 0;
  }
  else
  {
    object -> id = (orig > -1) ? orig : (orig < -2) ? abs(orig) - 3 : abs(orig) - 1;
  }
  object -> origin = orig;

  for (i=0; i<object -> atoms; i++)
  {
    object -> at_list[i].x += coor_ins.x + object -> dim*ulam.a;
    object -> at_list[i].y += coor_ins.y + object -> dim*ulam.b;
    object -> at_list[i].z += coor_ins.z + object -> dim*ulam.c;
  }
  asearch -> in_selection ++;

  if (visible)
  {
    int * atid;
    if (act == 1 || act == 2)
    {
      if (object -> id > 0)
      {
        atid = duplicate_int (object -> id, asearch -> todo);
        g_free (asearch -> todo);
      }
      allocate_todo (asearch, object -> id+1);
      for (i=0; i<object -> id; i++)
      {
        asearch -> todo[i] = atid[i];
      }
      asearch -> todo[i] = 0;
      update_search_tree (asearch);
    }
  }
}
