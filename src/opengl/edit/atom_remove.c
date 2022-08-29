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

int tmbs;
int molcounter;
int atoms_in_frag;
int * togl;
int * ats_in_frag;
int * ats_id;
int * contacts;
int ** neighbors;

extern atom_search * allocate_atom_search (int proj, int action, int searchid, int tsize);

void set_mol_data (int the_atom, int the_mol)
{
  int ac, ad;
  molcounter ++;

  togl[the_atom] = the_mol;
  tmbs ++;

  if (molcounter == 10000) goto end;
  if (tmbs == atoms_in_frag) goto end;
  for (ac=0; ac < contacts[the_atom]; ac++)
  {
    ad = ats_id[neighbors[the_atom][ac]];
    if (! togl[ad] && molcounter < 100000) set_mol_data (ad, the_mol);
    if (tmbs == atoms_in_frag) goto end;
    if (molcounter == 10000) goto end;
  }

  molcounter --;
  end:;
}

int test_this_fragment (int natomes, int fcoord, int fid, struct atom * new_list, int tmpbond[2], int ** tmpbondid[2], int * old_id, gboolean remove)
{
  int i, j, k, l, m, n;
  struct atom * tmp_list;
  for (i=0; i<2; i++)
  {
    if (i)
    {
      ats_in_frag = allocint(k);
      ats_id = allocint(natomes);
      contacts = allocint(k);
      togl = allocint(k);
      neighbors = allocdint(k, k);
      // g_debug ("remove_bonds :: init :: k= %d", k);
    }
    k = 0;
    tmp_list = new_list;
    while (tmp_list)
    {
      if (! remove || old_id[tmp_list -> id] > 0)
      {
        if (tmp_list -> coord[2] == fid)
        {
          if (i)
          {
            ats_in_frag[k] = tmp_list -> id;
            ats_id[tmp_list -> id] = k;
            // g_debug ("remove_bonds :: rem :: k= %d, ats_in_frag[%d]= %d, ats_id[%d]= %d", k, k, ats_in_frag[k], tmp_list -> id, ats_id[tmp_list -> id]);
          }
          k ++;
        }
      }
      tmp_list = tmp_list -> next;
    }
  }
  atoms_in_frag = k;
  // Build neighbors map for frag fid
  for (i=0; i<atoms_in_frag; i++)
  {
    for (j=0; j<2; j++)
    {
      for (k=0; k < tmpbond[j]; k++)
      {
        l = tmpbondid[j][k][0];
        m = tmpbondid[j][k][1];
        if (l == ats_in_frag[i] || m == ats_in_frag[i])
        {
          n = (l == ats_in_frag[i]) ? m : l;
          neighbors[i][contacts[i]] = n;
          contacts[i] ++;
        }
      }
    }
  }
  // Check each atoms (recursive testing)
  int totmol = 0;
  tmbs = molcounter = 0;
  start:;
  m = n = 0;
  for (i=0; i<atoms_in_frag; i++)
  {
    m ++;
    if (molcounter > 0 && togl[i] == totmol)
    {
      for (j=0; j<contacts[i]; j++)
      {
        l = ats_id[neighbors[i][j]];
        if (! togl[l])
        {
          molcounter = 0;
          n = 1;
          set_mol_data (l, totmol);
          if (tmbs == atoms_in_frag) goto end;
          molcounter = 1;
          goto start;
        }
      }
    }
    else if (! molcounter && ! togl[i])
    {
      totmol ++;
      set_mol_data (i, totmol);
      if (tmbs == atoms_in_frag) goto end;
      if (molcounter == 10000) goto start;
      molcounter = 0;
    }
  }
  if (m == atoms_in_frag)
  {
    if (! n) molcounter = 0;
    goto start;
  }
  if (molcounter) goto start;
  end:;

  l = 0;
  for (i=0; i<atoms_in_frag; i++)
  {
    if (togl[i] > 1)
    {
      j = ats_in_frag[i];
      tmp_list = new_list;
      while (tmp_list -> id != j) tmp_list = tmp_list -> next;
      tmp_list -> coord[2] = fcoord + togl[i] - 2;
    }
    l = max (l, togl[i]);
  }
  g_free (ats_in_frag);
  g_free (ats_id);
  g_free (contacts);
  g_free (neighbors);
  g_free (togl);
  return  l-1;
}

struct atom * get_atom_pointer (int aid, struct atom * new_list)
{
  struct atom * tmp_list;
  tmp_list = new_list;
  while (tmp_list)
  {
    if (tmp_list -> id == aid) return tmp_list;
  }
  return NULL;
}

void remove_bonds_from_project (struct project * this_proj, int * old_id, int new_atoms, struct atom * new_list, gboolean remove)
{
  int i, j, k, l, m;
  int tmpbond[2];
  int ** tmpbondid[2];
  struct atom * tmp_list;
  gboolean split = FALSE;
  gboolean * frag_to_test;
  int * per_frag;
  int * in_frag;
  int * tmp_vois = NULL;

  /*tmp_list = new_list;
  while (tmp_list)
  {
    i = tmp_list -> id;
    if ((remove && old_id[i] > 0) || ! remove)
    {
      g_debug ("Keeping atom: %d", i+1);
      g_debug ("        atom: %d has: %d neighbor(s):", i+1, tmp_list -> numv);
      for (j=0; j<tmp_list -> numv; j++)
      {
      g_debug ("           n: %d, atom= %d", j+1, tmp_list -> vois[j]+1);
      }
    }
    tmp_list = tmp_list -> next;
  }*/

  // The next lines refresh the bonding information without calculating the distance matrix
  tmp_list = new_list;
  tmp_vois = allocint (20);
  while (tmp_list)
  {
    i = tmp_list -> id;
    if (tmp_list -> numv > 0)
    {
      k = 0;
      if (old_id[i] > 0)
      {
        for (j=0; j<tmp_list -> numv; j++)
        {
          l = tmp_list -> vois[j];
          if (old_id[l] > 0)
          {
            tmp_vois[k] = l;
            k ++;
          }
        }
      }
      else if (! remove && old_id[i] < 0)
      {
        for (j=0; j<tmp_list -> numv; j++)
        {
          l = tmp_list -> vois[j];
          if (old_id[l] < 0)
          {
            tmp_vois[k] = l;
            k ++;
          }
        }
      }
      g_free (tmp_list -> vois);
      tmp_list -> vois = NULL;
      tmp_list -> numv = k;
      if (k)
      {
        tmp_list -> vois = allocint (k);
        for (j=0; j<k; j++) tmp_list -> vois[j] = tmp_vois[j];
      }
    }
    tmp_list = tmp_list -> next;
  }
  g_free (tmp_vois);

  if (this_proj -> modelgl -> adv_bonding[0])
  {
    frag_to_test = allocbool (this_proj -> coord -> totcoord[2] + 1);
    per_frag = allocint (this_proj -> coord -> totcoord[2] + 1);
    in_frag = allocint (this_proj -> coord -> totcoord[2] + 1);
    for (i=0; i<this_proj -> natomes; i++)
    {
      j = this_proj -> atoms[0][i].coord[2];
      per_frag[j] ++;
      if (old_id[i] < 0) in_frag[j] ++;
    }

    for (i=0; i<this_proj -> coord -> totcoord[2]; i++)
    {
      // g_debug ("Frag check:: i= %d, per_frag[%d]= %d, in_frag[%d]= %d", i, i, per_frag[i], i, in_frag[i]);
      if (in_frag[i] && (per_frag[i] != in_frag[i]))
      {
        frag_to_test[i] = TRUE;
        split = TRUE;
      }
      else if (per_frag[i] == in_frag[i])
      {
        split = TRUE;
      }
    }
  }
  // for (i=0; i<this_proj -> natomes; i++) g_debug ("at:i= %d old_id[%i]= %d", i, i, old_id[i]);
  for (i=0; i<2; i++)
  {
    tmpbondid[i] = NULL;
    tmpbond[i] = 0;
    if (this_proj -> modelgl -> bonds[0][i])
    {
      tmpbondid[i] = allocdint (this_proj -> modelgl -> bonds[0][i], 2);
      j = 0;
      for (k=0; k<this_proj -> modelgl -> bonds[0][i]; k++)
      {
        l = this_proj -> modelgl -> bondid[0][i][k][0];
        m = this_proj -> modelgl -> bondid[0][i][k][1];
        if (old_id[l] > 0 && old_id[m] > 0)
        {
          tmpbondid[i][j][0] = l;
          tmpbondid[i][j][1] = m;
          j ++;
        }
        else if (! remove && (old_id[l] < 0 && old_id[m] < 0))
        {
          tmpbondid[i][j][0] = l;
          tmpbondid[i][j][1] = m;
          j ++;
        }
      }
      tmpbond[i] = j;
      // g_debug ("i= %d, tmpbond[%d]= %d", i, i, tmpbond[i]);
    }
  }
  if (split)
  {
    gboolean * show_this = duplicate_bool (this_proj -> coord -> totcoord[2], this_proj -> modelgl -> anim -> last -> img -> show_coord[2]);
    showfrag = duplicate_bool (this_proj -> coord -> totcoord[2], this_proj -> modelgl -> anim -> last -> img -> show_coord[2]);
    if (! remove)
    {
      tmp_list = new_list;
      while (tmp_list)
      {
        j = tmp_list -> id;
        if (old_id[j] < 0)
        {
          tmp_list -> coord[2] = this_proj -> coord -> totcoord[2];
        }
        tmp_list = tmp_list -> next;
      }
      frag_to_test[this_proj -> coord -> totcoord[2]] = TRUE;
      this_proj -> coord -> totcoord[2] ++;
      g_free (showfrag);
      showfrag = allocbool (this_proj -> coord -> totcoord[2]);
      for (m=0; m<this_proj -> coord -> totcoord[2]-1; m++) showfrag[m] = show_this[m];
      showfrag[m] = TRUE;
      g_free (show_this);
      show_this = duplicate_bool (this_proj -> coord -> totcoord[2], showfrag);
    }
    j = this_proj -> coord -> totcoord[2];
    l = 0;
    for (i=0; i<j; i++)
    {
      k = 0;
      if (per_frag[i] == in_frag[i] && i<j-1)
      {
        tmp_list = new_list;
        while (tmp_list)
        {
          if (! remove || old_id[tmp_list -> id] > 0)
          {
            if (tmp_list -> coord[2] > i-l)
            {
              tmp_list -> coord[2] --;
            }
          }
          tmp_list = tmp_list -> next;
        }
        g_free (showfrag);
        showfrag = allocbool (this_proj -> coord -> totcoord[2]-1);
        for (m=0; m<i-l; m++) showfrag[m] = show_this[m];
        for (m=i-l+1; m<this_proj -> coord -> totcoord[2]; m++) showfrag[m-1] = show_this[m];
        g_free (show_this);
        show_this = duplicate_bool (this_proj -> coord -> totcoord[2]-1, showfrag);
        k = -1;
        l ++;
      }
      else if (frag_to_test[i])
      {
        k = test_this_fragment (this_proj -> natomes, this_proj -> coord -> totcoord[2], i-l, new_list, tmpbond, tmpbondid, old_id, remove);
        if (k > 0)
        {
          g_free (showfrag);
          showfrag = allocbool (this_proj -> coord -> totcoord[2]+k);
          for (m=0; m<this_proj -> coord -> totcoord[2]; m++) showfrag[m] = show_this[m];
          for (m=this_proj -> coord -> totcoord[2]; m<this_proj -> coord -> totcoord[2]+k; m++) showfrag[m] = show_this[i-l];
          g_free (show_this);
          show_this = duplicate_bool (this_proj -> coord -> totcoord[2]+k, showfrag);
        }
      }
      this_proj -> coord -> totcoord[2] += k;
    }
    g_free (frag_to_test);
    g_free (show_this);
  }
  struct distance clo;
  for (i=0; i<2; i++)
  {
    if (this_proj -> modelgl -> bonds[0][i])
    {
      g_free (this_proj -> modelgl -> bondid[0][i]);
      this_proj -> modelgl -> bondid[0][i] = NULL;
      if (i)
      {
        g_free (this_proj -> modelgl -> clones[0]);
        this_proj -> modelgl -> clones[0] = NULL;
      }
      if (tmpbond[i])
      {
        if (i) this_proj -> modelgl -> clones[0] = g_malloc0 (tmpbond[i]*sizeof*this_proj -> modelgl -> clones[0]);
        this_proj -> modelgl -> bondid[0][i] = allocdint (tmpbond[i], 2);
        for (k=0; k<tmpbond[i]; k++)
        {
          this_proj -> modelgl -> bondid[0][i][k][0] = tmpbondid[i][k][0];
          this_proj -> modelgl -> bondid[0][i][k][1] = tmpbondid[i][k][1];
          //g_debug ("bc:: i= %d, j= %d, a= %d, b= %d", i, k, tmpbondid[i][k][0], tmpbondid[i][k][1]);
          if (i)
          {
            l = this_proj -> modelgl -> bondid[0][i][k][0];
            m = this_proj -> modelgl -> bondid[0][i][k][1];
            clo = distance_3d (& this_proj -> cell, 0, & this_proj -> atoms[0][l], & this_proj -> atoms[0][m]);
            this_proj -> modelgl -> clones[0][k].x = clo.x;
            this_proj -> modelgl -> clones[0][k].y = clo.y;
            this_proj -> modelgl -> clones[0][k].z = clo.z;
          }
        }
        if (new_list && old_id)
        {
          if (tmpbondid[i])
          {
            g_free (tmpbondid[i]);
            tmpbondid[i] = NULL;
          }
          tmpbondid[i] = allocdint (tmpbond[i], 2);
          tmp_list = new_list;
          k = 0;
          while (tmp_list)
          {
            if (! remove || old_id[tmp_list -> id] > 0)
            {
              for (l=0; l<tmpbond[i]; l++)
              {
                if (this_proj -> modelgl -> bondid[0][i][l][0] == tmp_list -> id)
                {
                  tmpbondid[i][l][0] = k;
                  m = 1;
                }
                if (this_proj -> modelgl -> bondid[0][i][l][1] == tmp_list -> id)
                {
                  tmpbondid[i][l][1] = k;
                  m = 0;
                }
              }
              k ++;
            }
            tmp_list = tmp_list -> next;
          }
        }
      }
      this_proj -> modelgl -> bonds[0][i] = tmpbond[i];
      this_proj -> modelgl -> allbonds[i] = tmpbond[i];
    }
    if (tmpbond[i])
    {
      for (j=0; j<tmpbond[i]; j++)
      {
        this_proj -> modelgl -> bondid[0][i][j][0] = tmpbondid[i][j][0];
        this_proj -> modelgl -> bondid[0][i][j][1] = tmpbondid[i][j][1];
      }
      g_free (tmpbondid[i]);
      tmpbondid[i] = NULL;
    }
  }
  /*tmp_list = new_list;
  while (tmp_list)
  {
    i = tmp_list -> id;
    if ((remove && old_id[i] > 0) || ! remove)
    {
      g_debug ("Correct atom: %d", i+1);
      g_debug ("        atom: %d has: %d neighbor(s):", i+1, tmp_list -> numv);
      for (j=0; j<tmp_list -> numv; j++)
      {
      g_debug ("           n: %d, atom= %d", j+1, tmp_list -> vois[j]+1);
      }
    }
    tmp_list = tmp_list -> next;
  }*/
}

void to_remove_this_list_of_objects (struct project * this_proj, atom_search * asearch)
{
  int i, j;
  struct insert_object * tmp_object;
  struct insert_object * tmp_replace;
  if (asearch -> action == REPLACE)
  {
    if (asearch -> pointer[0].c == 3)
    {
      tmp_replace = this_proj -> modelgl -> atom_win -> to_be_inserted[0];
    }
    else
    {
      tmp_replace = this_proj -> modelgl -> atom_win -> to_be_inserted[3];
    }
  }
  int filter = get_asearch_filter (asearch);
  int object = get_asearch_object (asearch);
  remove_search = allocate_atom_search (this_proj -> id, REMOVE, REMOVE, this_proj -> natomes);
  for (i=0; i<asearch -> todo_size; i++)
  {
    if (asearch -> todo[i])
    {
      tmp_object = NULL;
      if (filter > 0 && filter < 3 && ((object && ! asearch -> passivating) || (object == 2 && asearch -> passivating)))
      {
        tmp_object = create_object_from_atom_coordination (this_proj, filter-1, i, remove_search);
      }
      else if (filter > 2 && ((object && ! asearch -> passivating) || (object == 2 && asearch -> passivating)))
      {
        j = (asearch -> mode && filter == 4) ? filter - 1 : filter;
        tmp_object = create_object_from_frag_mol (this_proj, j-1, i, remove_search);
      }
      else
      {
        remove_search -> todo[i] = 1;
        if (asearch -> action == REPLACE)
        {
          tmp_object = g_malloc0(sizeof*tmp_object);
          tmp_object -> baryc = allocdouble (3);
          tmp_object -> baryc[0] = this_proj -> atoms[0][i].x;
          tmp_object -> baryc[1] = this_proj -> atoms[0][i].y;
          tmp_object -> baryc[2] = this_proj -> atoms[0][i].z;
        }
      }
      if (asearch -> action == REPLACE)
      {
        for (j=0; j<3; j++) tmp_replace -> baryc[j] += tmp_object -> baryc[j];
        tmp_replace = tmp_replace -> next;
      }
      if (tmp_object) g_free (tmp_object);
    }
  }

  i = 0;
  for (j=0; j<this_proj -> natomes; j++)
  {
    if (remove_search -> todo[j]) i++;
  }
  remove_search -> in_selection = i;
}

void to_passivate_using_the_objects (struct project * this_proj, atom_search * asearch)
{
  int i, j, k, l, m;
  struct insert_object * object = NULL;
  struct insert_object * tmp_oba, * tmp_obb;
  int filter = get_asearch_filter (asearch);
  int num_elem = asearch -> todo_size;
  int * passivate_todo = duplicate_int (num_elem, asearch -> todo);
  g_free (asearch -> todo);
  allocate_todo (asearch, this_proj -> natomes);
  if (this_proj -> modelgl -> atom_win -> to_be_inserted[3])
  {
    tmp_oba = this_proj -> modelgl -> atom_win -> to_be_inserted[3];
    object = duplicate_insert_object (tmp_oba);
    tmp_obb = object;
    while (tmp_oba -> next)
    {
      tmp_oba = tmp_oba -> next;
      tmp_obb -> next = duplicate_insert_object (tmp_oba);
      tmp_obb = tmp_obb -> next;
    }
    g_free (this_proj -> modelgl -> atom_win -> to_be_inserted[3]);
    this_proj -> modelgl -> atom_win -> to_be_inserted[3] = NULL;
  }
  gboolean doit;
  asearch -> in_selection = 0;
  tmp_oba = object;
  for (i=0; i<num_elem; i++)
  {
    if (passivate_todo[i])
    {
      for (j=0; j<this_proj -> natomes; j++)
      {
        doit = FALSE;
        if (this_proj -> atoms[0][j].pick[0])
        {
          k = this_proj -> atoms[0][j].sp;
          switch (filter)
          {
            case 0:
              if (k == i) doit = TRUE;
              break;
            case 1:
              if (this_proj -> atoms[0][j].numv == i) doit = TRUE;
              break;
            case 2:
              l = this_proj -> atoms[0][j].coord[1];
              for (m=0;m<k;m++) l += this_proj -> coord -> ntg[1][m];
              if (l == i) doit = TRUE;
              break;
            default:
              if (this_proj -> atoms[0][j].coord[filter-1] == i) doit = TRUE;
              break;
          }
          if (doit)
          {
            asearch -> todo[j] = 1;
            if (this_proj -> modelgl -> atom_win -> to_be_inserted[3] == NULL)
            {
              this_proj -> modelgl -> atom_win -> to_be_inserted[3] = duplicate_insert_object (tmp_oba);
              tmp_obb = this_proj -> modelgl -> atom_win -> to_be_inserted[3];
            }
            else
            {
              tmp_obb -> next = duplicate_insert_object (tmp_oba);
              tmp_obb -> next -> prev = tmp_obb;
              tmp_obb = tmp_obb -> next;
            }
            tmp_obb -> id = j;
            asearch -> in_selection ++;
          }
        }
      }
      if (tmp_oba -> next != NULL) tmp_oba = tmp_oba -> next;
    }
  }
  if (passivate_todo) g_free (passivate_todo);
  apply_action (this_proj, asearch);
}
