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
* This file: 'atom_remove.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  int test_this_fragment (int natomes, int fcoord, int fid, struct atom * new_list, int tmpbond[2], int ** tmpbondid[2], int * old_id, gboolean remove);

  void set_mol_data (int the_atom, int the_mol);
  void to_remove_this_list_of_objects (struct project * this_proj, atom_search * asearch);
  void to_passivate_using_the_objects (struct project * this_proj, atom_search * asearch);

  struct atom * get_atom_pointer (int aid, struct atom * new_list);

*/

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

/*
*  void set_mol_data (int the_atom, int the_mol)
*
*  Usage:
*
*  int the_atom :
*  int the_mol  :
*/
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

/*
*  int test_this_fragment (int natomes, int fcoord, int fid, struct atom * new_list, int tmpbond[2], int ** tmpbondid[2], int * old_id, gboolean remove)
*
*  Usage:
*
*  int natomes            :
*  int fcoord             :
*  int fid                :
*  struct atom * new_list :
*  int tmpbond[2]         :
*  int tmpbond[2]         :
*  int tmpbond[2]         :
*  int tmpbond[2]         :
*/
int test_this_fragment (int natomes, int fcoord, int fid, struct atom * new_list, int tmpbond[2], int ** tmpbondid[2], int * old_id, gboolean remove)
{
  int i, j, k, l, m, n;
  struct atom * tmp_list;
  gboolean modif;
  for (i=0; i<2; i++)
  {
    if (i)
    {
      ats_in_frag = allocint(k);
      ats_id = allocint(natomes);
      contacts = allocint(k);
      neighbors = allocdint(k, k);
      togl = allocint(k);
    }
    k = 0;
    tmp_list = new_list;
    while (tmp_list)
    {
      modif = FALSE;
      if (! remove)
      {
        modif = TRUE;
      }
      else if (old_id)
      {
         if (old_id[tmp_list -> id] > 0) modif = TRUE;
      }
      if (modif)
      {
        if (tmp_list -> coord[2] == fid)
        {
          if (i)
          {
            ats_in_frag[k] = tmp_list -> id;
            ats_id[tmp_list -> id] = k;
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

/*
*  struct atom * get_atom_pointer (int aid, struct atom * new_list)
*
*  Usage:
*
*  int aid                :
*  struct atom * new_list :
*/
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

gboolean * remove_bonds_from_project (struct project * this_proj, struct insert_object * this_object, int * old_id,
                                      int new_atoms, struct atom * new_list, gboolean remove)
{
  int h, i, j, k, l, m;
  int tmpbond[2];
  int ** tmpbondid[2];
  struct atom * tmp_list;
  gboolean split = FALSE;
  gboolean * frag_to_test;
  int * per_frag;
  int * in_frag;
  int * tmp_vois = NULL;
  int * id_mod = NULL;
  int * id_frag = NULL;

  /* tmp_list = new_list;
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
  h = -1;
  if (this_proj)
  {
    id_mod = allocint (this_proj -> natomes);
    while (tmp_list)
    {
      i = tmp_list -> id;
      if (old_id[i] > 0 || (! remove && old_id[i] < 0))
      {
        h ++;
        id_mod[i] = h;
      }
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
    tmp_list = new_list;
    while (tmp_list)
    {
      i = tmp_list -> id;
      if (old_id[i] > 0 || (! remove && old_id[i] < 0))
      {
        if (tmp_list -> numv > 0)
        {
          for (j=0; j<tmp_list -> numv; j++)
          {
            k = tmp_list -> vois[j];
            tmp_list -> vois[j] = id_mod[k];
          }
        }
      }
      tmp_list = tmp_list -> next;
    }
  }
  int tcf = (this_proj) ? this_proj -> coord -> totcoord[2] : this_object -> coord -> totcoord[2];
  int nat = (this_proj) ? this_proj -> natomes : this_object -> atoms;
  if (tcf)
  {
    frag_to_test = allocbool (tcf + 1);
    per_frag = allocint (tcf + 1);
    in_frag = allocint (tcf + 1);
    for (i=0; i<nat; i++)
    {
      j = (this_proj) ? this_proj -> atoms[0][i].coord[2] : this_object -> at_list[i].coord[2];
      per_frag[j] ++;
      if (this_proj)
      {
        if (old_id[i] > 0 || (! remove && old_id[i] <0)) in_frag[j] ++;
      }
      else
      {
        in_frag[j] ++;
      }
    }

    for (i=0; i<tcf; i++)
    {
      if (in_frag[i] > 1)
      {
        frag_to_test[i] = TRUE;
        split = TRUE;
      }
    }
  }

  for (i=0; i<2; i++)
  {
    tmpbondid[i] = NULL;
    tmpbond[i] = 0;
  }
  if (this_proj)
  {
    for (i=0; i<2; i++)
    {
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
  }
  else
  {
    tmpbond[0] = this_object -> bonds;
    tmpbondid[0] = allocdint (this_object -> bonds, 2);
    for (i=0; i<this_object -> bonds; i++)
    {
      tmpbondid[0][i][0] = this_object -> ibonds[i][0];
      tmpbondid[0][i][1] = this_object -> ibonds[i][1];
    }
  }
  gboolean * show_this;
  gboolean * showfrag = NULL;
  if (split)
  {
    if (this_proj)
    {
      show_this = duplicate_bool (this_proj -> coord -> totcoord[2], this_proj -> modelgl -> anim -> last -> img -> show_coord[2]);
      showfrag = duplicate_bool (this_proj -> coord -> totcoord[2], this_proj -> modelgl -> anim -> last -> img -> show_coord[2]);
    }
    if (! remove)
    {
      if (this_proj)
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
        frag_to_test[tcf] = TRUE;
        tcf ++;
        g_free (showfrag);
        showfrag = allocbool (tcf);
        for (m=0; m<tcf-1; m++) showfrag[m] = show_this[m];
        showfrag[m] = TRUE;
        g_free (show_this);
        show_this = duplicate_bool (tcf, showfrag);
      }
    }
    j = tcf;
    l = 0;
    for (i=0; i<j; i++)
    {
      k = 0;
      if (per_frag[i] == in_frag[i] && i<j-1)
      {
        tmp_list = new_list;
        while (tmp_list)
        {
          if (this_proj)
          {
            if (! remove || old_id[tmp_list -> id] > 0)
            {
              if (tmp_list -> coord[2] > i-l)
              {
                tmp_list -> coord[2] --;
              }
            }
          }
          else
          {
            if (tmp_list -> coord[2] > i-l)
            {
              tmp_list -> coord[2] --;
            }
          }
          tmp_list = tmp_list -> next;
        }
        if (this_proj)
        {
          g_free (showfrag);
          showfrag = allocbool (tcf-1);
          for (m=0; m<i-l; m++) showfrag[m] = show_this[m];
          for (m=i-l+1; m<tcf; m++) showfrag[m-1] = show_this[m];
          g_free (show_this);
          show_this = duplicate_bool (tcf-1, showfrag);
        }
        k = -1;
        l ++;
      }
      else if (frag_to_test[i])
      {
        k = test_this_fragment (nat, tcf, i-l, new_list, tmpbond, tmpbondid, old_id, remove);
        if (k > 0)
        {
          if (this_proj)
          {
            g_free (showfrag);
            showfrag = allocbool (tcf+k);
            for (m=0; m<tcf; m++) showfrag[m] = show_this[m];
            for (m=tcf; m<tcf+k; m++) showfrag[m] = show_this[i-l];
            g_free (show_this);
            show_this = duplicate_bool (tcf+k, showfrag);
          }
        }
      }
      tcf += k;
    }
    g_free (frag_to_test);
    if (this_proj) g_free (show_this);
  }

  id_frag = allocint (tcf);
  tmp_list = new_list;
  i = 0;
  while (tmp_list)
  {
    j = tmp_list -> id;

    if ((this_proj && (old_id[j] > 0 || (! remove && old_id[j] <0))) || this_object)
    {
      if (! id_frag[tmp_list -> coord[2]])
      {
        id_frag[tmp_list -> coord[2]] = i+1;
        i ++;
      }
    }
    tmp_list = tmp_list -> next;
  }
  tmp_list = new_list;
  while (tmp_list)
  {
    tmp_list -> coord[2] = id_frag[tmp_list -> coord[2]] - 1;
    tmp_list = tmp_list -> next;
  }
  if (i < tcf)
  {
    if (this_proj && split)
    {
      show_this = allocbool (i);
      for (j=0; j<tcf; j++)
      {
        if (id_frag[j]) show_this[id_frag[j]-1] = showfrag[j];
      }
      g_free (showfrag);
      showfrag = duplicate_bool (i, show_this);
      g_free (show_this);
    }
    tcf = i;
  }
  if (this_proj)
  {
    this_proj -> coord -> totcoord[2] = tcf;
  }
  else
  {
    this_object -> coord -> totcoord[2] = tcf;
  }
  if (id_frag)
  {
    g_free (id_frag);
    id_frag = NULL;
  }

  if (this_proj)
  {
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
            this_proj -> modelgl -> bondid[0][i][k][0] = id_mod[tmpbondid[i][k][0]];
            this_proj -> modelgl -> bondid[0][i][k][1] = id_mod[tmpbondid[i][k][1]];
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
        }
        this_proj -> modelgl -> bonds[0][i] = tmpbond[i];
        this_proj -> modelgl -> allbonds[i] = tmpbond[i];
      }
      if (tmpbondid[i])
      {
        g_free (tmpbondid[i]);
        tmpbondid[i] = NULL;
      }
    }
  }

  if (id_mod)
  {
    g_free (id_mod);
    id_mod = NULL;
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
  return showfrag;
}

/*
*  void to_remove_this_list_of_objects (struct project * this_proj, atom_search * asearch)
*
*  Usage:
*
*  struct project * this_proj : the target project
*  atom_search * asearch      :
*/
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

/*
*  void to_passivate_using_the_objects (struct project * this_proj, atom_search * asearch)
*
*  Usage:
*
*  struct project * this_proj : the target project
*  atom_search * asearch      :
*/
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
