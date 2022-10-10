/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include "global.h"
#include "interface.h"
#include "glview.h"
#include "atom_edit.h"

extern G_MODULE_EXPORT void window_measures (GtkWidget * widg, gpointer data);
extern void dihedrals_loop (glwin * view, int id, int pi, GtkTreeStore * store);
extern void angles_loop (glwin * view, int id, int pi, GtkTreeStore * store);
extern void bonds_loop (glwin * view, int id, int pi, GtkTreeStore * store);
extern void update_label_selection (glwin * view, int pi);
#ifdef GTK4
extern G_MODULE_EXPORT void select_unselect_atoms (GSimpleAction * action, GVariant * parameter, gpointer data);
extern G_MODULE_EXPORT void select_unselect_coord (GSimpleAction * action, GVariant * parameter, gpointer data);
#else
G_MODULE_EXPORT void select_unselect_atoms (GtkWidget * widg, gpointer data);
G_MODULE_EXPORT void select_unselect_coord (GtkWidget * widg, gpointer data);
#endif
extern int selected_aspec;
extern int get_to_be_selected (glwin * view);

int find_bond_in_bonds (struct project * this_proj, int i, int j, int b, int id)
{
  int k, l, m;
  for (k=0; k < this_proj -> modelgl -> bonds[j][b]; k++)
  {
    l = this_proj -> modelgl -> bondid[j][b][k][0];
    m = this_proj -> modelgl -> bondid[j][b][k][1];
    if (this_proj -> atoms[j][l].show[b]) i ++;
    if (i == id) return k;
    if (b)
    {
      if (this_proj -> atoms[j][l].show[b]) i ++;
      if (i == id) return k;
    }
    if (this_proj -> atoms[j][m].show[b]) i ++;
    if (i == id) return k;
    if (b)
    {
      if (this_proj -> atoms[j][m].show[b]) i ++;
      if (i == id) return k;
    }
  }
  return -i;
}

int find_selected_bond (struct project * this_proj, int id)
{
  int i, j, k;

  i = this_proj -> modelgl -> clones_to_be_picked - 1;
  j = this_proj -> modelgl -> anim -> last -> img -> step;
  k = find_bond_in_bonds (this_proj, i, j, 0, id);
  if (k < 0 && this_proj -> modelgl -> anim -> last -> img -> draw_clones)
  {
    k = find_bond_in_bonds (this_proj, -k, j, 1, id);
    if (k < 0)
    {
      return -1;
    }
    else
    {
      return k;
    }
  }
  else if (k < 0)
  {
    return -1;
  }
  else
  {
    return k;
  }
}

int find_selected_atom (struct project * this_proj, int id)
{
  int i, j, k, l, m;
  i = -1;
  j = this_proj -> modelgl -> anim -> last -> img -> step;
  for (k=0; k<this_proj -> natomes; k++)
  {
    if (this_proj -> atoms[j][k].show[0]) i++;
    if (i == id) return k;
  }
  if (this_proj -> modelgl -> anim -> last -> img -> draw_clones)
  {
    for (k=0; k < this_proj -> modelgl -> bonds[j][1]; k++)
    {
      l = this_proj -> modelgl -> bondid[step][1][k][0];
      m = this_proj -> modelgl -> bondid[step][1][k][1];
      if (this_proj -> atoms[j][l].show[1]) i++;
      if (i == id) return l;
      if (this_proj -> atoms[j][m].show[1]) i++;
      if (i == id) return m;
    }
  }
  return -1;
}

int num_bonds (int i)
{
  return i*(i-1)/2;
}

int num_angles (int i)
{
  return i*(i-1)*(i-2)/2;
}

int num_dihedrals (int i)
{
  return i*(i-1)*(i-2)*(i-3)/2;
}

int objects[3] = {0, 0, 0};
int * object_was_selected[3] = {NULL, NULL, NULL};
int ** tmp_object_id[3] = {NULL, NULL, NULL};

void save_dihedral_selection (glwin * view, int pi)
{
  int i, j;
  image * img = view -> anim -> last -> img;
  if (img -> selected[pi] -> selected > 3 && img -> selected[pi] -> selected < MAX_IN_SELECTION-10)
  {
    objects[2] = i = num_dihedrals (img -> selected[pi] -> selected);
    tmp_object_id[2] = allocdint (i, 4);
    object_was_selected[2] = allocint(i);
    for (j=0; j<i; j++) object_was_selected[2][j] = img -> selected[pi] -> selected_dihedrals[j];
    dihedrals_loop (view, -2, pi, NULL);
    g_free (img -> selected[pi] -> selected_dihedrals);
  }
  else
  {
    objects[2] = 0;
  }
}

void update_dihedral_selection (glwin * view, int pi)
{
  int i;
  image * img = view -> anim -> last -> img;
  if (img -> selected[pi] -> selected > 3 && img -> selected[pi] -> selected < MAX_IN_SELECTION-10)
  {
    i = num_dihedrals (img -> selected[pi] -> selected);
    img -> selected[pi] -> selected_dihedrals = allocint (i);
    dihedrals_loop (view, -1, pi, NULL);
  }
}

void save_angle_selection (glwin * view, int pi)
{
  int i, j;
  image * img = view -> anim -> last -> img;
  if (img -> selected[pi] -> selected > 2 && img -> selected[pi] -> selected < MAX_IN_SELECTION)
  {
    objects[1] = i = num_angles (img -> selected[pi] -> selected);
    tmp_object_id[1] = allocdint (i, 3);
    object_was_selected[1] = allocint(i);
    for (j=0; j<i; j++) object_was_selected[1][j] = img -> selected[pi] -> selected_angles[j];
    angles_loop (view, -2, pi, NULL);
    g_free (img -> selected[pi] -> selected_angles);
  }
  else
  {
    objects[1] = 0;
  }
}

void update_angle_selection (glwin * view, int pi)
{
  int i;
  image * img = view -> anim -> last -> img;
  if (img -> selected[pi] -> selected > 2 && img -> selected[pi] -> selected < MAX_IN_SELECTION)
  {
    i = num_angles (img -> selected[pi] -> selected);
    img -> selected[pi] -> selected_angles = allocint (i);
    angles_loop (view, -1, pi, NULL);
  }
}

void save_bond_selection (glwin * view, int pi)
{
  int i, j;
  image * img = view -> anim -> last -> img;
  if (img -> selected[pi] -> selected > 1 && img -> selected[pi] -> selected < MAX_IN_SELECTION)
  {
    objects[0] = i = num_bonds (img -> selected[pi] -> selected);
    tmp_object_id[0] = allocdint (i, 2);
    object_was_selected[0] = allocint(i);
    for (j=0; j<i; j++) object_was_selected[0][j] = img -> selected[pi] -> selected_bonds[j];
    bonds_loop (view, -2, pi, NULL);
    g_free (img -> selected[pi] -> selected_bonds);
  }
  else
  {
    objects[0] = 0;
  }
}

void update_bond_selection (glwin * view, int pi)
{
  int i;
  image * img = view -> anim -> last -> img;
  if (img -> selected[pi] -> selected > 1 && img -> selected[pi] -> selected < MAX_IN_SELECTION)
  {
    i = num_bonds (img -> selected[pi] -> selected);
    img -> selected[pi] -> selected_bonds = allocint (i);
    bonds_loop (view, -1, pi, NULL);
  }
  if (view -> measure_win != NULL)
  {
    if (view -> measure_win -> selection_tree[0] != NULL)
    {
      update_selection_tree (view, pi, 0);
    }
  }
}

void save_all_selections (glwin * view, int pi)
{
  save_bond_selection (view, pi);
  save_angle_selection (view, pi);
  save_dihedral_selection (view, pi);
}

void update_all_selections (glwin * view, int pi)
{
  update_bond_selection (view, pi);
  update_angle_selection (view, pi);
  update_dihedral_selection (view, pi);
  if (view -> measure_win != NULL) update_label_selection (view, pi);
  int i;
  for (i=0; i<3; i++)
  {
    if (objects[i] > 0)
    {
      if (object_was_selected[i])
      {
        g_free (object_was_selected[i]);
        object_was_selected[i] = NULL;
      }
      if (tmp_object_id[i])
      {
        g_free (tmp_object_id[i]);
        tmp_object_id[i] = NULL;
      }
    }
    objects[i] = 0;
    if (view -> measure_win != NULL)
    {
      if (view -> measure_win -> selection_tree[i] != NULL)
      {
        update_selection_tree (view, pi, i);
      }
    }
  }
}

struct selatom * new_selatom (int id, int sp)
{
  struct selatom *  new_sel = g_malloc0 (sizeof*new_sel);
  new_sel -> id = id;
  new_sel -> sp = sp;
  return new_sel;
}

void update_selection_list (struct atom_selection * at_list, struct atom * at, gboolean add)
{
  int i;
  struct selatom * selection = at_list -> first;

  if (add)
  {
    selection = at_list -> last;
    if (at_list -> selected == 0)
    {
        at_list -> first = new_selatom (at -> id, at -> sp);
        at_list -> last = at_list -> first;
    }
    else
    {
      selection -> next = new_selatom (at -> id, at -> sp);
      selection -> next -> prev = selection;
      at_list -> last = selection -> next;
    }
  }
  else
  {
    selection = at_list -> first;
    for (i=0; i<at_list -> selected; i++)
    {
      if (selection -> id == at -> id)
      {
        if (i == 0 && at_list -> selected == 1)
        {
          at_list -> last = NULL;
          at_list -> first = NULL;
        }
        else if (i == 0)
        {
          at_list -> first = at_list -> first -> next;
          at_list -> first -> prev = NULL;
        }
        else
        {
          if (i == at_list -> selected-1)
          {
            at_list -> last = selection -> prev;
            at_list -> last -> next = NULL;
          }
          else
          {
            selection -> next -> prev = selection -> prev;
            selection -> prev -> next = selection -> next;
          }
        }
        break;
      }
      if (selection -> next != NULL) selection = selection -> next;
    }
  }
}

void process_selected_atom (struct project * this_proj, glwin * view, int id, int ac, int se, int pi)
{
  int i;
  i = view -> anim -> last -> img -> step;
  if (this_proj -> atoms[i][id].pick[pi])
  {
    if (! se)
    {
      this_proj -> atoms[i][id].pick[pi] = FALSE;
      update_selection_list (view -> anim -> last -> img -> selected[pi], & this_proj -> atoms[i][id], FALSE);
      view -> anim -> last -> img -> selected[pi] -> selected --;
    }
    else if (this_proj -> atoms[i][id].label[ac])
    {
      this_proj -> atoms[i][id].pick[pi] = FALSE;
      this_proj -> atoms[i][id].label[ac] = FALSE;
      view -> labelled = check_label_numbers (this_proj, 2);
      update_selection_list (view -> anim -> last -> img -> selected[pi], & this_proj -> atoms[i][id], FALSE);
      view -> anim -> last -> img -> selected[pi] -> selected --;
    }
    else
    {
      this_proj -> atoms[i][id].label[ac] = TRUE;
      view -> labelled = check_label_numbers (this_proj, 2);
    }
  }
  else
  {
    this_proj -> atoms[i][id].pick[pi] = TRUE;
    update_selection_list (view -> anim -> last -> img -> selected[pi], & this_proj -> atoms[i][id], TRUE);
    view -> anim -> last -> img -> selected[pi] -> selected ++;
  }
}

void process_selection (struct project * this_proj, glwin * view, int id, int ac, int se, int pi)
{
  int i, j, k;
  j =  view -> anim -> last -> img -> step;
  k = this_proj -> atoms[j][id].pick[pi];
  for (i=0; i<this_proj -> steps; i++)
  {
    if (i == j)
    {
      process_selected_atom (this_proj, view, id, ac, se, pi);
    }
    else
    {
      this_proj -> atoms[i][id].pick[pi] = ! k;
    }
  }
}

void process_the_hits (glwin * view, gint event_button, double ptx, double pty)
{
  int i, j, k, l, m, n, o, p, q;
  view -> picked = FALSE;
  GLubyte pixel[4];
  GLint viewport[4];
  int scale = gtk_widget_get_scale_factor (view -> win);
  glGetIntegerv (GL_VIEWPORT, viewport);
  glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
  glReadPixels (scale * view -> mouseX, viewport[3] - scale * view -> mouseY, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, pixel);

  int picked_id = pixel[0] + 256*pixel[1] + 256*256*pixel[2];
  for (i=0; i<view -> to_be_picked; i++)
  {
    if (picked_id == view -> color_to_pick[i])
    {
      j = i;
      view -> picked = TRUE;
      break;
    }
  }
  to_pop.action = 0;
  to_pop.x = 0.0;
  to_pop.y = 0.0;
  to_pop.pts[0] = to_pop.pts[1] = to_pop.pts[2] = to_pop.pts[3] = to_pop.pts[4] = -1;
  if (view -> picked)
  {
    struct project * this_proj = get_project_by_id(view -> proj);
    k = (is_atom_win_active(view) || (view -> mode == EDITION && view -> selection_mode == NSELECTION-1)) ? 1 : 0;
    l = view -> anim -> last -> img -> step;
    if (j < view -> clones_to_be_picked)
    {
      m = find_selected_atom (this_proj, j);
      n = (j < view -> atoms_to_be_picked) ? 0 : 1;
      o = m;
      p = -1;
      q = this_proj -> atoms[l][m].pick[k];
    }
    else
    {
      m = find_selected_bond (this_proj, j);
      n = (j <  view -> bonds_to_be_picked) ? 0 : 1;
      o = view -> bondid[l][n][m][0];
      p = view -> bondid[l][n][m][1];
      q = 0;
      if (this_proj -> atoms[l][o].pick[k] || this_proj -> atoms[l][p].pick[k]) q = 1;
    }
    if (m != -1)
    {
      if (event_button == 1)
      {
        if (view -> selection_mode == ATOMS || view -> selection_mode == NSELECTION-1 || is_atom_win_active(view))
        {
          save_all_selections (view, k);
          if (j < view -> clones_to_be_picked)
          {
            process_selection (this_proj, view, m, n, 1, k);
          }
          else
          {
            if (this_proj -> atoms[l][o].show[n]) process_selection (this_proj, view, o, n, 1, k);
            if (this_proj -> atoms[l][p].show[n]) process_selection (this_proj, view, p, n, 1, k);
          }
          update_all_selections (view, k);
        }
        else if (view -> selection_mode == 1)
        {
          opengl_project_changed (view -> proj);
          if (j < view -> clones_to_be_picked)
          {
            selected_status = ! this_proj -> atoms[l][m].pick[get_to_be_selected (view)];
#ifdef GTK4
            select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(m));
#else
            select_unselect_this_atom (NULL, GINT_TO_POINTER(m));
#endif
          }
          else
          {
            selected_status = ! this_proj -> atoms[l][o].pick[get_to_be_selected (view)];
#ifdef GTK4
            select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(o));
            select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(p));
#else
            select_unselect_this_atom (NULL, GINT_TO_POINTER(o));
            select_unselect_this_atom (NULL, GINT_TO_POINTER(p));
#endif
          }
        }
        else
        {
          opengl_project_changed (view -> proj);
          if (view -> selection_mode > 3)
          {
            p = view -> selection_mode-2;
          }
          else
          {
            p = view -> selection_mode;
          }
          if (j < view -> clones_to_be_picked)
          {
            n = this_proj -> atoms[l][m].coord[p];
            o = this_proj -> atoms[l][m].pick[0];
          }
          else
          {
            n = this_proj -> atoms[l][o].coord[p];
            o = this_proj -> atoms[l][o].pick[0];
          }
          if (view -> selection_mode > 3)
          {
            selected_aspec = -1;
#ifdef GTK4
            select_unselect_atoms (NULL, NULL, & view -> colorp[0][0]);
#else
            select_unselect_atoms (NULL, & view -> colorp[0][0]);
#endif
          }
          tint pointer;
          pointer.a = p;
          pointer.b = n;
          pointer.c = ! o;
#ifdef GTK4
          select_unselect_coord (NULL, NULL, & pointer);
#else
          select_unselect_coord (NULL, & pointer);
#endif
        }
        if (view -> mode == EDITION)
        {
          init_coordinates (this_proj, 1, FALSE, TRUE);
          view -> baryc[1] = get_bary (this_proj, 1);
          if (view -> rebuild[0][1]) view -> rebuild[0][0] = TRUE;
          if (view -> rebuild[1][1]) view -> rebuild[1][0] = TRUE;
        }
        int shaders[1] = {SELEC};
        re_create_md_shaders (1, shaders, this_proj);
        view -> create_shaders[LABEL] = TRUE;
        view -> create_shaders[MEASU] = TRUE;
        update (view);
        // if (view -> anim -> last -> img -> m_is_pressed && gtk_accelerator_get_default_mod_mask() == GDK_CONTROL_MASK) window_measures (NULL, view);
      }
      else if (event_button == 3)
      {
        to_pop.action = 2;
        to_pop.x = ptx;
        to_pop.y = pty;
        to_pop.pts[0] = q;
        to_pop.pts[1] = k;
        to_pop.pts[2] = o;
        to_pop.pts[3] = p;
        to_pop.pts[4] = n;
        // popup_selection (view, ptx, pty, q, k, o, p, n);
      }
    }
  }
  else if (event_button == 3)
  {
    to_pop.action = 1;
    to_pop.x = ptx;
    to_pop.y = pty;
    // popup_main_menu (view, ptx, pty);
  }
}
