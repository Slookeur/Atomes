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
#include "calc.h"
#include "interface.h"
#include "glview.h"

int active_sel;
int * new_at;
int ** sel_at;
int a_ato;
extern int * astr;
extern int vdw_id;
extern ColRGBA init_color (int id, int numid);
struct field_atom * at_to_remove;
GtkTreeViewColumn * ato_col[4];
GtkCellRenderer * ato_cell[4];
extern GtkWidget * remove_label;
extern GtkCellRenderer * remove_renderer[5];
extern GtkTreeViewColumn * remove_col[5];
extern float val;
extern float * val_at;
extern void field_selection (int i, int viz, int lab, int aid);
extern void field_unselect_all ();
extern void compare_non_bonded (gchar * fatom);
extern void init_all_impropers_inversions (int stru);
extern void init_default_shaders (glwin * view);

void set_sensitive_atom (GtkTreeViewColumn * col,
                         GtkCellRenderer   * renderer,
                         GtkTreeModel      * mod,
                         GtkTreeIter       * iter,
                         gpointer          data)
{
  int m;
  gtk_tree_model_get (mod, iter, 0, & m, -1);
  if (a_ato && m-1 != new_at[0])
  {
    gtk_cell_renderer_set_visible (renderer, FALSE);
  }
  else
  {
    gtk_cell_renderer_set_visible (renderer, TRUE);
  }
}

void atom_set_color_and_markup (GtkTreeViewColumn * col,
                                GtkCellRenderer   * renderer,
                                GtkTreeModel      * mod,
                                GtkTreeIter       * iter,
                                gpointer          data)
{
  int i, j;
  gtk_tree_model_get (mod, iter, 3, & j, -1);
  gtk_tree_model_get (mod, iter, 0, & i, -1);
  set_renderer_color (j, renderer, init_color (i, num_field_objects));
}

G_MODULE_EXPORT void select_field_atom (GtkCellRendererToggle * cell_renderer,
                                        gchar * string_path,
                                        gpointer data)
{
  int i, j;
  GtkTreeStore ** model = (GtkTreeStore **)data;
  GtkTreeIter iter;
  GtkTreePath * path = gtk_tree_path_new_from_string (string_path);
  gtk_tree_model_get_iter (GTK_TREE_MODEL(* model), & iter, path);
  if (gtk_cell_renderer_toggle_get_active(cell_renderer))
  {
    toviz.c = 0;
    tmp_fbt = get_active_atom (tmp_fmol -> id, new_at[a_ato-1]);
    for (j=0; j<tmp_fbt -> num; j++) field_selection (tmp_fbt -> list[j], FALSE, FALSE, new_at[a_ato-1]);
    new_at[a_ato-1] = -1;
    a_ato --;
    gtk_tree_store_set (* model, & iter, 3, 0, -1);
  }
  else
  {
    a_ato ++;
    gtk_tree_store_set (* model, & iter, 3, 1, -1);
    gtk_tree_model_get (GTK_TREE_MODEL(* model), & iter, 0, & i, -1);
    new_at[a_ato-1] = i-1;
    toviz.c = 1;
    tmp_fbt = get_active_atom (tmp_fmol -> id, new_at[a_ato-1]);
    for (j=0; j<tmp_fbt -> num; j++) field_selection (tmp_fbt -> list[j], TRUE, TRUE, new_at[a_ato-1]);
  }
  init_default_shaders (tmp_view);
  gtk_tree_view_column_set_cell_data_func (remove_col[3],
                                           remove_renderer[3],
                                           set_sensitive_atom, NULL, NULL);
  i = gtk_combo_box_get_active(GTK_COMBO_BOX(combo_mol[0]));
  if (new_at[0] == -1)
  {
    gtk_label_set_text (GTK_LABEL(remove_label), remove_text(-2, 0, at_to_remove -> name));
  }
  else
  {
    gtk_label_set_text (GTK_LABEL(remove_label), remove_text(i, new_at[0], at_to_remove -> name));
  }
  gtk_label_set_use_markup (GTK_LABEL(remove_label), TRUE);
}

void clean_old_atom (struct field_atom * at, int atos, int * atid)
{
  int h, i, j, k, l, m;
  gboolean save;
  h = at -> num - atos*tmp_fmol -> multi;
  int * tlist = allocint (h);
  int * dlist = allocint (h);
  j = -1;
  for (i=0; i < at -> num; i++)
  {
    save = TRUE;
    for (k=0; k<atos; k++)
    {
      if (at -> list_id[i] == atid[k])
      {
        save = FALSE;
        break;
      }
    }
    if (save)
    {
      j ++;
      tlist[j] = at -> list[i];
      dlist[j] = at -> list_id[i];
    }
  }
  at -> num = h;
  g_free (at -> list);
  at -> list = duplicate_int (h, tlist);
  g_free (at -> list_id);
  at -> list_id = duplicate_int (h, dlist);
  for (i=0; i<h; i++)
  {
    k = at -> list[i];
    l = at -> list_id[i];
    for (m=0; m< tmp_fmol -> multi; m++)
    {
      if (tmp_proj -> atoms[0][k].coord[2] == tmp_fmol -> fragments[m]) break;
    }
    tmp_fmol -> atoms_id[l][m].b = i;
  }
  g_free (tlist);
  g_free (dlist);
}

// extern void print_all_field_struct (struct field_molecule * mol, int str);
void adjust_field_struct (int oid, int k, struct field_struct * olds)
{
  int i, j;
  gboolean doit;
  struct field_struct * str;
  i = struct_id(k);
  int * aids = allocint (i);

  while (olds)
  {
    doit = TRUE;
    if (oid < 0)
    {
      for (j=0; j<i; j++) aids[j] = olds -> aid[j];
    }
    else
    {
      for (j=0; j<i; j++)
      {
        if (olds -> aid[j] < oid)
        {
          aids[j] = olds -> aid[j];
        }
        else if (olds -> aid[j] > oid)
        {
          aids[j] = olds -> aid[j] - 1;
        }
        else
        {
          doit = FALSE;
        }
      }
    }
    if (doit)
    {
      j = get_struct_id_from_atom_id (k, aids);
      if (j > 0)
      {
        str = get_active_struct (k, tmp_fmol -> id, j-1);
        str -> def = duplicate_field_prop (olds -> def, k);
        if (olds -> other)
        {
          duplicate_other_prop (oid, olds, str);
        }
      }
    }
    olds = olds -> next;
  }
  // if (k == 0) print_all_field_struct (tmp_fmol, k);
  g_free (aids);
}

G_MODULE_EXPORT void run_add_atom_dialog (GtkDialog * add_dialog, gint response_id, gpointer data)
{
  gboolean done = FALSE;
  selection_confirmed = FALSE;
  int i, j, k;
  gchar * str;
  switch (response_id)
  {
    case GTK_RESPONSE_APPLY:
      if (a_ato > 0 && a_ato < tmp_fat -> num / tmp_fmol -> multi)
      {
        if (a_ato > 1)
        {
          str = g_strdup_printf ("%d atoms have been selected !", a_ato);
          str = g_strdup_printf ("%s\nConfirm this choice and create a new field atom to describe them ?", str);
        }
        else
        {
          str = g_strdup_printf ("A single atom has been selected !");
          str = g_strdup_printf ("%s\nIs this correct, create a new field atom to describe it ?", str);
        }
        field_question (str, G_CALLBACK(confirm_selection), NULL);
        if (selection_confirmed)
        {
          done = TRUE;
          i = gtk_combo_box_get_active(GTK_COMBO_BOX(combo_mol[0]));
          tmp_fbt = get_active_atom (i, tmp_fmol -> atoms-1);
          new_at = allocint(a_ato);
          j = 0;
          for (k=0; k<tmp_fat -> num/tmp_fmol -> multi; k++)
          {
            if (sel_at[1][k])
            {
              new_at[j] = sel_at[0][k];
               j ++;
            }
          }
          tmp_fbt -> next = init_field_atom (tmp_fmol -> atoms,
                                             OTHER,
                                             tmp_fat -> sp,
                                             a_ato*tmp_fmol -> multi,
                                             -1,
                                             new_at);
          tmp_fbt -> next -> prev = g_malloc (sizeof*tmp_fbt -> next -> prev);
          tmp_fbt -> next -> prev = tmp_fbt;
          tmp_fmol -> atoms ++;
          clean_old_atom (tmp_fat, a_ato, new_at);
          g_free (new_at);
          struct field_struct * struct_saved[8];
          for (k=0; k<8; k++)
          {
            struct_saved[k] = duplicate_field_struct_list (tmp_fmol -> first_struct[k], FALSE);
          }
          init_all_field_struct (FALSE);
          // Cross check saved vs. new struct
          for (k=0; k<8; k++)
          {
            adjust_field_struct (-1, k, struct_saved[k]);
            clean_field_struct_list (struct_saved[k]);
          }
          // Update non bonded interactions
          compare_non_bonded (tmp_fbt -> next -> name);
        }
      }
      break;
    default:
      done = TRUE;
      break;
  }
  if (done)
  {
    destroy_this_dialog (add_dialog);
    field_unselect_all ();
    update_field_trees ();
  }
}

G_MODULE_EXPORT void run_select_atom_dialog (GtkDialog * select_dialog, gint response_id, gpointer data)
{
  int sid = GPOINTER_TO_INT (data);
  gboolean done = FALSE;
  gboolean redone;
  gboolean save;
  gchar * str;
  int i, j, k, l;
  redone = FALSE;
  save = FALSE;
  switch (response_id)
  {
    case GTK_RESPONSE_APPLY:
      if (sid == 6 || sid == 7)
      {
        i = sid - 6;
        if (tmp_fpmf -> num[i] > 0)
        {
          k = 0;
          for (j=0; j<tmp_fmol -> mol -> natoms; j++)
          {
            if (sel_at[0][j]) k ++;
          }
        }
        else
        {
          k = a_ato;
        }
      }
      else if (sid == 8)
      {
        if (tmp_frig -> num > 0)
        {
          k = 0;
          for (j=0; j<tmp_fmol -> mol -> natoms; j++)
          {
            if (sel_at[0][j]) k ++;
          }
        }
        else
        {
          k = a_ato;
        }
      }
      else
      {
        k = a_ato;
      }
      if (k > 1)
      {
        if (sid < 2 || sid > 4)
        {
          str = g_strdup_printf ("%d atoms selected !\nIs this correct ?", k);
        }
        else
        {
          str = g_strdup_printf ("%d atoms selected !\nOnly a single atom is required !", k);
          redone = TRUE;
        }
      }
      else if (k == 1)
      {
        switch (sid)
        {
          case 1:
            str = g_strdup_printf ("A single atom selected !\nIs this correct ?");
            break;
          default:
            done = TRUE;
            save = TRUE;
            break;
        }
      }
      else
      {
        str = g_strdup_printf ("No atom selected !\nIs this correct ?");
      }
      if (! done)
      {
        selection_confirmed = FALSE;
        field_question (str, G_CALLBACK(confirm_selection), NULL);
        g_free (str);
        if (selection_confirmed && ! redone)
        {
          done = TRUE;
          save = TRUE;
        }
      }
      break;
    default:
      done = TRUE;
      break;
  }
  if (done)
  {
    destroy_this_dialog (select_dialog);
    if (save)
    {
      if (sid == 1)
      {
        tmp_fat -> frozen = a_ato;
        for (i=0; i < tmp_fat -> num/tmp_fmol -> multi; i++)
        {
          j = sel_at[0][i];
          k = sel_at[1][i];
          for (l=0; l<tmp_fat -> num; l++)
          {
            if (tmp_fat -> list_id[l] == j)
            {
              tmp_fat -> frozen_id[l] = k;
            }
          }
        }
      }
      else if ((sid > 1 && sid < 6) || sid == 9)
      {
        for (j=0; j<tmp_fmol -> mol -> natoms; j++) if (sel_at[0][j]) break;
        switch (sid)
        {
          case 2:
            tmp_fshell -> ia[0] = j+1;
            break;
          case 3:
            tmp_fshell -> ia[1] = j+1;
            break;
          case 4:
            tmp_fcons -> ia[0] = j+1;
            break;
          case 5:
            tmp_fcons -> ia[1] = j+1;
            break;
          case 9:
            tmp_ftet -> num = j+1;
            break;
        }
      }
      else if (sid == 6 || sid == 7)
      {
        tmp_fpmf -> num[i] = k;
        tmp_fpmf -> list[i] = NULL;
        tmp_fpmf -> weight[i] = NULL;
        if (k > 0)
        {
          tmp_fpmf -> list[i] = allocint(tmp_fpmf -> num[i]);
          tmp_fpmf -> weight[i] = allocfloat(tmp_fpmf -> num[i]);
          k = -1;
          for (j=0; j<tmp_fmol -> mol -> natoms; j++)
          {
            if (sel_at[0][j])
            {
              k ++;
              tmp_fpmf -> list[i][k] = j;
              tmp_fpmf -> weight[i][k] = val_at[j];
            }
          }
        }
      }
      else if (sid == 8)
      {
        tmp_frig -> num = k;
        tmp_frig -> list = NULL;
        if (k > 0)
        {
          tmp_frig -> list = allocint(tmp_frig -> num);
          k = -1;
          for (j=0; j<tmp_fmol -> mol -> natoms; j++)
          {
            if (sel_at[0][j])
            {
              k ++;
              tmp_frig -> list[k] = j;
            }
          }
        }
      }
      else if (sid > 10 && sid < SEXTERN)
      {
        i = sid - 11;
        tmp_fbody -> na[i] = get_active_body (vdw_id, 0)-> na[0];
        tmp_fbody -> a[i] = duplicate_int (get_active_body (vdw_id, 0) -> na[0], get_active_body (vdw_id, 0)-> a[0]);
        tmp_fbody -> ma[i] = duplicate_int (get_active_body (vdw_id, 0)-> na[0], get_active_body (vdw_id, 0)-> ma[0]);
      }
    }
    field_unselect_all ();
  }
}

void merging_atoms (struct field_atom * to_merge, struct field_atom * to_remove, gboolean upda)
{
  int * tmp_ato = allocint (to_merge -> num + to_remove -> num);
  int * tmp_atd = allocint (to_merge -> num + to_remove -> num);
  int * tmp_fre = allocint (to_merge -> num + to_remove -> num);
  int i, j, k, l;
  i = to_merge -> num + to_remove -> num;
  for (j=0; j<to_merge -> num; j++)
  {
    tmp_ato[j] = to_merge -> list[j];
    tmp_atd[j] = to_merge -> list_id[j];
    tmp_fre[j] = to_merge -> frozen_id[j];
  }

  for (j=to_merge -> num; j<i ; j++)
  {
    tmp_ato[j] = to_remove -> list[j - to_merge -> num];
    tmp_atd[j] = to_remove -> list_id[j - to_merge -> num];
    tmp_fre[j] = to_remove -> frozen_id[j - to_merge -> num];
  }

  int val_a, val_b, val_c;
  for (j=1; j<i; j++)
  {
    val_a = tmp_ato[j];
    val_b = tmp_atd[j];
    val_c = tmp_fre[j];
    for (k=j-1; k>-1; k--)
    {
      if (tmp_atd[k] < val_b) break;
      tmp_ato[k+1] = tmp_ato[k];
      tmp_atd[k+1] = tmp_atd[k];
      tmp_fre[k+1] = tmp_fre[k];
    }
    tmp_ato[k+1] = val_a;
    tmp_atd[k+1] = val_b;
    tmp_fre[k+1] = val_c;
  }
  to_merge -> num = i;
  g_free (to_merge -> list);
  to_merge -> list = duplicate_int (to_merge -> num, tmp_ato);
  g_free (tmp_ato);
  g_free (to_merge -> list_id);
  to_merge -> list_id = duplicate_int (to_merge -> num, tmp_atd);
  g_free (tmp_atd);
  g_free (to_merge -> frozen_id);
  to_merge -> frozen_id = duplicate_int (to_merge -> num, tmp_fre);
  g_free (tmp_fre);

  if (upda)
  {
    for (i=0; i<to_merge -> num; i++)
    {
      j = to_merge -> list[i];
      k = to_merge -> list_id[i];
      for (l=0; l< tmp_fmol -> multi; l++)
      {
        if (tmp_proj -> atoms[0][j].coord[2] == tmp_fmol -> fragments[l]) break;
      }
      tmp_fmol -> atoms_id[k][l].a = to_merge -> id;
      tmp_fmol -> atoms_id[k][l].b = i;
      tmp_proj -> atoms[0][j].faid = to_merge -> id;
    }
  }
}

G_MODULE_EXPORT void run_remove_atom_from_field_molecule (GtkDialog * rmol, gint response_id, gpointer data)
{
  int i, j, k;
  gboolean done = FALSE;
  gchar * str;
  switch (response_id)
  {
    case GTK_RESPONSE_APPLY:
      if (a_ato)
      {
        i = gtk_combo_box_get_active(GTK_COMBO_BOX(combo_mol[0]));
        struct field_atom * to_merge = get_active_atom(i, new_at[0]);
        str = g_strdup_printf ("Merging with atom N°%d - %s !\nIs this correct ?",
                               new_at[0]+1, to_merge -> name);
        selection_confirmed = FALSE;
        field_question (str, G_CALLBACK(confirm_selection), NULL);
        g_free (str);
        if (selection_confirmed)
        {
          done = TRUE;
          merging_atoms (to_merge, at_to_remove, TRUE);
          struct field_struct * struct_saved[8];
          for (k=0; k<8; k++)
          {
            struct_saved[k] = duplicate_field_struct_list (tmp_fmol -> first_struct[k], FALSE);
          }
          // Modify the atom list
          j = at_to_remove -> id;
          if (at_to_remove -> prev == NULL)
          {
            tmp_fmol -> first_atom = at_to_remove -> next;
            tmp_fmol -> first_atom -> prev = NULL;
          }
          else if (at_to_remove -> next == NULL)
          {
            tmp_fct = at_to_remove -> prev;
            tmp_fct -> next = NULL;
          }
          else
          {
            tmp_fct -> next = at_to_remove -> next;
            at_to_remove -> next -> prev = tmp_fct;
          }
           // Finally clearing atom id
          tmp_fct = tmp_fmol -> first_atom;
          for (k=0; k<tmp_fmol -> atoms; k++)
          {
            tmp_fct -> id = k;
            if (tmp_fct -> next != NULL) tmp_fct = tmp_fct -> next;
          }
          tmp_fmol -> atoms --;
          init_all_field_struct (FALSE);
          // Cross check saved vs. new struct
          for (k=0; k<8; k++)
          {
            adjust_field_struct (j, k, struct_saved[k]);
            clean_field_struct_list (struct_saved[k]);
          }
          // Non-bonded interactions:
          compare_non_bonded (to_merge -> name);
        }
      }
      break;
    default:
      done = TRUE;
      break;
  }
  if (done) destroy_this_dialog (rmol);
}

G_MODULE_EXPORT void remove_atom_from_field_molecule (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  at_to_remove = (struct field_atom *) data;
  int i, j;
  field_object = 1;
  gchar * str = g_strdup_printf ("Select the field atom to merge atom \"%s\" with", at_to_remove -> name);
  GtkWidget * rmol = dialogmodal (str, GTK_WINDOW(field_assistant));
  g_free (str);
  gtk_dialog_add_button (GTK_DIALOG(rmol), "Apply", GTK_RESPONSE_APPLY);

  GtkWidget * remove_tree =  NULL;
  GtkTreeIter iter;

  gchar * mol_title[4] = {"Id", "Name", "Number", "Viz.3D & Merge with"};
  gchar * ctype[4] = {"text", "text", "text", "active"};
  GType col_type[4] = {G_TYPE_INT, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN};

  a_ato = 0;
  new_at = allocint(1);
  GtkTreeStore * remove_model = gtk_tree_store_newv (4, col_type);
  remove_tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(remove_model));
  num_field_objects = tmp_fmol -> atoms;
  for (i=0; i<4; i++)
  {
    if (i < 3)
    {
      remove_renderer[i] = gtk_cell_renderer_text_new ();
    }
    else
    {
      remove_renderer[i] = gtk_cell_renderer_toggle_new ();
      gtk_cell_renderer_toggle_set_radio (GTK_CELL_RENDERER_TOGGLE(remove_renderer[i]), TRUE);
      g_signal_connect (G_OBJECT(remove_renderer[i]), "toggled", G_CALLBACK(select_field_atom), & remove_model);
    }
    remove_col[i] = gtk_tree_view_column_new_with_attributes (mol_title[i], remove_renderer[i], ctype[i], i, NULL);
    gtk_tree_view_append_column (GTK_TREE_VIEW(remove_tree), remove_col[i]);
    if (i < 3)
    {
      gtk_tree_view_column_set_cell_data_func (remove_col[i], remove_renderer[i], atom_set_color_and_markup, NULL, NULL);
    }
  }

  // Clean 3D viz
  field_unselect_all ();
  // fill model
  j = 0;
  tmp_fat = tmp_fmol -> first_atom;
  for (i=0; i<tmp_fmol -> atoms; i++)
  {
    if (tmp_fat -> sp == at_to_remove -> sp && tmp_fat -> id != at_to_remove -> id)
    {
      j ++;
      gtk_tree_store_append (remove_model, & iter, NULL);
      gtk_tree_store_set (remove_model, & iter, 0, tmp_fat -> id+1,
                                                1, tmp_fat -> name,
                                                2, tmp_fat -> num,
                                                3, 0, -1);
    }
    if (tmp_fat -> next != NULL) tmp_fat = tmp_fat -> next;
  }
  g_object_unref (remove_model);
  gtk_tree_view_expand_all (GTK_TREE_VIEW(remove_tree));

  i = ((j+1)*40 < 500) ? (j+1)*40 : 500;
  GtkWidget * scrollsets = create_scroll (dialog_get_content_area (rmol), 375, i, GTK_SHADOW_ETCHED_IN);
  add_container_child (CONTAINER_SCR, scrollsets, remove_tree);
  remove_label = markup_label(remove_text(-2, 0, at_to_remove -> name), -1, -1, 0.5, 0.5);
  gtk_label_set_use_markup (GTK_LABEL(remove_label), TRUE);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, dialog_get_content_area (rmol), remove_label, FALSE, FALSE, 0);
  run_this_gtk_dialog (rmol, G_CALLBACK(run_remove_atom_from_field_molecule), NULL);
  field_unselect_all ();
  update_field_trees ();
}
