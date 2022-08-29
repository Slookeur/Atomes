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

void add_bonds_to_project (struct project * this_proj, int removed, int nbd, int ** new_bond_list)
{
  int i, j;
  int ** tmpbondid = NULL;
  if (nbd)
  {
    i = this_proj -> modelgl -> bonds[0][0];
    tmpbondid = allocdint (i+nbd, 2);
    for (j=0; j<i; j++)
    {
      tmpbondid[j][0] = this_proj -> modelgl -> bondid[0][0][j][0];
      tmpbondid[j][1] = this_proj -> modelgl -> bondid[0][0][j][1];
    }
    if (this_proj -> modelgl -> allbonds[0]) g_free (this_proj -> modelgl -> bondid[0][0]);
    this_proj -> modelgl -> bondid[0][0] = allocdint (i+nbd, 2);
    for (j=0; j<i; j++)
    {
      this_proj -> modelgl -> bondid[0][0][j][0] = tmpbondid[j][0];
      this_proj -> modelgl -> bondid[0][0][j][1] = tmpbondid[j][1];
    }
    for (j=0; j<nbd; j++)
    {
      this_proj -> modelgl -> bondid[0][0][j+i][0] = new_bond_list[j][0] + this_proj -> natomes - removed;
      this_proj -> modelgl -> bondid[0][0][j+i][1] = new_bond_list[j][1] + this_proj -> natomes - removed;
    }
    this_proj -> modelgl -> bonds[0][0] += nbd;
    this_proj -> modelgl -> allbonds[0] += nbd;
    g_free (tmpbondid);
  }
}

void add_bonds_to_list (int ** new_bond_list, int nat, int nbd, struct insert_object * object)
{
  int i;
  if (object -> bonds)
  {
    for (i=0; i<object -> bonds; i++)
    {
      new_bond_list[i+nbd][0] = object -> ibonds[i][0] + nat;
      new_bond_list[i+nbd][1] = object -> ibonds[i][1] + nat;
    }
  }
}

void prepare_to_instert (gchar * key, struct project * this_proj, atom_search * asearch, gboolean visible)
{
  int i = get_selected_object_id (visible, this_proj -> id,  key, asearch);
  if (i == FROM_PROJECT || i == FROM_DATA || i > 0) to_insert_in_project (i, -1, this_proj, asearch, visible);
}

G_MODULE_EXPORT void set_atoms_to_insert (GtkComboBox * box, gpointer data)
{
  GValue val = {0, };
  GtkTreeModel * cmodel = gtk_combo_box_get_model (box);
  GtkTreeIter iter;
  if (gtk_combo_box_get_active_iter (box, & iter))
  {
    gtk_tree_model_get_value (cmodel, & iter, 0, & val);
    tint * dat = (tint *)data;
    gchar * str = g_strdup_printf ("%s", (char *)g_value_get_string (& val));
    struct project * this_proj = get_project_by_id (dat -> a);
    prepare_to_instert (str, this_proj, this_proj -> modelgl -> search_widg[dat -> c], TRUE);
    g_free (str);
  }
  gtk_combo_box_set_active (box, 0);
}

