/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include "cell_edit.h"
#include "atom_edit.h"

extern void clean_coord_window (struct project * this_proj);
extern GtkWidget * cell_tab (int i, struct project * this_proj);

gboolean ** duplicate_geom_info (struct project * this_proj)
{
  int i, j;
  gboolean ** show = g_malloc (2*sizeof*show);
  for (i=0; i<2; i++)
  {
    show[i] = allocbool(this_proj -> coord -> totcoord[i]);
    for (j=0; j<this_proj -> coord -> totcoord[i]; j++)
    {
      show[i][j] = this_proj -> modelgl -> anim -> last -> img -> show_coord[i][j];
    }
  }
  return show;
}

gboolean ** duplicate_poly_info (struct project * this_proj)
{
  int i, j;
  gboolean ** show = g_malloc (2*sizeof*show);
  for (i=0; i<2; i++)
  {
    show[i] = allocbool(this_proj -> coord -> totcoord[i]);
    for (j=0; j<this_proj -> coord -> totcoord[i]; j++)
    {
      show[i][j] = this_proj -> modelgl -> anim -> last -> img -> show_poly[i][j];
    }
  }
  return show;
}

void restore_coord_and_poly_info (struct project * proj, gboolean ** cshow, gboolean ** pshow)
{
  int i, j;
  for (i=0; i<2; i++)
  {
    for (j=0; j<active_coord -> totcoord[i]; j++)
    {
#ifdef GTK3
      if (active_glwin -> ogl_geom[0][i][j] != NULL)
      {
        if (GTK_IS_WIDGET(active_glwin -> ogl_geom[0][i][j]))
        {
          check_menu_item_set_active ((gpointer)active_glwin -> ogl_geom[0][i][j], cshow[i][j]);
          if (cshow[i][j]) show_hide_coord (active_glwin -> ogl_geom[0][i][j], & active_glwin -> gcid[i][j][i]);
        }
      }
      if (active_glwin -> ogl_poly[0][i][j] != NULL)
      {
        if (GTK_IS_WIDGET(active_glwin -> ogl_poly[0][i][j]))
        {
          check_menu_item_set_active ((gpointer)active_glwin -> ogl_poly[0][i][j], pshow[i][j]);
          if (pshow[i][j]) show_hide_poly (active_glwin -> ogl_poly[0][i][j], & active_glwin -> gcid[i][j][i]);
        }
      }
#endif
    }
  }
}

void sens_superbut (struct project * this_proj)
{
  int i, j;
  if (this_proj -> modelgl -> record)
  {
    i = 0;
  }
  else
  {
    i = 0;
    for (j=0; j<3; j++) i += this_proj -> modelgl -> anim -> last -> img -> extra_cell[j];
  }
  if (this_proj -> modelgl -> cell_win)
  {
    widget_set_sensitive (this_proj -> modelgl -> cell_win -> superbut, i);
  }
#ifdef GTK3
  // GTK3 Menu Action To Check
  widget_set_sensitive (this_proj -> modelgl -> ogl_box[5], i);
#endif
}

void super_celling (glwin * view)
{
  gchar * txta = "You are about to change the periodicity of the 3D model,";
  gchar * txtb = "this will affect the entire molecular dynamics trajectory,";
  gchar * txtc = "\tand the action is irreversible, proceed anyway ?";
  gchar * str;
  if (get_project_by_id(view -> proj) -> steps > 1)
  {
    str = g_strdup_printf ("%s\n%s\n%s", txta, txtb, txtc);
  }
  else
  {
    str = g_strdup_printf ("%s\n%s", txta, txtc);
  }
  if (ask_yes_no ("Create a super-cell ?", str, GTK_MESSAGE_WARNING, view -> win))
  {
    int i, j, k, l;
    k = activep;
    image * last = view -> anim -> last -> img;
    if (k != view -> proj) active_project_changed (view -> proj);
    preserve_ogl_selection (view);
    if (add_cells_ (& active_project -> natomes, & active_project -> steps, last -> extra_cell))
    {
      if (active_cell -> crystal)
      {
        vec3_t shift;
        shift.x = active_box -> vect[0][0] + active_box -> vect[1][0] + active_box -> vect[2][0];
        shift.y = active_box -> vect[0][1] + active_box -> vect[1][1] + active_box -> vect[2][1];
        shift.z = active_box -> vect[0][2] + active_box -> vect[1][2] + active_box -> vect[2][2];
        for (i=0; i<active_project -> steps; i++)
        {
          for (j=0; j<active_project -> natomes; j++)
          {
            active_project -> atoms[i][j].x += shift.x;
            active_project -> atoms[i][j].y += shift.y;
            active_project -> atoms[i][j].z += shift.z;
          }
        }
      }
      l = 1;
      for (i=0; i<3; i++)
      {
        for (j=0; j<3; j++)
        {
          active_box -> vect[i][j] *= (last -> extra_cell[i] + 1);
        }
        l *= (last -> extra_cell[i] + 1);
        active_box -> param[0][i] *= (last -> extra_cell[i] + 1);
        last -> extra_cell[i] = 0;
        if (active_glwin -> cell_win)
        {
          if (active_glwin -> cell_win -> ax_cell[i])
          {
            gtk_spin_button_set_value (GTK_SPIN_BUTTON(active_glwin -> cell_win -> ax_cell[i]), 1.0);
          }
        }
      }
      if (active_cell -> crystal)
      {
        vec3_t shift;
        shift.x = active_box -> vect[0][0] + active_box -> vect[1][0] + active_box -> vect[2][0];
        shift.y = active_box -> vect[0][1] + active_box -> vect[1][1] + active_box -> vect[2][1];
        shift.z = active_box -> vect[0][2] + active_box -> vect[1][2] + active_box -> vect[2][2];
        for (i=0; i<active_project -> steps; i++)
        {
          for (j=0; j<active_project -> natomes; j++)
          {
            active_project -> atoms[i][j].x -= shift.x/2.0;
            active_project -> atoms[i][j].y -= shift.y/2.0;
            active_project -> atoms[i][j].z -= shift.z/2.0;
          }
        }
      }
      for (i=0; i<active_project -> nspec; i++)
      {
        active_chem -> nsps[i] *= l;
      }
      init_curves_and_calc (active_project);
      if (! active_project -> cell.crystal) center_molecule (active_project);

      active_project_changed (view -> proj);
      active_project -> dmtx = FALSE;
      bonds_update = 1;
      active_project -> runc[0] = FALSE;
      frag_update = (active_project -> natomes > ATOM_LIMIT) ? 0 : 1;
      mol_update = (frag_update) ? ((active_project -> steps > STEP_LIMIT) ? 0 : 1) : 0;
      gboolean ** cshow = duplicate_geom_info (active_project);
      gboolean ** pshow = duplicate_poly_info (active_project);
      if (view -> rings)
      {
        view -> rings = FALSE;
        for (i=0; i<5; i++)
        {
          clean_rings_data (i, view);
          update_rings_menus (view);
        }
      }
      if (view -> chains)
      {
        clean_chains_data (view);
        update_chains_menus (view);
      }
      on_calc_bonds_released (NULL, NULL);
      restore_coord_and_poly_info (active_project, cshow, pshow);
      g_free (cshow);
      g_free (pshow);
      int shaders[1] = {POLYS};
      re_create_md_shaders (1, shaders, active_project);
      view -> create_shaders[PICKS] = TRUE;
      view -> create_shaders[MDBOX] = TRUE;
      view -> create_shaders[LABEL] = TRUE;
      view -> create_shaders[MEASU] = TRUE;
    }
    else
    {
      show_warning ("Something went wrong, the 3D model was not updated", view -> win);
    }
    restore_ogl_selection (view);
    fill_tool_model ();
    clean_other_window_after_edit (active_project);
    if (k != view -> proj) active_project_changed (k);
  }
  update (view);
#ifdef GTK
  update_menu_bar (view);
#endif
}

#ifdef GTK4
G_MODULE_EXPORT void super_cell (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void super_cell (GtkWidget * widg, gpointer data)
#endif
{
  glwin * view = (glwin *)data;
  super_celling (view);
  sens_superbut (get_project_by_id(view -> proj));
}

G_MODULE_EXPORT void super_cell_but (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *)data;
  super_celling (view);
  sens_superbut (get_project_by_id(view -> proj));
}

