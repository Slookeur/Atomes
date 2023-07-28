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
* This file: 'close_p.c'
*
*  Contains:
*

 - The subroutines to close a project
 - the callbacks to close a project

*
*  List of subroutines:

  void update_insert_combos ();
  void close_project (struct project * to_close);

  void to_close_this_project (int to_activate, struct project * this_proj);
  G_MODULE_EXPORT void on_close_activate (GtkWidget * widg, gpointer cdata);

*/

#include "global.h"
#include "bind.h"
#include "callbacks.h"
#include "interface.h"
#include "project.h"
#include "workspace.h"
#include "curve.h"
#include "glview.h"

extern GtkTreeStore * tool_model;
extern GtkTreeModel * replace_combo_tree (gboolean insert, int p);

/*
*  void update_insert_combos ()
*
*  Usage: update some GtkComboBox in the workspace if a project is removed
*/
void update_insert_combos ()
{
  GtkTreeModel * model;
  struct project * this_proj;
  GList * cell_list;
  GtkWidget * box;
  int i;
  for (i=0; i<nprojects; i++)
  {
    this_proj = get_project_by_id(i);
    if (this_proj -> modelgl)
    {
      if ((this_proj -> modelgl -> atom_win && this_proj -> modelgl -> atom_win -> visible) || this_proj -> modelgl -> builder_win)
      {
        model = replace_combo_tree (TRUE, this_proj -> id);
        box = (this_proj -> modelgl -> builder_win) ? this_proj -> modelgl -> builder_win -> add_combo : this_proj -> modelgl -> atom_win -> atom_combo[3];
        gtk_combo_box_set_model (GTK_COMBO_BOX(box), model);
        gtk_combo_box_set_active (GTK_COMBO_BOX(box), 0);
        cell_list = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(box));
        if (cell_list && cell_list -> data)
        {
          gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(box), cell_list -> data, "markup", 0, NULL);
        }
        g_object_unref (model);
      }
    }
  }
}

/*
*  void close_project (struct project * to_close)
*
*  Usage: close a project
*
*  struct project * to_close : the project to close
*/
void close_project (struct project * to_close)
{
  int i, j, k, l;

#ifdef DEBUG
  g_debug ("CLOSE_PROJECT: proj to close= %d", to_close -> id);
  g_debug ("CLOSE_PROJECT: nprojects    = %d", nprojects);
  g_debug ("CLOSE_PROJECT: activep      = %d", activep);
#endif

  if (to_close -> initgl)
  {
    if (to_close -> modelgl -> measure_win)
    {
      to_close -> modelgl -> measure_win -> win = destroy_this_widget (to_close -> modelgl -> measure_win -> win);
      g_free (to_close -> modelgl -> measure_win);
    }
    if (to_close -> modelgl -> volume_win)
    {
      to_close -> modelgl -> volume_win -> win = destroy_this_widget (to_close -> modelgl -> volume_win -> win);
      g_free (to_close -> modelgl -> volume_win);
    }
    if (to_close -> modelgl -> player)
    {
      to_close -> modelgl -> player -> win = destroy_this_widget (to_close -> modelgl -> player -> win);
      g_free (to_close -> modelgl -> player);
    }
    if (to_close -> modelgl -> spiner)
    {
      to_close -> modelgl -> spiner -> win = destroy_this_widget (to_close -> modelgl -> spiner -> win);
      g_free (to_close -> modelgl -> spiner);
    }
    if (to_close -> modelgl -> rec)
    {
      to_close -> modelgl -> rec -> win = destroy_this_widget (to_close -> modelgl -> rec -> win);
      g_free (to_close -> modelgl -> rec);
    }
    if (to_close -> modelgl -> atom_win)
    {
      to_close -> modelgl -> atom_win -> win = destroy_this_widget (to_close -> modelgl -> atom_win -> win);
      g_free (to_close -> modelgl -> atom_win);
    }
    if (to_close -> modelgl -> cell_win)
    {
      to_close -> modelgl -> cell_win -> win = destroy_this_widget (to_close -> modelgl -> cell_win -> win);
      g_free (to_close -> modelgl -> cell_win);
    }
    if (to_close -> modelgl -> builder_win)
    {
      to_close -> modelgl -> builder_win -> win = destroy_this_widget (to_close -> modelgl -> builder_win -> win);
      g_free (to_close -> modelgl -> builder_win);
    }
    if (to_close -> modelgl -> coord_win)
    {
      to_close -> modelgl -> coord_win -> win = destroy_this_widget (to_close -> modelgl -> coord_win -> win);
      g_free (to_close -> modelgl -> coord_win);
    }
    for (i=0; i<2; i++)
    {
     if (to_close -> modelgl -> model_win[i])
      {
        to_close -> modelgl -> model_win[i] -> win = destroy_this_widget (to_close -> modelgl -> model_win[i] -> win);
        g_free (to_close -> modelgl -> model_win[i]);
      }
    }
    if (to_close -> modelgl -> opengl_win)
    {
      to_close -> modelgl -> opengl_win -> win = destroy_this_widget (to_close -> modelgl -> opengl_win -> win);
      g_free (to_close -> modelgl -> opengl_win);
    }
    to_close -> modelgl -> win = destroy_this_widget (to_close -> modelgl -> win);
    for (i=0; i<NGLOBAL_SHADERS; i++) cleaning_shaders (to_close -> modelgl, i);
    g_free (to_close -> modelgl);
    if (to_close -> modelfc)
    {
      for (i=0; i< to_close -> steps; i++)
      {
        g_free (to_close -> modelfc -> mols[i]);
      }
    }
  }
  if (to_close -> run)
  {
    for (i=0 ; i<NGRAPHS ; i++)
    {
      to_close -> visok[i]=FALSE;
      if (to_close -> curves[i])
      {
        hide_curves (to_close, i);
        erase_curves (to_close, i);
      }
    }
  }
  clean_view ();
  if (nprojects == 1)
  {
    prep_calc_actions ();
    workzone.first = NULL;
    workzone.last = NULL;
    activep = -1;
    correct_this_window_title (MainWindow, g_strdup_printf ("%s", PACKAGE));
    correct_this_window_title (curvetoolbox, g_strdup_printf ("Toolboxes"));
    if (workspacefile != NULL)
    {
      g_free (workspacefile);
      workspacefile = NULL;
    }
    newspace = TRUE;
  }
  else if (nprojects > 1)
  {
    if (to_close == workzone.first)
    {
      workzone.first = workzone.first -> next;
      workzone.first -> prev = NULL;
    }
    else if (to_close == workzone.last)
    {
      workzone.last = workzone.last -> prev;
      workzone.last -> next = NULL;
    }
    else
    {
      to_close -> prev -> next = to_close -> next;
      to_close -> next -> prev = to_close -> prev;
    }
    g_free (to_close);
  }
  nprojects --;
  if (nprojects)
  {
    struct project * this_proj = workzone.first;
    for (i=0 ; i<nprojects ; i++)
    {
      this_proj -> id = i;
      if (this_proj -> initgl)
      {
        this_proj -> modelgl -> proj = i;
        for (j=0; j<NUM_COLORS; j++)
        {
          for (k=0; k<this_proj -> nspec*2; k++)
          {
            this_proj -> modelgl -> colorp[j][k].a = i;
          }
        }
        for (j=0; j<10; j++)
        {
          if (this_proj -> modelgl -> gcid[j] != NULL)
          {
            for (k=0; k<this_proj -> coord -> totcoord[j]; k++)
            {
              for (l=0; l<NUM_COLORS; l++)
              {
                this_proj -> modelgl -> gcid[j][k][l].a = i;
              }
            }
          }
        }
        if (this_proj -> modelgl -> atom_win)
        {
          for (j=0; j<8; j++)
          {
            if (this_proj -> modelgl -> search_widg[j])
            {
              for (k=0; k<3; k++) this_proj -> modelgl -> search_widg[j] -> pointer[k].a = i;
            }
          }
        }
      }
      for (j=0; j<NGRAPHS; j++)
      {
        if (this_proj -> idcc[j] != NULL)
        {
          for (k=0; k<this_proj -> numc[j]; k++)
          {
            this_proj -> idcc[j][k].a = i;
          }
        }
      }
      if (this_proj -> next != NULL) this_proj = this_proj -> next;
    }
    this_proj = workzone.first;
    for (i=0 ; i<nprojects ; i++)
    {
      this_proj -> id = i;
      for (j=0; j<NGRAPHS; j++)
      {
        for (k=0; k<this_proj -> numc[j]; k++)
        {
          if (this_proj -> curves[j][k] -> window)
          {
            curve_window_add_menu_bar (&  this_proj -> idcc[j][k]);
            if (is_the_widget_visible(this_proj -> curves[j][k] -> plot))
            {
              gtk_widget_queue_draw (this_proj -> curves[j][k] -> plot);
            }
          }
        }
      }
      if (this_proj -> next != NULL) this_proj = this_proj -> next;
    }
  }
  update_insert_combos ();
}

/*
*  void to_close_this_project (int to_activate, struct project * this_proj)
*
*  Usage: to close this project
*
*  int to_activate            : If the workspace is not empty, activate first another project
*  struct project * this_proj : the target project
*/
void to_close_this_project (int to_activate, struct project * this_proj)
{
  if (nprojects > 0) close_project (this_proj);
  if (nprojects > 0)
  {
    activate_project (NULL, GINT_TO_POINTER(to_activate));
  }
  else
  {
    remove_edition_and_analyze_actions ();
    active_project = NULL;
    fill_tool_model ();
  }
}

/*
*  G_MODULE_EXPORT void on_close_activate (GtkWidget * widg, gpointer cdata)
*
*  Usage: signal to close a project
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer cdata   : the associated data pointer
*/
G_MODULE_EXPORT void on_close_activate (GtkWidget * widg, gpointer cdata)
{
  if (nprojects > 0)
  {
    int i = GPOINTER_TO_INT(cdata);
    int j = activep;
    if (j >= i && j > 0) j--;
    gtk_tree_store_clear (tool_model);
    remove_project_from_workspace (i);
    close_project (get_project_by_id(i));
    if (nprojects > 0)
    {
      activate_project (NULL, GINT_TO_POINTER(j));
    }
    else
    {
      remove_edition_and_analyze_actions ();
      active_project = NULL;
      fill_tool_model ();
    }
  }
  else
  {
    show_warning ("No project to be closed", MainWindow);
  }
}
