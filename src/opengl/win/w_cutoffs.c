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
* This file: 'w_cutoffs.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  void update_cutoffs (struct project * this_proj);
  void cut_box (struct project * this_proj, GtkWidget * vbox);

  G_MODULE_EXPORT void set_cut (GtkEntry * res, gpointer data);
  G_MODULE_EXPORT void run_window_cuts (GtkDialog * win, gint response_id, gpointer data);
  G_MODULE_EXPORT void window_cuts (GSimpleAction * action, GVariant * parameter, gpointer data);
  G_MODULE_EXPORT void window_cuts (GtkWidget * widg, gpointer data);

*/

#include "global.h"
#include "interface.h"
#include "project.h"
#include "glview.h"
#include "glwindow.h"

extern gchar * label_cutrab (struct project * this_proj, glwin * view, int id);
double * tmpcut;

/*
*  void update_cutoffs (struct project * this_proj)
*
*  Usage:
*
*  struct project * this_proj : the target project
*/
void update_cutoffs (struct project * this_proj)
{
  int i, j, k;
  k = 0;
  for (i=0; i<this_proj -> nspec; i++, k++)
  {
    if (this_proj -> chemistry -> cutoffs[i][i] != tmpcut[k] && tmpcut[k] >= 0.0)
    {
      this_proj -> chemistry -> cutoffs[i][i] = tmpcut[k];
      this_proj -> dmtx = FALSE;
    }
  }
  for (i=0; i<this_proj -> nspec-1; i++)
  {
    for (j=i+1; j<this_proj -> nspec; j++, k++)
    {
      if (this_proj -> chemistry -> cutoffs[i][j] != tmpcut[k] && tmpcut[k] >= 0.0)
      {
        this_proj -> chemistry -> cutoffs[i][j] = this_proj -> chemistry -> cutoffs[j][i] = tmpcut[k];
        this_proj -> dmtx = FALSE;
      }
    }
  }
  if (this_proj -> chemistry -> grtotcutoff != tmpcut[k] && tmpcut[k] >= 0.0)
  {
    this_proj -> chemistry -> grtotcutoff = tmpcut[k];
    this_proj -> dmtx = FALSE;
  }
  if (! this_proj -> dmtx && this_proj -> initgl)
  {
    if (this_proj -> modelgl -> rings)
    {
      this_proj -> modelgl -> rings = FALSE;
      for (j=0; j<5; j++)
      {
        clean_rings_data (j, this_proj -> modelgl);
#ifdef GTK3
        update_rings_menus (this_proj -> modelgl);
#endif
      }
    }
    if (this_proj -> modelgl -> chains)
    {
      clean_chains_data (this_proj -> modelgl);
#ifdef GTK3
      update_chains_menus (this_proj -> modelgl);
#endif
    }
    bonds_update = 1;
    frag_update = (this_proj -> natomes > ATOM_LIMIT) ? 0 : 1;
    mol_update = (frag_update) ? ((this_proj -> steps > STEP_LIMIT) ? 0 : 1) : 0;
    this_proj -> runc[0] = FALSE;
    if (this_proj -> id != activep)
    {
      k = activep;
      active_project_changed (this_proj -> id);
      on_calc_bonds_released (NULL, NULL);
      active_project_changed (k);
    }
    else
    {
      update_project (this_proj -> id);
      on_calc_bonds_released (NULL, NULL);
    }
  }
}

/*
*  G_MODULE_EXPORT void set_cut (GtkEntry * res, gpointer data)
*
*  Usage:
*
*  GtkEntry * res : the GtkEntry sending the signal
*  gpointer data  : the associated data pointer
*/
G_MODULE_EXPORT void set_cut (GtkEntry * res, gpointer data)
{
  const gchar * m;
  int id = GPOINTER_TO_INT(data);
  m = entry_get_text (res);
  double v = atof(m);
  if (v >= 0.0)
  {
    tmpcut[id] = (v < 0.5) ? 0.5 : v;
  }
  update_entry_double (res, tmpcut[id]);
}

/*
*  void cut_box (struct project * this_proj, GtkWidget * vbox)
*
*  Usage:
*
*  struct project * this_proj : the target project
*  GtkWidget * vbox           : the GtkWidget sending the signal
*/
void cut_box (struct project * this_proj, GtkWidget * vbox)
{
  int i, j, k;
  GtkWidget * vbo, * hbo, * cut;
  gchar * str;

  i = 1 + this_proj -> nspec * (this_proj -> nspec + 1) / 2;
  j = (this_proj -> nspec > 4) ? 500 : i * 37;
  GtkWidget * cutscroll = create_scroll (vbox, 350, j, GTK_SHADOW_NONE);
  vbo = create_vbox (BSEP);
  tmpcut = allocdouble (i);
  k = 0;
  for (i=0; i<this_proj -> nspec; i++, k++)
  {
    hbo = create_hbox (0);
    gtk_widget_set_size_request (hbo, 250, -1);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbo, hbo, FALSE, FALSE, 1);
    str = g_strdup_printf ("%s - %s", exact_name(this_proj -> chemistry -> label[i]), exact_name(this_proj -> chemistry -> label[i]));
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, markup_label(str, 100, -1, 0.0, 0.5), FALSE, FALSE, 5);
    g_free (str);
    cut = create_entry (G_CALLBACK(set_cut), 120, 15, FALSE, GINT_TO_POINTER(k));
    update_entry_double (GTK_ENTRY(cut), this_proj -> chemistry -> cutoffs[i][i]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, cut, FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, markup_label("&#xC5;", 30, -1, 0.0, 0.5), FALSE, FALSE, 5);
    tmpcut[k] = this_proj -> chemistry -> cutoffs[i][i];
  }
  for (i=0; i<this_proj -> nspec-1; i++)
  {
    for (j=i+1; j<this_proj -> nspec; j++, k++)
    {
      hbo = create_hbox (0);
      gtk_widget_set_size_request (hbo, 250, -1);
      add_box_child_start (GTK_ORIENTATION_VERTICAL, vbo, hbo, FALSE, FALSE, 1);
      str = g_strdup_printf ("%s - %s", exact_name(this_proj -> chemistry -> label[i]), exact_name(this_proj -> chemistry -> label[j]));
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, markup_label(str, 100, -1, 0.0, 0.5), FALSE, FALSE, 5);
      g_free (str);
      cut = create_entry (G_CALLBACK(set_cut), 120, 15, FALSE, (gpointer)GINT_TO_POINTER(k));
      update_entry_double (GTK_ENTRY(cut), this_proj -> chemistry -> cutoffs[i][j]);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, cut, FALSE, FALSE, 0);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, markup_label("&#xC5;", 30, -1, 0.0, 0.5), FALSE, FALSE, 5);
      tmpcut[k] = this_proj -> chemistry -> cutoffs[i][j];
    }
  }
  hbo = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbo, hbo, FALSE, FALSE, 1);
  gtk_widget_set_size_request (hbo, 250, -1);
  str = g_strdup_printf ("Total");
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, markup_label(str, 100, -1, 0.0, 0.5), FALSE, FALSE, 5);
  g_free (str);
  cut = create_entry (G_CALLBACK(set_cut), 120, 15, FALSE, (gpointer)GINT_TO_POINTER(k));
  update_entry_double (GTK_ENTRY(cut), this_proj -> chemistry -> grtotcutoff);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, cut, FALSE, FALSE, 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbo, markup_label("&#xC5;", 30, -1, 0.0, 0.5), FALSE, FALSE, 5);
  tmpcut[k] = this_proj -> chemistry -> grtotcutoff;
  add_container_child (CONTAINER_SCR, cutscroll, vbo);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, markup_label("<b>Cutoff(s) must be &#8805; 0.5 &#xC5;</b>", -1, -1, 0.5, 0.5), FALSE, FALSE, 10);
}

/*
*  G_MODULE_EXPORT void run_window_cuts (GtkDialog * win, gint response_id, gpointer data)
*
*  Usage:
*
*  GtkDialog * win  : the GtkDialog sending the signal
*  gint response_id :
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void run_window_cuts (GtkDialog * win, gint response_id, gpointer data)
{
  if (response_id == GTK_RESPONSE_APPLY)
  {
    gboolean upc = FALSE;
    if (opengl_project -> modelgl -> rings || opengl_project -> modelgl -> chains)
    {
      upc = ask_yes_no ("Data can be lost !", "You will lose\n rings statistics and/or chains statistics data\nProceed anyway ?", GTK_MESSAGE_WARNING, GTK_WIDGET(win));
    }
    else
    {
      upc = TRUE;
    }
    if (upc)
    {
      bonds_update = 1;
      update_cutoffs (opengl_project);
#ifdef GTK3
      gtk_menu_item_set_label (GTK_MENU_ITEM(opengl_project -> modelgl -> ogl_bonds[7]), label_cutrab (opengl_project, opengl_project -> modelgl, 0));
#endif
    }
  }
  destroy_this_dialog (win);
  if (tmpcut)
  {
    g_free (tmpcut);
    tmpcut = NULL;
  }
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT void window_cuts (GSimpleAction * action, GVariant * parameter, gpointer data)
*
*  Usage:
*
*  GSimpleAction * action : the GAction sending the signal
*  GVariant * parameter   : GVariant parameter of the GAction
*  gpointer data          : the associated data pointer
*/
G_MODULE_EXPORT void window_cuts (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
/*
*  G_MODULE_EXPORT void window_cuts (GtkWidget * widg, gpointer data)
*
*  Usage:
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void window_cuts (GtkWidget * widg, gpointer data)
#endif
{
  tint * the_data = (tint *)data;
  int p = the_data -> a;
  opengl_project_changed (p);
  gchar * str = g_strdup_printf ("Adjust cutoff radius(ii) - %s", prepare_for_title(opengl_project -> name));
  GtkWidget * win = dialog_cancel_apply (str, opengl_project -> modelgl -> win, FALSE);
  g_free (str);
  cut_box (opengl_project, dialog_get_content_area (win));
  run_this_gtk_dialog (win, G_CALLBACK(run_window_cuts), NULL);
}
