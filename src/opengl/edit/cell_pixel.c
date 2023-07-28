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
* This file: 'cell_pixel.c'
*
*  Contains:
*
*
*
*
*  List of subroutines:

  void pix_info_ (int * na, int * nb, int * nc);
  void send_pix_info_ (int * p, int listp[27], int * ngb);
  void update_pix_table (struct project * this_proj);

  G_MODULE_EXPORT void set_pix (GtkEntry * res, gpointer data);

  GtkWidget * create_css_label (gchar * str, int id);
  GtkWidget * attach_grid (struct project * this_proj, int init);
  GtkWidget * pixels_tab (struct project * this_proj);

*/

#ifdef DEBUG

#include "cell_edit.h"

/*
*  void pix_info_ (int * na, int * nb, int * nc)
*
*  Usage:
*
*  int * na :
*  int * nb :
*  int * nc :
*/
void pix_info_ (int * na, int * nb, int * nc)
{
  active_project -> pix[0] = *na;
  active_project -> pix[1] = *nb;
  active_project -> pix[2] = *nc;
  active_project -> pixels = allocdint (active_project -> pix[0]*active_project -> pix[1]*active_project -> pix[2],27);
}

/*
*  void send_pix_info_ (int * p, int listp[27], int * ngb)
*
*  Usage:
*
*  int * p       :
*  int listp[27] :
*  int listp[27] :
*/
void send_pix_info_ (int * p, int listp[27], int * ngb)
{
  int i;
  for (i=0; i<* ngb; i++)
  {
    active_project -> pixels[* p][i] = listp[i];
  }
}

/*
*  GtkWidget * create_css_label (gchar * str, int id)
*
*  Usage:
*
*  gchar * str :
*  int id      :
*/
GtkWidget * create_css_label (gchar * str, int id)
{
  gchar * colo[2] = {"white", "yellow"};
  gchar * backcol = g_strdup_printf ("label#color {\n"
                                     "  background-color: blue;\n"
                                     "  color: %s;\n"
                                     "}", colo[id]);
  provide_gtk_css (backcol);
  g_free (backcol);
  GtkWidget * lab = markup_label(str, 50, 50, 0.5, 0.5);
  gtk_widget_set_name (lab, "color");
  gtk_widget_show (lab);
  return lab;
}

/*
*  GtkWidget * attach_grid (struct project * this_proj, int init)
*
*  Usage:
*
*  struct project * this_proj : the target project
*  int init                   :
*/
GtkWidget * attach_grid (struct project * this_proj, int init)
{
  GtkWidget * table = gtk_grid_new ();
  gchar * str;
  gboolean actif;
  int i, j, k, l, m;
  l = init+1;
  for (i=0; i<this_proj -> pix[1]; i++)
  {
    for (j=0; j<this_proj -> pix[0]; j++)
    {
      actif = FALSE;
      for (k=0; k<27; k++)
      {
        if (l == this_proj -> pixels[this_proj -> actif_pix-1][k])
        {
          actif = TRUE;
        }
      }
      if (l == this_proj -> actif_pix)
      {
        str = g_strdup_printf ("<b>%d</b>", l);
        m = 1;
      }
      else
      {
        str = g_strdup_printf ("%d", l);
        m = 0;
      }
      l ++;
      if (actif)
      {
        gtk_grid_attach (GTK_GRID (table), create_css_label(str, m), j, i, 1, 1);
      }
      else
      {
        gtk_grid_attach (GTK_GRID (table), markup_label(str, 50, 50, 0.5, 0.5), j, i, 1, 1);
      }
      g_free (str);
    }
  }
  gtk_widget_set_size_request (table, this_proj -> pix[0]*50, this_proj -> pix[1]*50);
  return table;
}

/*
*  void update_pix_table (struct project * this_proj)
*
*  Usage:
*
*  struct project * this_proj : the target project
*/
void update_pix_table (struct project * this_proj)
{
  int i;
  int pix[3];

  for (i=0; i<3; i++)
  {
    this_proj -> pix_tab[i] = destroy_this_widget (this_proj -> pix_tab[i]);
  }

  // Seed for bottom grid
  if (this_proj -> actif_pix <= this_proj -> pix[0]*this_proj -> pix[1])
  {
    pix[0] = this_proj -> pix[0]*this_proj -> pix[1]*(this_proj -> pix[2]-1);
  }
  else
  {
    pix[0] = (this_proj -> actif_pix-1)/(this_proj -> pix[0]*this_proj -> pix[1]);
    pix[0] -= 1;
    pix[0] *= (this_proj -> pix[0]*this_proj -> pix[1]);
  }

  // Seed for pixel grid
  pix[1] = (this_proj -> actif_pix-1)/(this_proj -> pix[0]*this_proj -> pix[1]);
  pix[1] *= (this_proj -> pix[0]*this_proj -> pix[1]);

  // Seed for top grid
  if (this_proj -> actif_pix >= this_proj -> pix[0]*this_proj -> pix[1]*(this_proj -> pix[2]-1))
  {
    pix[2] = 0;
  }
  else
  {
    pix[2] = (this_proj -> actif_pix-1)/(this_proj -> pix[0]*this_proj -> pix[1]);
    pix[2] += 1;
    pix[2] *= (this_proj -> pix[0]*this_proj -> pix[1]);
  }
  for (i=0; i<3; i++)
  {
    this_proj -> pix_tab[i] = attach_grid (this_proj, pix[i]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, this_proj -> pix_box, this_proj -> pix_tab[i], FALSE, FALSE, 20);
  }
  show_the_widgets (this_proj -> pix_box);
}

/*
*  G_MODULE_EXPORT void set_pix (GtkEntry * res, gpointer data)
*
*  Usage:
*
*  GtkEntry * res : the GtkEntry sending the signal
*  gpointer data  : the associated data pointer
*/
G_MODULE_EXPORT void set_pix (GtkEntry * res, gpointer data)
{
  int id = GPOINTER_TO_INT (data);
  struct project * this_proj = get_project_by_id (id);
  const gchar * m = entry_get_text (res);
  double v = atof(m);
  int p = (int)v;
  int n = this_proj-> pix[0] * this_proj -> pix[1] * this_proj -> pix[2];
  if (p > 0 && p <= n)
  {
    this_proj -> actif_pix = p;
  }
  update_entry_int (res, this_proj -> actif_pix);
  update_pix_table (this_proj);
}

/*
*  GtkWidget * pixels_tab (struct project * this_proj)
*
*  Usage:
*
*  struct project * this_proj : the target project
*/
GtkWidget * pixels_tab (struct project * this_proj)
{
  GtkWidget * layout = create_layout (700, 750);
  GtkWidget * vbox = add_vbox_to_layout (layout, -1, -1);
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 5);
  gchar * str = g_strdup_printf ("\tSelect pixel to check, from 1 to %d:", this_proj -> pix[0]*this_proj -> pix[1]*this_proj->pix[2]);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label(str, -1, -1, 0.0, 0.5), FALSE, FALSE, 10);
  g_free (str);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, create_entry(G_CALLBACK(set_pix), 100, 15, FALSE, GINT_TO_POINTER(this_proj -> id)), FALSE, FALSE, 10);
  this_proj -> actif_pix = 1;
  this_proj -> pix_box = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, this_proj -> pix_box, FALSE, FALSE, 5);
  this_proj -> pix_tab[0] = this_proj -> pix_tab[1] = this_proj -> pix_tab[2] = NULL;
  update_pix_table (this_proj);
  return layout;
}

#endif // DEBUG
