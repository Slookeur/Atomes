/* This file is part of the 'atomes' software

'atomes' is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

'atomes' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with 'atomes'.
If not, see <https://www.gnu.org/licenses/>

Copyright (C) 2022-2024 by CNRS and University of Strasbourg */

/*!
* @file read_cif.c
* @short Functions to read CIF files
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'read_cif.c'
*
* Contains:
*

 - The functions to read CIF files

*
* List of functions:

  int get_atom_wyckoff (gchar * line, int wid);
  int cif_get_value (gchar * kroot, gchar * keyw, int linec, int lstart, gchar ** cif_word, gboolean rec_val, gboolean all_ligne, gboolean in_loop);
  int cif_file_get_data_in_loop (int linec, int lid);
  int cif_file_get_number_of_atoms (int linec, int lid, int nelem);
  int get_loop_line_id (int linec, int lid);
  int get_loop_line_for_key (gchar * key_a, gchar * key_b, int linec);
  int cif_file_get_number_of_positions (int linec, int lid);
  int get_space_group_from_hm (gchar * hmk);
  int get_setting_from_hm (gchar * hmk, int end);
  int group_info_from_hm_key (int spg, gchar * key_hm);
  int cif_get_space_group (int linec);
  int open_cif_file (int linec);

  float get_atom_coord (gchar * line, int mid);

  gboolean get_missing_object_from_user ();
  gboolean cif_file_get_atoms_data (int lin, int cid[8]);
  gboolean cif_get_atomic_coordinates (int linec);
  gboolean cif_get_symmetry_positions (int linec);
  gboolean cif_get_cell_data (int linec);

  gchar * get_cif_word (gchar * mot);
  gchar * get_atom_label (gchar * line, int lid);
  gchar * get_string_from_origin (space_group * spg);

  G_MODULE_EXPORT void set_cif_to_insert (GtkComboBox * box, gpointer data);
  void file_get_to_line (int line_id);
  void check_for_to_lab (int ato, gchar * stlab);

*/

#include "global.h"
#include "bind.h"
#include "interface.h"
#include "project.h"
#include "atom_edit.h"
#include "cbuild_edit.h"
#include "readers.h"
#include <ctype.h>
#ifdef OPENMP
#  include <omp.h>
#endif

extern int get_atom_id_from_periodic_table (atom_search * asearch);
extern double get_z_from_periodic_table (gchar * lab);
extern void get_origin (space_group * spg);
extern void compute_lattice_properties (cell_info * cell);
extern int test_lattice (builder_edition * cbuilder, cell_info * cif_cell);
extern int read_space_group (builder_edition * cbuilder, int spg);
extern gchar * wnpos[3];
extern void get_wyck_char (float val, int ax, int bx);
extern space_group * duplicate_space_group (space_group * spg);
extern int build_crystal (gboolean visible, project * this_proj, gboolean to_wrap, gboolean show_clones, cell_info * cell, GtkWidget * widg);
extern distance distance_3d (cell_info * cell, int mdstep, atom * at, atom * bt);
extern void sort (int dim, int * tab);

extern gchar * tmp_pos;

FILE * cifp;
char * line_ptr;
int * keylines = NULL;
int cif_loop_id;

gchar * cif_coord_opts[40][2] = {{"b1", "Monoclinic unique axis b, cell choice 1, abc"},    // 0
                                 {"b2", "Monoclinic unique axis b, cell choice 2, abc"},    // 1
                                 {"b3", "Monoclinic unique axis b, cell choice 3, abc"},    // 2
                                 {"-b1", "Monoclinic unique axis b, cell choice 1, c-ba"},  // 3
                                 {"-b2", "Monoclinic unique axis b, cell choice 2, c-ba"},  // 4
                                 {"-b3", "Monoclinic unique axis b, cell choice 3, c-ba"},  // 5
                                 {"c1", "Monoclinic unique axis c, cell choice 1, abc"},    // 6
                                 {"c2", "Monoclinic unique axis c, cell choice 2, abc"},    // 7
                                 {"c3", "Monoclinic unique axis c, cell choice 3, abc"},    // 8
                                 {"-c1", "Monoclinic unique axis c, cell choice 1, ba-c"},  // 9
                                 {"-c2", "Monoclinic unique axis c, cell choice 2, ba-c"},  // 10
                                 {"-c3", "Monoclinic unique axis c, cell choice 3, ba-c"},  // 11
                                 {"a1", "Monoclinic unique axis a, cell choice 1, abc"},    // 12
                                 {"a2", "Monoclinic unique axis a, cell choice 2, abc"},    // 13
                                 {"a3", "Monoclinic unique axis a, cell choice 3, abc"},    // 14
                                 {"-a1", "Monoclinic unique axis a, cell choice 1, -acb"},  // 15
                                 {"-a2", "Monoclinic unique axis a, cell choice 2, -acb"},  // 16
                                 {"-a3", "Monoclinic unique axis a, cell choice 3, -acb"},  // 17
                                 {"abc", "Orthorhombic"},                                   // 18
                                 {"ba-c", "Orthorhombic"},                                  // 10
                                 {"cab", "Orthorhombic"},                                   // 20
                                 {"-cba", "Orthorhombic"},                                  // 21
                                 {"bca", "Orthorhombic"},                                   // 22
                                 {"a-cb", "Orthorhombic"},                                  // 23
                                 {"1abc", "Orthorhombic origin choice 1"},                  // 24
                                 {"1ba-c", "Orthorhombic origin choice 1"},                 // 25
                                 {"1cab", "Orthorhombic origin choice 1"},                  // 26
                                 {"1-cba", "Orthorhombic origin choice 1"},                 // 27
                                 {"1bca", "Orthorhombic origin choice 1"},                  // 28
                                 {"1a-cb", "rthorhombic origin choice 1"},                  // 29
                                 {"2abc", "Orthorhombic origin choice 2"},                  // 30
                                 {"2ba-c", "Orthorhombic origin choice 2"},                 // 31
                                 {"2cab", "Orthorhombic origin choice 2"},                  // 32
                                 {"2-cba", "Orthorhombic origin choice 2"},                 // 33
                                 {"2bca", "Orthorhombic origin choice 2"},                  // 34
                                 {"2a-cb", "Orthorhombic origin choice 2"},                 // 35
                                 {"1", "Tetragonal or cubic origin choice 1"},              // 36
                                 {"2", "Tetragonal or cubic origin choice 2"},              // 37
                                 {"h", "Trigonal using hexagonal axes"},                    // 38
                                 {"r", "Trigonal using rhombohedral axes "}};               // 39

#ifdef G_OS_WIN32
  typedef intptr_t ssize_t;

  ssize_t getline(char **lineptr, size_t *n, FILE *stream)
  {
    size_t pos;
    int c;

    if (lineptr == NULL || stream == NULL || n == NULL)
    {
      errno = EINVAL;
      return -1;
    }

    c = getc(stream);
    if (c == EOF) return -1;

    if (*lineptr == NULL)
    {
      *lineptr = malloc(128);
      if (*lineptr == NULL) return -1;
      *n = 128;
    }

    pos = 0;
    while(c != EOF)
    {
      if (pos + 1 >= *n)
      {
        size_t new_size = *n + (*n >> 2);
        if (new_size < 128)
        {
          new_size = 128;
        }
        char *new_ptr = realloc(*lineptr, new_size);
        if (new_ptr == NULL) return -1;
        *n = new_size;
        *lineptr = new_ptr;
      }

      ((unsigned char *)(*lineptr))[pos ++] = c;
      if (c == '\n') break;
      c = getc(stream);
    }

    (*lineptr)[pos] = '\0';
    return pos;
  }
#endif // G_OS_WIN_32

/*!
  \fn gchar * get_cif_word (gchar * mot)

  \brief get string from CIF file, EOL can be ugly

  \param mot the string that was read in the file
*/
gchar * get_cif_word (gchar * mot)
{
  gchar * word = substitute_string (mot, "\n", NULL);
  word = substitute_string (word, "\r", NULL);
  return word;
}

/*!
  \fn float get_atom_coord (gchar * line, int mid)

  \brief read atom coordinates from CIF file

  \param line the string that contains the data
  \param mid the position to reach on the line
*/
float get_atom_coord (gchar * line, int mid)
{
  gchar * co_line = g_strdup_printf ("%s", line);
  char * co_word = strtok_r (co_line, " ", & line);
  int i;
  for (i=0; i<mid-1; i++)
  {
    co_word = strtok_r (NULL, " ", & line);
  }
  double v = string_to_double ((gpointer)get_cif_word(co_word));
  co_line = NULL;
  co_word = NULL;
  return v;
}

/*!
  \fn gchar * get_atom_label (gchar * line, int lid)

  \brief read atom label from CIF file

  \param line the string that contains the data
  \param lid the position to reach on the line
*/
gchar * get_atom_label (gchar * line, int lid)
{
  gchar * at_line = g_strdup_printf ("%s", line);
  char * at_word = strtok_r (at_line, " ", & line);
  int i;
  for (i=0; i<lid-1; i++) at_word = strtok_r (NULL, " ", & line);
  gchar * str;
  for (i=0; i<10; i++)
  {
    str = g_strdup_printf ("%d", i);
    at_word = substitute_string (at_word, str, NULL);
    g_free (str);
  }
  at_word = get_cif_word (at_word);
  at_word = substitute_string (at_word, "-", NULL);
  at_word = substitute_string (at_word, "+", NULL);
  g_free (at_line);
  return g_strdup_printf ("%c%c", at_word[0], tolower(at_word[1]));
}

/*!
  \fn int get_atom_wyckoff (gchar * line, int wid)

  \brief read Wyckoff position from CIF file

  \param line the string that contains the data
  \param wid the position to reach on the line
*/
int get_atom_wyckoff (gchar * line, int wid)
{
  gchar * wy_line = g_strdup_printf ("%s", line);
  char * wy_word = strtok_r (wy_line, " ", & line);
  int i, j;
  j = 0;
  for (i=0; i<wid-1; i++) wy_word = strtok_r (NULL, " ", & line);
  for (i=0; i<this_reader -> lattice.sp_group -> numw; i++)
  {
    if (g_strcmp0(get_cif_word(wy_word), this_reader -> lattice.sp_group -> wyckoff[i].let) == 0)
    {
      j = i;
      break;
    }
  }
  wy_line = NULL;
  wy_word = NULL;
  return j;
}

GtkWidget ** img_cif;
atom_search * cif_search = NULL;
atomic_object * cif_object = NULL;

/*!
  \fn G_MODULE_EXPORT void set_cif_to_insert (GtkComboBox * box, gpointer data)

  \brief change the object to insert at an empty cif position

  \param box the GtkComboBox sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void set_cif_to_insert (GtkComboBox * box, gpointer data)
{
  GValue val = {0, };
  int i, j, k;
  i = GPOINTER_TO_INT(data);
  GtkTreeModel * cmodel = gtk_combo_box_get_model (box);
  GtkTreeIter iter;
  gchar * str;
  gboolean done = TRUE;
  if (gtk_combo_box_get_active_iter (box, & iter))
  {
    gtk_tree_model_get_value (cmodel, & iter, 0, & val);
    str = g_strdup_printf ("%s", (char *)g_value_get_string (& val));
    j = get_selected_object_id (FALSE, activep, str, cif_search);
    to_insert_in_project (j, i, active_project, cif_search, FALSE);
    if (j > 0)
    {
      gtk_tree_store_set (GTK_TREE_STORE(cmodel), & iter, 0, periodic_table_info[j].lab, -1);
    }
    cif_search -> todo[i]  = (! j) ? 0 : 1;
    if (! j) done = FALSE;
    cif_search -> in_selection = 0;
    for (k=0; k<this_reader -> object_to_insert; k++) cif_search -> in_selection += cif_search -> todo[k];
  }
  str = (done) ? g_strdup_printf (APPLY) : g_strdup_printf (DELETEB);
  set_image_from_icon_name (img_cif[i], str);
  g_free (str);
  if (! done) gtk_combo_box_set_active (box, 0);
}

/*!
  \fn gboolean get_missing_object_from_user ()

  \brief get missing atomic number in CIF file from the user
*/
gboolean get_missing_object_from_user ()
{
  cif_search = allocate_atom_search (activep, REPLACE, 0, this_reader -> object_to_insert);
  cif_object = NULL;
  GtkWidget * info = dialogmodal ("Error while reading CIF file", GTK_WINDOW(MainWindow));
  GtkWidget * vbox, * hbox;
  gchar * str;
  vbox = dialog_get_content_area (info);
  gchar * labpick = "<b>To continue and build the crystal according to the information of the CIF file\n"
                    "it is required to provide a suitable value for each and every missing parameter(s).</b>"
                    "\n\nPlease select an atom type for the following object(s):";
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, markup_label (labpick, 200, -1, 0.5, 0.5), FALSE, FALSE, 10);
  img_cif = g_malloc0(this_reader -> object_to_insert*sizeof*img_cif);
  GtkWidget * but;
  GtkCellRenderer * renderer;
  GtkTreeModel * model;
  GList * cell_list;
  int i;
  for (i=0; i<this_reader -> object_to_insert; i++)
  {
    hbox = create_hbox(0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 5);
    str = g_strdup_printf ("Type N°%d:\t<b>%s</b>", i+1, this_reader -> label[i]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label(str, 150, -1, 0.0, 0.5), FALSE, FALSE, 20);
    g_free (str);
    img_cif[i] = stock_image (DELETEB);
    model = replace_combo_tree (TRUE, activep);
    but = gtk_combo_box_new_with_model (model);
    g_object_unref (model);
    renderer = gtk_cell_renderer_combo_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (but), renderer, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (but), renderer, "text", 0, NULL);
    gtk_combo_box_set_active (GTK_COMBO_BOX(but), 0);
    g_signal_connect (G_OBJECT(but), "changed", G_CALLBACK(set_cif_to_insert), GINT_TO_POINTER(i));
    cell_list = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(but));
    if(cell_list && cell_list -> data)
    {
      gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(but), cell_list -> data, "markup", 0, NULL);
    }
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, but, FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, img_cif[i], FALSE, FALSE, 30);
  }

  gchar * endpick = "In case of a molecule: insert an extra type of atom and run a substitution afterwards.";
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, markup_label (endpick, 200, -1, 0.5, 0.5), FALSE, FALSE, 10);
  run_this_gtk_dialog (info, G_CALLBACK(run_destroy_dialog), NULL);
  g_free (img_cif);
  return (cif_search -> in_selection == this_reader -> object_to_insert) ? TRUE : FALSE;
}

#ifndef OPENMP
/*!
  \fn void file_get_to_line (int line_id)

  \brief reach line in CIF file

  \param line_id Line to reach
*/
void file_get_to_line (int line_id)
{
  int i;
  tail = head;
  for (i=0; i<line_id; i++) tail = tail -> next;
}
#endif

/*!
  \fn int cif_get_value (gchar * kroot, gchar * keyw, int linec, int lstart, gchar ** cif_word, gboolean rec_val, gboolean all_ligne, gboolean in_loop)

  \brief read pattern in CIF file

  \param kroot String root (first part)
  \param keyw String root (first part)
  \param linec Total number of lines
  \param lstart Line to reach
  \param cif_word pointer to store the data read
  \param rec_val Record position on the line
  \param all_ligne Browse all line (1/0)
  \param in_loop More than one identical key string (1/0)
*/
int cif_get_value (gchar * kroot, gchar * keyw, int linec, int lstart, gchar ** cif_word, gboolean rec_val, gboolean all_ligne, gboolean in_loop)
{
  int res = 0;
  int i;
  size_t j, k, l, m;
  gchar * str;
  gchar * str_w, * str_a, * str_b;
  gchar * mot;
  j = strlen(kroot);
  k = strlen(keyw);
  l = j+k+1;
#ifdef OPENMP
  int numth = (in_loop) ? 1 : omp_get_max_threads ();
  gchar * saved_line;
  #pragma omp parallel for num_threads(numth) private(i,m,this_line,saved_line,this_word,str_a,str_b,str_w) shared(j,k,l,this_reader,res,coord_line,cif_word,mot,rec_val,all_ligne,in_loop,kroot,keyw)
  for (i=lstart; i<linec; i++)
  {
    if (res) goto endi;
    this_line = g_strdup_printf ("%s", coord_line[i]);
    saved_line = g_strdup_printf ("%s", this_line);
    this_word = strtok_r (this_line, " ", & saved_line);
    while (this_word)
    {
      if (in_loop && this_word[0] != '_')
      {
        res = -1;
        goto endi;
      }
      str_w = get_cif_word (this_word);
      str_w = g_ascii_strdown (str_w, strlen(str_w));
      if (strlen(str_w) == l)
      {
        str_a = g_strdup_printf ("%c", str_w[0]);
        for (m=1; m<j; m++) str_a = g_strdup_printf ("%s%c", str_a, str_w[m]);
        str_b = g_strdup_printf ("%c", str_w[j+1]);
        for (m=j+2; m<l; m++) str_b = g_strdup_printf ("%s%c", str_b, str_w[m]);
        if (g_strcmp0(str_a, kroot) == 0 && g_strcmp0(str_b,keyw) == 0)
        {
          this_word = strtok_r (NULL, " ", & saved_line);
          if (! this_word)
          {
            if (rec_val || all_ligne)
            {
              str = g_strdup_printf ("Wrong file format: searching for <b>%s</b> - error at line <b>%d</b> !", keyw, i+1);
              add_reader_info (str, 0);
              g_free (str);
              g_free (str_w);
              g_free (str_a);
              g_free (str_b);
              res = -1;
              goto endi;
            }
          }
          res = i+1;
          if (all_ligne)
          {
            mot = g_strdup_printf ("%s", this_word);
            this_word = strtok_r (NULL, " ", & saved_line);
            while (this_word)
            {
              mot = g_strdup_printf ("%s%s", mot, this_word);
              this_word = strtok_r (NULL, " ", & saved_line);
            }
            this_word = g_strdup_printf ("%s", mot);
            g_free (mot);
          }
          if (this_word) * cif_word = get_cif_word (this_word);
          g_free (str_w);
          g_free (str_a);
          g_free (str_b);
          goto endi;
        }
        g_free (str_a);
        g_free (str_b);
      }
      g_free (str_w);
      this_word = strtok_r (NULL, " ", & saved_line);
    }
    endi:;
  }
  if (res < 0) res = 0;
#else
  file_get_to_line (lstart);
  res = 0;
  i = lstart;
  while (tail)
  {
    this_line = g_strdup_printf ("%s", tail -> line);
    this_word = strtok (this_line, " ");
    while (this_word)
    {
      if (in_loop && this_word[0] != '_') return 0;
      str_w = get_cif_word(this_word);
      str_w = g_ascii_strdown (str_w, strlen(str_w));
      if (strlen(str_w) == l)
      {
        str_a = g_strdup_printf ("%c", str_w[0]);
        for (m=1; m<j; m++) str_a = g_strdup_printf ("%s%c", str_a, str_w[m]);
        str_b = g_strdup_printf ("%c", str_w[j+1]);
        for (m=j+2; m<l; m++) str_b = g_strdup_printf ("%s%c", str_b, str_w[m]);
        if (g_strcmp0(str_a, kroot) == 0 && g_strcmp0(str_b,keyw) == 0)
        {
          this_word = strtok (NULL, " ");
          if (! this_word)
          {
            if (rec_val || all_ligne)
            {
              str = g_strdup_printf ("Wrong file format: searching for <b>%s</b> - error at line <b>%d</b> !", keyw, i+1);
              add_reader_info (str, 0);
              g_free (str);
              g_free (str_w);
              g_free (str_a);
              g_free (str_b);
              return 0;
            }
          }
          if (all_ligne)
          {
            mot = g_strdup_printf ("%s", this_word);
            this_word = strtok (NULL, " ");
            while (this_word)
            {
              mot = g_strdup_printf ("%s%s", mot, this_word);
              this_word = strtok (NULL, " ");
            }
            this_word = g_strdup_printf ("%s", mot);
            g_free (mot);
          }
          if (this_word) * cif_word = get_cif_word (this_word);
          g_free (str_w);
          g_free (str_a);
          g_free (str_b);
          return i+1;
        }
        g_free (str_a);
        g_free (str_b);
      }
      g_free (str_w);
      this_word = strtok (NULL, " ");
    }
    tail = tail -> next;
    i ++;
  }
#endif
  return res;
}

/*!
  \fn int cif_file_get_data_in_loop (int linec, int lid)

  \brief get the number of "_" motifs in a line

  \param linec Total number of lines
  \param lid Line to reach
*/
int cif_file_get_data_in_loop (int linec, int lid)
{
  gboolean res = FALSE;
  int i = 0;
#ifdef OPENMP
  while (! res)
  {
    if (lid+i < linec)
    {
      this_line = g_strdup_printf ("%s", coord_line[lid+i]);
      this_word = strtok (this_line, " ");
      if (this_word[0] == '_')
      {
        i ++;
      }
      else
      {
        res = TRUE;
      }
    }
    else
    {
      res = TRUE;
    }
  }
#else
  file_get_to_line (lid);
  while (! res)
  {
    if (tail)
    {
      this_line = g_strdup_printf ("%s", tail -> line);
      this_word = strtok (this_line, " ");
      if (this_word[0] == '_')
      {
        i ++;
        tail = tail -> next;
      }
      else
      {
        res = TRUE;
      }
    }
    else
    {
      res = TRUE;
    }
  }
#endif
  return i;
}

/*!
  \fn int cif_file_get_number_of_atoms (int linec, int lid, int nelem)

  \brief get the number of atom(s) in a CIF file

  \param linec Total number of lines
  \param lid Line to reach
  \param nelem Number of element(s) the line
*/
int cif_file_get_number_of_atoms (int linec, int lid, int nelem)
{
  gboolean res = FALSE;
  int i, j;
  char init;
  i = 0;
#ifdef OPENMP
  while (! res && (lid+i) < linec)
  {
    this_line = g_strdup_printf ("%s", coord_line[lid+i]);
    this_word = strtok (this_line, " ");
    j = 0;
    while (this_word)
	{
      if (! j) init = this_word[0];
      j ++;
      this_word = strtok (NULL, " ");
    }
    if (j == nelem && init != '_')
    {
      i ++;
    }
    else
    {
      res = TRUE;
    }
  }
#else
  file_get_to_line (lid);
  while (! res && tail)
  {
    this_line = g_strdup_printf ("%s", tail -> line);
    this_word = strtok (this_line, " ");
    j = 0;
    while (this_word)
	{
      if (j == 0) init = this_word[0];
      j ++;
      this_word = strtok (NULL, " ");
    }
    if (j == nelem && init != '_')
    {
      i ++;
    }
    else
    {
      res = TRUE;
    }
    tail = tail -> next;
  }
#endif
  return i;
}

/*!
  \fn void check_for_to_lab (int ato, gchar * stlab)

  \brief check atom label

  \param ato Atom id
  \param stlab Label read in the CIF file
*/
void check_for_to_lab (int ato, gchar * stlab)
{
  int i, j;
  j = -1;
  // First is the label of 'ato' already listed
  for (i=0; i<this_reader -> object_to_insert; i++)
  {
    if (g_strcmp0(this_reader -> label[i], stlab) == 0)
    {
      j = i;
      break;
    }
  }
  if (j < 0)
  {
    if (this_reader -> label)
    {
      this_reader -> label = g_realloc (this_reader -> label, (this_reader -> object_to_insert+1)*sizeof*this_reader -> label);
    }
    else
    {
      this_reader -> label = g_malloc0 (1*sizeof*this_reader -> label);
    }
    this_reader -> label[this_reader -> object_to_insert] = g_strdup_printf ("%s", stlab);
    this_reader -> object_to_insert ++;
    j = this_reader -> object_to_insert-1;
  }
  if (this_reader -> object_list)
  {
    this_reader -> object_list = g_realloc (this_reader -> object_list, (this_reader -> atom_unlabelled+1)*sizeof*this_reader -> object_list);
    this_reader -> u_atom_list = g_realloc (this_reader -> u_atom_list, (this_reader -> atom_unlabelled+1)*sizeof*this_reader -> u_atom_list);
  }
  else
  {
    this_reader -> object_list = g_malloc0 (1*sizeof*this_reader -> object_list);
    this_reader -> u_atom_list = g_malloc0 (1*sizeof*this_reader -> u_atom_list);
  }
  this_reader -> object_list[this_reader -> atom_unlabelled] = j;
  this_reader -> u_atom_list[this_reader -> atom_unlabelled] = ato;
  this_reader -> atom_unlabelled ++;
}

/*!
  \fn gboolean cif_file_get_atoms_data (int lin, int cid[8])

  \brief get atoms data from the CIF file

  \param lin Line to reach
  \param cid positions on the line for the data to read
*/
gboolean cif_file_get_atoms_data (int lin, int cid[8])
{
  int i, j;
  double v;
  gchar * str;
  gboolean done = TRUE;
  gchar * cline;
#ifdef OPENMP
  int numth = omp_get_max_threads ();
  #pragma omp parallel for num_threads(numth) private(i,j,v,cline,str) shared(this_reader,coord_line,done)
  for (i=0; i<this_reader -> natomes; i++)
  {
    cline = g_strdup_printf ("%s", coord_line[i+lin]);
    str = get_atom_label (cline, (cid[0]) ? cid[0] : cid[1]);
    v = get_z_from_periodic_table (str);
    #pragma omp critical
    {
      if (v)
      {
        check_for_species (v, i);
      }
      else
      {
        done = FALSE;
        check_for_to_lab (i, str);
      }
    }
    if (this_reader -> cartesian)
    {
      active_project -> atoms[0][i].x = get_atom_coord (cline, cid[2]);
      active_project -> atoms[0][i].y = get_atom_coord (cline, cid[3]);
      active_project -> atoms[0][i].z = get_atom_coord (cline, cid[4]);
    }
    else
    {
      for (j=0; j<3; j++) this_reader -> coord[i][j] = get_atom_coord (cline, cid[j+2]);
    }
    if (! this_reader -> cartesian)
    {
      this_reader -> wyckoff[i] = (cid[5]) ? get_atom_wyckoff (cline, cid[5]) : 0;
      this_reader -> occupancy[i] = (cid[6]) ? get_atom_coord (cline, cid[6]) : 1.0;
      this_reader -> multi[i] = (cid[7]) ? get_atom_coord (cline, cid[7]) : 0.0;
    }
    if (cline) g_free (cline);
    if (str) g_free (str);
  }
#else
  file_get_to_line (lin);
  for (i=0; i<this_reader -> natomes; i++)
  {
    cline = g_strdup_printf ("%s", tail -> line);
    str = get_atom_label (cline, (cid[0]) ? cid[0] : cid[1]);
    v = get_z_from_periodic_table (str);
    if (v)
    {
      check_for_species (v, i);
    }
    else
    {
      done = FALSE;
      check_for_to_lab (i, str);
    }
    if (this_reader -> cartesian)
    {
      active_project -> atoms[0][i].x = get_atom_coord (cline, cid[2]);
      active_project -> atoms[0][i].y = get_atom_coord (cline, cid[3]);
      active_project -> atoms[0][i].z = get_atom_coord (cline, cid[4]);
    }
    else
    {
      for (j=0; j<3; j++) this_reader -> coord[i][j] = get_atom_coord (cline, cid[j+2]);
    }
    if (! this_reader -> cartesian)
    {
      this_reader -> wyckoff[i] = (cid[5]) ? get_atom_wyckoff (cline, cid[5]) : 0;
      this_reader -> occupancy[i] = (cid[6]) ? get_atom_coord (cline, cid[6]) : 1.0;
      this_reader -> multi[i] = (cid[7]) ? get_atom_coord (cline, cid[7]) : 0.0;
    }
/* #ifdef DEBUG
    j = this_reader -> wyckoff[i];
    g_debug ("CIF:: At= %s, w_letter[%d]= %s, occ= %f, x= %f, y= %f, z= %f", this_reader -> label[i],
             j, this_reader -> lattice.sp_group -> wyckoff[j].let, this_reader -> occupancy[i],
             this_reader -> coord[i][0], this_reader -> coord[i][1], this_reader -> coord[i][2]);
#endif */
    tail = tail -> next;
    g_free (tail -> prev);
  }
#endif

  if (! done)
  {
    done = (cif_search) ? TRUE : get_missing_object_from_user ();
  }
  return done;
}

/*!
  \fn int get_loop_line_id (int linec, int lid)

  \brief reach a line in the CIF file

  \param linec Total number of lines
  \param lid Line to reach
*/
int get_loop_line_id (int linec, int lid)
{
  int i;
  gchar * str_w;
#ifdef OPENMP
  gchar * saved_line;
  for (i=lid-1; i>-1; i--)
  {
    this_line = g_strdup_printf ("%s", coord_line[i]);
    saved_line = g_strdup_printf ("%s", this_line);
    this_word = strtok_r (this_line, " ", & saved_line);
    if (this_word)
    {
      str_w = g_ascii_strdown (this_word, strlen(this_word));
      if (g_strcmp0 ("loop_", get_cif_word(str_w)) == 0)
      {
        g_free (str_w);
        return i+1;
      }
      g_free (str_w);
    }
  }
#else
  file_get_to_line (lid);
  i = lid;
  while (tail)
  {
    this_line = g_strdup_printf ("%s", tail -> line);
    this_word = strtok (this_line, " ");
    if (this_word)
    {
      str_w = g_ascii_strdown (this_word, strlen(this_word));
      if (g_strcmp0 ("loop_", get_cif_word(str_w)) == 0)
      {
        g_free (str_w);
        return i+1;
      }
      g_free (str_w);
    }
    i --;
    tail = tail -> prev;
  }
#endif
  return 0;
}

/*!
  \fn int get_loop_line_for_key (gchar * key_a, gchar * key_b, int linec)

  \brief search a string

  \param key_a String root (first part)
  \param key_b String end (second part)
  \param linec Total number of lines
*/
int get_loop_line_for_key (gchar * key_a, gchar * key_b, int linec)
{
  int i;
  gchar * str;
  i = cif_get_value (key_a, key_b, linec, 0, & str, FALSE, FALSE, FALSE);
  return (i) ? get_loop_line_id (linec, i) : 0;
}

/*!
  \fn gboolean cif_get_atomic_coordinates (int linec)

  \brief read the atomic coordinates from the CIF file

  \param linec Total number of lines
*/
gboolean cif_get_atomic_coordinates (int linec)
{
  gchar * labkeys[2] = {"type_symbol", "label"};
  gchar * frackeys[3] = {"fract_x", "fract_y", "fract_z"};
  gchar * cartkeys[3] = {"cartn_x", "cartn_y", "cartn_z"};
  gchar * symkeys[3] = {"wyckoff_symbol", "occupancy", "symmetry_multiplicity"};
  gchar * str = NULL;
  int cid[8];
  int loop_line;
  int i, j, k, l;

  loop_line = get_loop_line_for_key ("_atom_site", cartkeys[0], linec);
  if (! loop_line)
  {
    loop_line = get_loop_line_for_key ("_atom_site", frackeys[0], linec);
    if (! loop_line)
    {
      return FALSE;
    }
  }
  else
  {
    this_reader -> cartesian = TRUE;
  }
  i = 0;
  for (j=0; j<2; j++)
  {
    cid[j] = cif_get_value ("_atom_site", labkeys[j], linec, loop_line, & str, FALSE, FALSE, TRUE);
    if (cid[j])
    {
      i ++;
      cid[j] -= loop_line;
    }
  }
  if (! i)
  {
    add_reader_info ("<b>Atomic coordinates</b>: impossible to find atomic label(s) ...", 0);
    return FALSE;
  }

  i = 0;
  for (j=0; j<3; j++)
  {
    cid[j+2] = cif_get_value ("_atom_site", cartkeys[j], linec, loop_line, & str, FALSE, FALSE, TRUE);
    if (cid[j+2])
    {
      i ++;
      cid[j+2] -= loop_line;
    }
    else if (this_reader -> cartesian)
    {
      str = g_strdup_printf ("<b>Atomic coordinates</b>: impossible to find '%s' ...", cartkeys[j]);
      add_reader_info (str, 1);
      g_free (str);
      this_reader -> cartesian = FALSE;
    }
  }
  if (i < 3)
  {
    j = 0;
    for (l=0; l<3; l++)
    {
      cid[l+2] = cif_get_value ("_atom_site", frackeys[l], linec, loop_line, & str, FALSE, FALSE, TRUE);
      if (cid[l+2])
      {
        cid[l+2] -= loop_line;
        j ++;
      }
      else
      {
        str = g_strdup_printf ("<b>Atomic coordinates</b>: impossible to find '%s' ...", frackeys[l]);
        add_reader_info (str, 1);
        g_free (str);
      }
    }
    if (j < 3)
    {
      add_reader_info ("<b>Atomic coordinates</b>: no complete cartesian coordinates !", 0);
      add_reader_info ("<b>Atomic coordinates</b>: no complete fractional coordinates !", 0);
      return FALSE;
    }
  }
  else
  {
    this_reader -> cartesian = TRUE;
  }

  if (! this_reader -> cartesian)
  {
    for (i=0; i<3; i++)
    {
      cid[i+5] = cif_get_value ("_atom_site", symkeys[i], linec, loop_line, & str, FALSE, FALSE, TRUE);
      if (cid[i+5])
      {
        cid[i+5] -= loop_line;
      }
    }
  }
  i = cif_file_get_data_in_loop (linec, loop_line);
#ifdef DEBUG
  g_debug ("CIF:: Num of field in loop= %d", i);
#endif
  this_reader -> natomes = cif_file_get_number_of_atoms (linec, loop_line+i, i);
#ifdef DEBUG
  g_debug ("CIF:: Number of atoms in CIF file= %d", this_reader -> natomes);
#endif
  if (! this_reader -> natomes) return FALSE;
  if (this_reader -> cartesian)
  {
    active_project -> steps = 1;
    active_project -> natomes = this_reader -> natomes;
    allocatoms (active_project);
  }
  else
  {
    this_reader -> coord = allocddouble (this_reader -> natomes, 3);
  }
  if (! this_reader -> cartesian)
  {
    this_reader -> lot = allocint (this_reader -> natomes);
    this_reader -> wyckoff = allocint (this_reader -> natomes);
	this_reader -> occupancy = allocdouble (this_reader -> natomes);
    this_reader -> multi = allocint (this_reader -> natomes);
  }
  this_reader -> z = allocdouble (1);
  this_reader -> nsps = allocint (1);
  if (! cif_file_get_atoms_data (loop_line+i, cid)) return FALSE;
  if (! this_reader -> cartesian)
  {
    this_reader -> occupied = g_malloc0(this_reader -> natomes*sizeof*this_reader -> occupied);
    double v;
    for (i=0; i<this_reader -> natomes; i++)
    {
      for (j=0; j<2; j++)
      {
        k = 1;
        v = this_reader -> occupancy[i];
        for (l=0; l<this_reader -> natomes; l++)
        {
          if (l != i)
          {
            if (this_reader -> coord[i][0] == this_reader -> coord[l][0]
             && this_reader -> coord[i][1] == this_reader -> coord[l][1]
             && this_reader -> coord[i][2] == this_reader -> coord[l][2])
            {
              v += this_reader -> occupancy[l];
              k ++;
              if (j) this_reader -> occupied[i][k] = l;
              if (v > 1.00001)
              {
                add_reader_info ("<b>Atomic coordinates</b>: a site was found to have an occupancy > 1.0 !", 0);
                return FALSE;
              }
            }
          }
        }
        if (! j)
        {
          this_reader -> occupied[i] = allocint (k+1);
          this_reader -> occupied[i][0] = k;
          this_reader -> occupied[i][1] = i;
        }
      }
    }
  }
  return TRUE;
}

/*!
  \fn int cif_file_get_number_of_positions (int linec, int lid)

  \brief get the number of symmetry positions

  \param linec Total number of lines
  \param lid Line to reach
*/
int cif_file_get_number_of_positions (int linec, int lid)
{
  gboolean res = FALSE;
  int i = 0;
  while (! res)
  {
#ifdef OPENMP
    this_line = g_strdup_printf ("%s", coord_line[lid+i]);
#else
    file_get_to_line (lid+i);
    this_line = g_strdup_printf ("%s", tail -> line);
#endif
    this_word = strtok (this_line, " ");
    if (this_word[0] == '_' || g_strcmp0(this_word, "loop_") == 0)
    {
      res = TRUE;
      break;
    }
    else
    {
      i ++;
    }
  }
  if (i)
  {
    this_reader -> sym_pos = g_malloc0(i*sizeof*this_reader -> sym_pos);
    int j, k;
    gchar * str;
    gchar * k_word;
    gchar * sym_pos_line;
    for (j=0; j<i; j++)
    {
      this_reader -> sym_pos[j] = g_malloc0(3*sizeof*this_reader -> sym_pos[j]);
#ifdef OPENMP
      sym_pos_line = g_strdup_printf ("%s", coord_line[lid+j]);
#else
      file_get_to_line (lid+j);
      sym_pos_line = g_strdup_printf ("%s", tail -> line);
#endif
      this_line = g_strdup_printf ("%s", sym_pos_line);
      this_word = strtok (this_line, " ");
      k_word = g_strdup_printf ("%s", this_word);
      str = g_strdup_printf ("%d", j+1);
      if (g_strcmp0(k_word, str) == 0)
      {
        g_free (this_line);
        this_line = NULL;
        for (k=strlen(k_word); k<strlen(sym_pos_line); k++)
        {
          if (! this_line)
          {
            this_line = g_strdup_printf ("%c", sym_pos_line[k]);
          }
          else
          {
            this_line = g_strdup_printf ("%s%c", this_line, sym_pos_line[k]);
          }
        }
      }
      g_free (k_word);
      g_free (str);
      g_free (sym_pos_line);
      this_line = substitute_string (this_line, "'", NULL);
      this_line = substitute_string (this_line, ",", " ");
      this_word = strtok (this_line, " ");
      for (k=0; k<3; k++)
      {
         this_reader -> sym_pos[j][k] = g_strdup_printf ("%s", this_word);
         this_word = strtok (NULL, " ");
      }
    }
  }
  return i;
}

/*!
  \fn gboolean cif_get_symmetry_positions (int linec)

  \brief read the symmetry positions from the CIF file

  \param linec Total number of lines
*/
gboolean cif_get_symmetry_positions (int linec)
{
  gchar * pos_key[2]={"_symmetry_equiv_pos_as", "_space_group_symop_operation"};
  gchar * str;
  int loop_line;
  int line_id;
  int i;
  for (i=0; i<2; i++)
  {
    loop_line = get_loop_line_for_key (pos_key[i], "xyz", linec);
    if (loop_line)
    {
      line_id = cif_get_value (pos_key[i], "xyz", linec, loop_line, & str, FALSE, FALSE, TRUE);
      break;
    }
  }
  if (! loop_line) return FALSE;
  // Read lines after the instruction, as many positions as line until _ or loop
  this_reader -> num_sym_pos = cif_file_get_number_of_positions (linec, line_id);
  return TRUE;
}

/*!
  \fn int get_space_group_from_hm (gchar * hmk)

  \brief retrieve space group using the HM Key

  \param hmk
*/
int get_space_group_from_hm (gchar * hmk)
{
  int i;
  gchar * str;
  gchar * hm = g_strdup_printf ("%s", replace_markup (hmk, "S", NULL));
  for (i=0; i<230; i++)
  {
    str = substitute_string (hmsymbols[i], " ", NULL);
    if (g_strcmp0(str, hm) == 0)
    {
      g_free (str);
      g_free (hm);
      return i+1;
    }
    g_free (str);
    str = substitute_string (groups[i], "<sub>", NULL);
    str = substitute_string (str, "</sub>", NULL);
    if (g_strcmp0(str, hm) == 0)
    {
      g_free (str);
      g_free (hm);
      return i+1;
    }
    g_free (str);
  }
  // Cross checking for erroneus writting in the CIF file
  // ie. Fm3m in place of Fm-3m
  for (i=0; i<230; i++)
  {
    str = substitute_string (groups[i], "<sub>", NULL);
    str = substitute_string (str, "</sub>", NULL);
    if (g_strrstr(str, "-"))
    {
      str = substitute_string (str, "-", NULL);
      if (g_strcmp0(str, hm) == 0)
      {
        g_free (str);
        g_free (hm);
        return i+1;
      }
    }
    g_free (str);
  }
  g_free (hm);
  return 0;
}

/*!
  \fn gchar * get_string_from_origin (space_group * spg)

  \brief get the space group origin from its name

  \param spg Space group
*/
gchar * get_string_from_origin (space_group * spg)
{
  gchar * str = NULL;
  if (wnpos[1])
  {
    g_free (wnpos[1]);
    wnpos[1] = NULL;
  }

  get_wyck_char (spg -> coord_origin.m01, 1, 0);
  get_wyck_char (spg -> coord_origin.m11, 1, 1);
  get_wyck_char (spg -> coord_origin.m21, 1, 2);

  if (wnpos[1])
  {
    str = g_strdup_printf ("%s", wnpos[1]);
    g_free (wnpos[1]);
    wnpos[1] = NULL;
  }
  return str;
}

/*!
  \fn int get_setting_from_hm (gchar * hmk, int end)

  \brief Getting the space group parameters using the HM Key

  \param hmk the HM key
  \param end Use origin (number of possible SP origins), or not (-1)
*/
int get_setting_from_hm (gchar * hmk, int end)
{
  int i, j;
  gchar * str;
  if (this_reader -> lattice.sp_group)
  {
    i = this_reader -> lattice.sp_group -> nums;
    for (j=0; j<i; j++)
    {
      str = replace_markup (this_reader -> lattice.sp_group -> settings[j].name, "s", "/");
      str = substitute_string (str, "_", NULL);
      if (end < 0)
      {
        if (g_strcmp0(str, hmk) == 0)
        {
          g_free (str);
          return j;
        }
      }
      else
      {
        if (g_strcmp0(str, hmk) == 0 && this_reader -> lattice.sp_group -> settings[j].origin == end+1)
        {
          g_free (str);
          return j;
        }
      }
      g_free (str);
    }
    if (this_reader -> lattice.sp_group -> id > 2 && this_reader -> lattice.sp_group -> id < 16)
    {
      // This is a way around the way this familly of SG is often written,
      // using incomplete or inexact hmk keyword, ex: P21/a instead of P121/a1
      for (j=0; j<i; j++)
      {
        str = replace_markup (this_reader -> lattice.sp_group -> settings[j].name, "s", "/");
        str = substitute_string (str, "_", NULL);
        str = substitute_string (str, "12", "2");
        str = substitute_string (str, "/a1", "/a");
        str = substitute_string (str, "/b1", "/b");
        str = substitute_string (str, "/c1", "/c");
        str = substitute_string (str, "/m1", "/m");
        str = substitute_string (str, "/n1", "/n");
        if (end < 0)
        {
          if (g_strcmp0(str, hmk) == 0)
          {
            g_free (str);
            str = g_strdup_printf ("<b>Space group</b>: CIF file information could be inaccurate !\n"
                                   " CIF file space group: <b>%s</b>, CIF file H-M symbol: <b>%s</b>",
                                   groups[this_reader -> lattice.sp_group -> id-1], hmk);
            add_reader_info (str, 1);
            g_free (str);
            return j;
          }
        }
        else
        {
          if (g_strcmp0(str, hmk) == 0 && this_reader -> lattice.sp_group -> settings[j].origin == end+1)
          {
            g_free (str);
            str = g_strdup_printf ("<b>Space group</b>: CIF file information could be inaccurate !\n"
                                   " CIF file space group: <b>%s</b>, CIF file H-M symbol: <b>%s</b>",
                                   groups[this_reader -> lattice.sp_group -> id-1], hmk);
            add_reader_info (str, 1);
            g_free (str);
            return j;
          }
        }
        g_free (str);
      }
    }
    return -1;
  }
  else
  {
    return -1;
  }
}

/*!
  \fn int group_info_from_hm_key (int spg, gchar * key_hm)

  \brief get the space group information using the HM key from the CIF file

  \param spg Space group
  \param key_hm HM key
*/
int group_info_from_hm_key (int spg, gchar * key_hm)
{
  int i, j;
  gchar * str;
  gchar * exts[2] = {"h", "r"};
  gchar * orig[2] = {"1", "2"};
  gchar * key = NULL;
  gchar * hmk = NULL;
  gchar * hma, * hmb;
  gchar * hmkey = substitute_string (key_hm, "'", NULL);
  //hmkey = substitute_string (hmkey, "/", "s");
  this_reader -> setting = -1;
  if (strstr(hmkey,":"))
  {
    key = g_strdup_printf ("%s", hmkey);
    hmk = g_strdup_printf ("%s", strtok (key, ":"));
    hma = g_strdup_printf ("%s:", hmk);
    hmb = replace_markup (hmkey, hma, NULL);
    i = strlen(hmb);
    hmb = g_ascii_strdown (hmb, i);
    for (i=0; i<2; i++)
    {
      if (g_strcmp0(hmb, exts[i]) == 0)
      {
        this_reader -> setting = i;
        break;
      }
    }
    g_free (hma);
    g_free (key);
    if (this_reader -> setting < 0)
    {
      for (i=0; i<2; i++)
      {
        if (g_strcmp0(hmb, orig[i]) == 0)
        {
          j = get_space_group_from_hm (hmk);
          this_reader -> setting = (spg) ? get_setting_from_hm (hmk, i) : 0;
          if (! j && this_reader -> setting < 0)
          {
            this_reader -> setting = 0;
            str = g_strdup_printf ("<b>Space group</b>: CIF file information could be inaccurate !\n"
                                   " CIF file space group: <b>%s</b>, CIF file H-M symbol: <b>%s</b>",
                                   groups[this_reader -> lattice.sp_group -> id-1], key_hm);
            add_reader_info (str, 1);
            g_free (str);
          }
          if (this_reader -> setting < 0) this_reader -> setting = 0;
          g_free (hmk);
          g_free (hmb);
          return j;
        }
      }
    }
    g_free (hmb);
  }
  else
  {
    hmk = g_strdup_printf ("%s", hmkey);
  }
  j = get_space_group_from_hm (hmk);
  this_reader -> setting = (spg || j) ? get_setting_from_hm (hmk, -1) : 0;
  g_free (hmk);
  if (! j && this_reader -> setting < 0)
  {
    str = g_strdup_printf ("<b>Space group</b>: CIF file information could be inaccurate !\n"
                           " CIF file space group: <b>%s</b>, CIF file H-M symbol: <b>%s</b>",
                           groups[this_reader -> lattice.sp_group -> id-1], key_hm);
    add_reader_info (str, 1);
    g_free (str);
  }
  if (this_reader -> setting < 0) this_reader -> setting = 0;
  return (spg) ? (j) ? j : spg : j;
}

/*!
  \fn gboolean cif_get_cell_data (int linec)

  \brief get the cell data from the CIF file

  \param linec Total number of lines
*/
gboolean cif_get_cell_data (int linec)
{
  gchar * cellkeys[3] = {"length_a", "length_b", "length_c"};
  gchar * cellangs[3] = {"angle_alpha", "angle_beta", "angle_gamma",};
  gchar * str = NULL;
  int i;
  this_reader -> lattice.box = g_malloc0(sizeof*this_reader -> lattice.box);
  for (i=0; i<3; i++)
  {
    if (! cif_get_value ("_cell", cellkeys[i], linec, 0, & str, TRUE, FALSE, FALSE))
    {
      str = g_strdup_printf ("<b>Lattice parameters</b>: impossible to retrieve the '%s' parameter !", box_prop[0][i]);
      add_reader_info (str, 0);
      g_free (str);
      return FALSE;
    }
    this_reader -> lattice.box[0].param[0][i] = string_to_double ((gpointer)str);
#ifdef DEBUG
    g_debug ("CIF:: box[0][%d]= %f", i, this_reader -> lattice.box[0].param[0][i]);
#endif
    if (! cif_get_value ("_cell", cellangs[i], linec, 0, & str, TRUE, FALSE, FALSE))
    {
      str = g_strdup_printf ("<b>Lattice parameters</b>: impossible to retrieve the '%s' parameter !", box_prop[1][i]);
      add_reader_info (str, 0);
      g_free (str);
      return FALSE;
    }
    this_reader -> lattice.box[0].param[1][i] = string_to_double ((gpointer)str);
#ifdef DEBUG
    g_debug ("CIF:: box[1][%d]= %f", i, this_reader -> lattice.box[0].param[1][i]);
#endif
  }
  this_reader -> lattice.ltype = 0;
  compute_lattice_properties (& this_reader -> lattice);
  for (i=0; i<3; i++) this_reader -> lattice.cextra[i] = 1;
  return TRUE;
}

/*!
  \fn int cif_get_space_group (int linec)

  \brief get the space group from the CIF file

  \param linec Total number of lines
*/
int cif_get_space_group (int linec)
{
  gchar * symkey[2] = {"int_tables_number", "group_it_number"};
  gchar * str = NULL;
  int spg = 0;
  int i, j, k, l;
  for (i=0; i<2; i++)
  {
    if (cif_get_value ("_symmetry", symkey[i], linec, 0, & str, TRUE, FALSE, FALSE))
    {
      spg = (int)string_to_double ((gpointer)str);
      break;
    }
  }
  if (! spg)
  {
    if (cif_get_value ("_space_group", "it_number", linec, 0, & str, TRUE, FALSE, FALSE))
    {
      spg = (int)string_to_double ((gpointer)str);
    }
  }
  gchar * hmkey = NULL;
  if (! cif_get_value ("_symmetry", "space_group_name_h-m", linec, 0, & hmkey, FALSE, TRUE, FALSE))
  {
    cif_get_value ("_space_group", "name_h-m_alt", linec, 0, & hmkey, FALSE, TRUE, FALSE);
  }
  if (! hmkey && ! spg)
  {
    add_reader_info ("<b>Space group</b>: no space group and no H-M symbol found !", 1);
    return FALSE;
  }
#ifdef DEBUG
  if (spg) g_debug ("CIF:: Space group:: N°= %d, name= %s", spg, groups[spg-1]);
  if (hmkey) g_debug ("CIF:: H-M symbol:: %s", hmkey);
#endif
  if (spg)
  {
    if (! read_space_group (NULL, spg-1)) return FALSE;
  }
  if (hmkey)
  {
    i = group_info_from_hm_key (spg, hmkey);
    if (! spg && ! i)
    {
      add_reader_info ("<b>Space group</b>: no space group found, unknown H-M symbol !", 1);
#ifdef DEBUG
      g_debug ("CIF:: No space group found, unknown H-M symbol !");
#endif
    }
    else if (spg && ! i)
    {
      str = g_strdup_printf ("<b>Space group</b>: space group and H-M symbol do not match !\n"
                             " CIF file space group: <b>%s</b>, CIF file H-M symbol: <b>%s</b>", groups[spg-1], hmkey);
      add_reader_info (str, 1);
      g_free (str);
#ifdef DEBUG
      g_debug ("CIF:: Space group and H-M symbol do not match:: spg= %d, hm= %d", spg, i);
#endif
    }
    else if (i && ! spg)
    {
      spg = i;
    }
    if (! this_reader -> lattice.sp_group)
    {
      if (! read_space_group (NULL, spg-1)) return FALSE;
    }
  }
  gchar * lat;
  int res = spg;
  if (spg > 1)
  {
    if (cif_get_value ("_space_group", "it_coordinate_system_code", linec, 0, & str, FALSE, FALSE, FALSE))
    {
      str = substitute_string (str, "'", NULL);
      for (i=0; i<40; i++)
      {
        if (g_strcmp0(str, cif_coord_opts[i][0]) == 0)
        {
          if (i < 18)
          {
            if (spg < 3 || spg > 15)
            {
              res = 0;
              break;
            }
            if (str[0] == '-')
            {
              k = (int) string_to_double ((gpointer)g_strdup_printf ("%c", str[2]));
              str = g_strdup_printf ("%c%c", str[0], str[1]);
            }
            else
            {
              k = (int) string_to_double ((gpointer)g_strdup_printf ("%c", str[1]));
              str = g_strdup_printf ("%c", str[0]);
            }
            l = 0;
            for (j=0; j< this_reader -> lattice.sp_group -> nums; j++)
            {
              this_reader -> lattice.sp_group -> sid = j;
              get_origin (this_reader -> lattice.sp_group);
              lat = get_string_from_origin (this_reader -> lattice.sp_group);
              if (g_strcmp0(lat, str) == 0) l ++;
              if (l == k)
              {
                if (j < this_reader -> setting) add_reader_info ("<b>Space group</b>: ambiguous space group setting !", 1);
                this_reader -> setting = j;
                break;
              }
            }
          }
          else if (i < 36)
          {
            if (spg < 16 || spg > 74)
            {
              res = 0;
              break;
            }
            k = 0;
            if (str[0] == '1' || str[0]=='2')
            {
              k = (int) string_to_double ((gpointer)g_strdup_printf ("%c", str[0]));
              str = replace_markup (str, g_strdup_printf("%d", k), NULL);
            }
            l = 0;
            for (j=0; j< this_reader -> lattice.sp_group -> nums; j++)
           {
              lat = g_strdup_printf ("%s%s%s",
                                     this_reader -> lattice.sp_group -> settings[j].pos[0],
                                     this_reader -> lattice.sp_group -> settings[j].pos[1],
                                     this_reader -> lattice.sp_group -> settings[j].pos[2]);
              if (g_strcmp0(lat, str) == 0)
              {
                if (j < this_reader -> setting || this_reader -> lattice.sp_group -> settings[j].origin != k)
                {
                  add_reader_info ("<b>Space group</b>: ambiguous space group setting !", 1);
                }
                this_reader -> setting = j;
                l = 1;
                break;
              }
            }
            if (! l) add_reader_info ("<b>Space group</b>: ambiguous space group setting !", 1);
          }
          else if (i < 38)
          {
            if (spg < 75 || (spg > 142 && spg < 195))
            {
              res = 0;
              break;
            }
            j = i - 36;
            if (j < this_reader -> setting) add_reader_info ("<b>Space group</b>: ambiguous space group setting !", 1);
            this_reader -> setting = j;
          }
          else
          {
            if (spg < 143 || spg > 165)
            {
              res = 0;
              break;
            }
            j = i - 38;
            if (j < this_reader -> setting) add_reader_info ("<b>Space group</b>: ambiguous space group setting !", 1);
            this_reader -> setting = j;
          }
        }
      }
      g_free (str);
    }

    if (spg > 142 && spg < 168)
    {
      // Trigonal space group
      gboolean correct_this = FALSE;
      box_info * box = & this_reader -> lattice.box[0];
      switch (this_reader -> setting)
      {
        case 0:
          if (box -> param[0][0] == box -> param[0][1] && box -> param[0][0] == box -> param[0][2])
          {
            if (box -> param[1][0] == box -> param[1][1] && box -> param[1][0] == box -> param[1][2])
            {
              correct_this = TRUE;
            }
          }
          break;
        case 1:
          if (box -> param[0][0] == box -> param[0][1] && box -> param[0][0] != box -> param[0][2])
          {
            if (box -> param[1][0] == box -> param[1][1] && box -> param[1][0] == 90.0 && box -> param[1][2] == 120.0)
            {
              correct_this = TRUE;
            }
          }
          break;
      }
      if (correct_this)
      {
        gchar * setc[2] = {"<b>hexagonal</b>", "<b>rhombohedral</b>"};
        str = g_strdup_printf ("<b>Space group</b>: found trigonal space group N°%d-%s, %s setting\n"
                             "but the lattice parameters were found in %s format ...\n"
                             "\t ... the space group setting was modified accordingly !",
                               spg, groups[spg-1], setc[this_reader -> setting], setc[! this_reader -> setting]);
        add_reader_info (str, 1);
        g_free (str);
        this_reader -> setting = ! this_reader -> setting;
      }
    }

    // Test space group vs. box parameters:
    this_reader -> lattice.sp_group -> sid = this_reader -> setting;
    if (! test_lattice (NULL, & this_reader -> lattice))
    {
      str = g_strdup_printf ("<b>Space group</b> and <b>lattice paramters</b> are not compatible !\n"
                             "\nCheck a, b, c, and &#x3B1;, &#x3B2;, &#x263;, with the type of crystal system.");
      add_reader_info (str, 0);
      g_free (str);
      res = -1;
    }
  }
  this_reader -> lattice.sp_group -> sid = this_reader -> setting;
  get_origin (this_reader -> lattice.sp_group);
  return res;
}

/*!
  \fn int open_cif_file (int linec)

  \brief open CIF file

  \param linec Total number of lines
*/
int open_cif_file (int linec)
{
  int res;
  int i, j, k, l, m, n;

  if (cif_get_cell_data (linec))
  {
    i = cif_get_space_group (linec);
    if (i > 0)
    {
#ifdef DEBUG
      g_debug ("CIF:: SP setting:: %d, name= %s", this_reader -> setting+1, this_reader -> lattice.sp_group -> settings[this_reader -> setting].name);
#endif
      if (this_reader -> lattice.sp_group) get_origin (this_reader -> lattice.sp_group);
    }
    else if (i == 0)
    {
      // No space group found
#ifdef DEBUG
      g_debug ("CIF:: Impossible to retrieve space group information !");
#endif
    }
    else
    {
      // Error in space group
      return 3;
    }
  }
  // Reading positions
  if (cif_get_symmetry_positions (linec))
  {
    if (! cif_use_symmetry_positions && this_reader -> num_sym_pos)
    {
      add_reader_info ("Symmetry position(s) in CIF file\n", 1);
    }
  }
  if (cif_use_symmetry_positions && ! this_reader -> num_sym_pos)
  {
    add_reader_info ("No symmetry position(s) in CIF file\n", 0);
    return 3;
  }
  if (cif_get_atomic_coordinates (linec))
  {
    if (! this_reader -> cartesian)
    {
      for (i=0; i<3; i++)
      {
        for (j=0; j<3; j++)
        {
          if (i < 2)
          {
            active_box -> param[i][j] = this_reader -> lattice.box[0].param[i][j];
          }
          active_box -> vect[i][j] = this_reader -> lattice.box[0].vect[i][j];
        }
      }
      active_cell -> ltype = 1;
      active_cell -> pbc = TRUE;
      active_cell -> has_a_box = TRUE;
      if (this_reader -> lattice.sp_group)
      {
        active_cell -> crystal = TRUE;
        active_cell -> sp_group = duplicate_space_group (this_reader -> lattice.sp_group);
      }
    }
    res = 0;
    if (cif_use_symmetry_positions)
    {
      this_reader -> cartesian = TRUE;
      compute_lattice_properties (active_cell);
      active_project -> steps = 1;
      double spgpos[3][4];
      int max_pos = this_reader -> num_sym_pos * this_reader -> natomes;
      gboolean dist_message = FALSE;
      gboolean low_occ = FALSE;
      gboolean save_it;
      vec3_t f_pos, c_pos;
      gboolean * save_pos = allocbool (max_pos);
      mat4_t pos_mat;
      atom at, bt;
      distance dist;
      double u;
      vec3_t * all_pos = g_malloc0(max_pos*sizeof*all_pos);
      int * all_origin = allocint (max_pos);
      int * cif_pos = allocint (this_reader -> natomes);
      double ** cryst_pos = allocddouble (this_reader -> natomes, 3);
      double ** occ_pos = g_malloc0(sizeof*occ_pos);
      int ** lot_pos = g_malloc0(sizeof*lot_pos);
      int num_pos = 0;
      int pos_max = 0;
      for (i=0; i<2; i++)
      {
        for (j=0; j<this_reader -> natomes; j++)
        {
          if (! j)
          {
            num_pos = 0;
            for (k=0; k<3; k++) cryst_pos[num_pos][k] = this_reader -> coord[j][k];
            cif_pos[num_pos] = 1;
            if (i)
            {
              occ_pos[num_pos][0] = this_reader -> occupancy[j];
              lot_pos[num_pos][0] = this_reader -> lot[j];
            }
            num_pos ++;
            pos_max = 1;
          }
          else
          {
            save_it = TRUE;
            for (k=0; k<num_pos; k++)
            {
              if (this_reader -> coord[j][0] == cryst_pos[k][0]
               && this_reader -> coord[j][1] == cryst_pos[k][1]
               && this_reader -> coord[j][2] == cryst_pos[k][2])
              {
                save_it = FALSE;
                break;
              }
            }
            if (save_it)
            {
              for (k=0; k<3; k++) cryst_pos[num_pos][k] = this_reader -> coord[j][k];
              cif_pos[num_pos] = 1;
              if (i)
              {
                occ_pos[num_pos][0] = this_reader -> occupancy[j];
                lot_pos[num_pos][0] = this_reader -> lot[j];
              }
              num_pos ++;
            }
            else
            {
              if (i)
              {
                occ_pos[k][cif_pos[k]] = this_reader -> occupancy[j];
                lot_pos[k][cif_pos[k]] = this_reader -> lot[j];
              }
              cif_pos[k] ++;
              pos_max = max (pos_max, cif_pos[k]);
            }
          }
        }
        if (! i)
        {
          occ_pos = allocddouble (num_pos, pos_max);
          lot_pos = allocdint (num_pos, pos_max);
        }
      }
      for (i=0; i<num_pos; i++)
      {
        u = 0;
        for (j=0; j<cif_pos[i]; j++)
        {
          u += occ_pos[i][j];
        }
        if (u < 1.0)
        {
          low_occ = TRUE;
          break;
        }
      }
      int * all_id = allocint (num_pos);
      l = m = 0;
      for (i=0; i<this_reader -> num_sym_pos; i++)
      {
        for (j=0; j<3; j++)
        {
          tmp_pos = g_strdup_printf ("%s", this_reader -> sym_pos[i][j]);
          for (k=0; k<3; k++)
          {
            spgpos[j][k] = get_val_from_wyckoff (vect_comp[k], this_reader -> sym_pos[i][j]);
          }
          if (tmp_pos)
          {
            spgpos[j][3] = get_value_from_pos (tmp_pos);
            g_free (tmp_pos);
            tmp_pos = NULL;
          }
          else
          {
            spgpos[j][3] = 0.0;
          }
        }
        pos_mat = mat4 (spgpos[0][0], spgpos[0][1], spgpos[0][2], spgpos[0][3],
                        spgpos[1][0], spgpos[1][1], spgpos[1][2], spgpos[1][3],
                        spgpos[2][0], spgpos[2][1], spgpos[2][2], spgpos[2][3],
                        0.0, 0.0, 0.0, 1.0);
        for (j=0; j<num_pos; j++)
        {
          f_pos = vec3 (cryst_pos[j][0], cryst_pos[j][1], cryst_pos[j][2]);
          f_pos = m4_mul_coord (pos_mat, f_pos);
          c_pos = m4_mul_coord (this_reader -> lattice.box[0].frac_to_cart, f_pos);
          all_pos[l].x = c_pos.x;
          all_pos[l].y = c_pos.y;
          all_pos[l].z = c_pos.z;
          all_origin[l] = j;
          save_it = TRUE;
          at.x = all_pos[l].x;
          at.y = all_pos[l].y;
          at.z = all_pos[l].z;
          for (k=0; k<l; k++)
          {
            bt.x = all_pos[k].x;
            bt.y = all_pos[k].y;
            bt.z = all_pos[k].z;
            dist = distance_3d (active_cell, 0, & at, & bt);
            if (dist.length < 0.1)
            {
              dist_message = TRUE;
              save_it = FALSE;
              break;
            }
          }
          save_pos[l] = save_it;
          l ++;
          if (save_it)
          {
            all_id[j] ++;
            m ++;
          }
        }
      }
      double prob;
      gboolean pick_it;
      gboolean ** taken_pos = g_malloc0 (num_pos*sizeof*taken_pos);
      int ** site_lot = g_malloc0 (num_pos*sizeof*site_lot);
      clock_t CPU_time;
      int tot_pos = 0;
      for (i=0; i<num_pos; i++)
      {
        taken_pos[i] = allocbool(all_id[i]);
        site_lot[i] = allocint(all_id[i]);
        for (j=0; j<cif_pos[i]; j++)
        {
          u = occ_pos[i][j]*all_id[i];
          if (u < 1.0 && tot_pos < all_id[i]) u = 1.0;
          k = lot_pos[i][j];
          l = 0;
          while (l < (int)u)
          {
            CPU_time = clock ();
            m = (CPU_time - (j+17)*all_id[i]);
            prob = random3_(& m);
            m = round (prob * (all_id[i]-1));
            pick_it = ! taken_pos[i][m];
            if (pick_it)
            {
              site_lot[i][m] = k;
              taken_pos[i][m] = TRUE;
              l ++;
              tot_pos ++;
            }
          }
        }
      }
      crystal_data * cryst = allocate_crystal_data (tot_pos, this_reader -> nspec + this_reader -> object_to_insert);
      i = 0;
      int * cryst_lot = allocint (cryst -> objects);
      int * from_origin = allocint (num_pos);
      for (j=0; j<max_pos; j++)
      {
        if (save_pos[j])
        {
          k = all_origin[j];
          l = from_origin[k];
          if (taken_pos[k][l])
          {
            cryst -> coord[i] = g_malloc0(sizeof*cryst -> coord[i]);
            cryst -> coord[i][0].x = all_pos[j].x;
            cryst -> coord[i][0].y = all_pos[j].y;
            cryst -> coord[i][0].z = all_pos[j].z;
            cryst -> pos_by_object[i] = 1;
            cryst_lot[i] = site_lot[k][l];
            if (cryst_lot[i] < 0)
            {
              cryst -> at_by_object[i] = get_atomic_object_by_origin (cif_object, - cryst_lot[i] - 1, 0) -> atoms;
            }
            else
            {
              cryst -> at_by_object[i] = 1;
            }
            i ++;
          }
          from_origin[k] ++;
        }
      }
      g_free (site_lot);
      g_free (all_origin);
      g_free (from_origin);
      g_free (all_pos);
      g_free (save_pos);
      g_free (taken_pos);
      g_free (all_id);
      i = 0;
      for (j=0; j<cryst -> objects; j++)
      {
        i += cryst -> at_by_object[j] * cryst -> pos_by_object[j];
      }
      active_project -> natomes = i;
      allocatoms (active_project);
      atomic_object * c_obj;
      int * spec_num = allocint (120);
      i = 0;
      for (j=0; j<cryst -> objects; j++)
      {
        if (cryst_lot[j] < 0)
        {
          k = - cryst_lot[j] - 1;
          c_obj = get_atomic_object_by_origin (cif_object, k, 0);
          for (l=0; l<c_obj -> atoms; l++)
          {
            m = c_obj -> at_list[l].sp;
            n = c_obj -> old_z[m];
            spec_num[n] ++;
            active_project -> atoms[0][i].sp = n;
            active_project -> atoms[0][i].x = cryst -> coord[j][0].x + c_obj -> at_list[l].x;
            active_project -> atoms[0][i].y = cryst -> coord[j][0].y + c_obj -> at_list[l].y;
            active_project -> atoms[0][i].z = cryst -> coord[j][0].z + c_obj -> at_list[l].z;
            i ++;
          }
        }
        else
        {
          k = (int)this_reader -> z[cryst_lot[j]];
          spec_num[k] ++;
          active_project -> atoms[0][i].sp = k;
          active_project -> atoms[0][i].x = cryst -> coord[j][0].x;
          active_project -> atoms[0][i].y = cryst -> coord[j][0].y;
          active_project -> atoms[0][i].z = cryst -> coord[j][0].z;
          i ++;
        }
      }
      g_free (this_reader -> nsps);
      int * tmp_nsps = allocint (120);
      int * tmp_spid = allocint (120);
      i = 0;
      for (j=0; j<120; j++)
      {
        if (spec_num[j])
        {
          tmp_nsps[i] = spec_num[j];
          tmp_spid[j] = i;
          i++;
        }
      }
      this_reader -> nspec = i;
      this_reader -> nsps = allocint (i);
      for (i=0; i<this_reader -> nspec; i++) this_reader -> nsps[i] = tmp_nsps[i];
      g_free (tmp_nsps);
      g_free (this_reader -> z);
      this_reader -> z = allocdouble(i);
      i = 0;
      for (j=0; j<120; j++)
      {
        if (spec_num[j])
        {
          this_reader -> z[i] = (double)j;
          i ++;
        }
      }
      for (i=0; i<active_project -> natomes; i++)
      {
        j = active_project -> atoms[0][i].sp;
        k = tmp_spid[j];
        active_project -> atoms[0][i].sp = k;
      }
      g_free (tmp_spid);
      if (low_occ)
      {
        add_reader_info ("The crystal will be created however some objects might be missing,\n"
                         "Occupancy is too low compared to the number of site(s) per cell.\n\n"
                         "<b>To build a crystal matching the defined occupancy</b>:\n"
                         "\t <b>1)</b> If you are trying to read a CIF file, use the crystal builder instead.\n"
                         "\t <b>2)</b> Modify the occupancy set-up to 'Completely random'.\n"
                         "\t <b>3)</b> Increase the number of unit cells up to get rid of this message.\n\n", 1);
      }
      if (dist_message)
      {
        add_reader_info ("Object(s) at equivalent positions have been removed\n"
                         "to ensure the consistency of the model\n"
                         "when using <b>P</b>eriodic <b>B</b>oundary <b>C</b>onditions\n ", 1);
      }
    }
  }
  else
  {
    // No coordinates found
#ifdef DEBUG
    g_debug ("CIF:: Impossible to retrieve atomic coordinates !");
#endif
    res = 2;
  }
  return res;
}
