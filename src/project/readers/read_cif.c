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
#include "bind.h"
#include "interface.h"
#include "project.h"
#include "cbuild_edit.h"
#include "readers.h"
#include <ctype.h>
#include <omp.h>

extern int get_atom_id_from_periodic_table (atom_search * asearch);
extern double get_z_from_periodic_table (gchar * lab);
extern void get_origin (space_group * spg);
extern void compute_lattice_properties (cell_info * cell);
extern int read_space_group (builder_edition * cbuilder, int spg);
extern gchar * wnpos[3];
extern void get_wyck_char (float val, int ax, int bx);
extern space_group * duplicate_space_group (space_group * spg);
extern int build_crystal (gboolean visible, struct project * this_proj, gboolean to_wrap, gboolean show_clones, cell_info * cell, GtkWidget * widg);
extern void sort (int dim, int * tab);

FILE * cifp;
char * line_ptr;
int * keylines = NULL;
int cif_loop_id;

cif_file * this_cif = NULL;

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

gchar * get_cif_word (gchar * mot)
{
  gchar * word = substitute_string (mot, "\n", NULL);
  word = substitute_string (word, "\r", NULL);
  return word;
}

float get_atom_coord (gchar * line, int mid)
{
  gchar * co_line = g_strdup_printf ("%s", line);
  char * co_word = strtok_r (co_line, " ", & line);
  int i;
  for (i=0; i<mid-1; i++)
  {
    co_word = strtok_r (NULL, " ", & line);
  }
  double v = atof(get_cif_word(co_word));
  co_line = NULL;
  co_word = NULL;
  return v;
}

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
atom_search * cif_search;

G_MODULE_EXPORT void select_cif_species (GtkButton * but, gpointer data)
{
  int i, j;
  i = GPOINTER_TO_INT(data);
  j = get_atom_id_from_periodic_table (cif_search);
  gchar * stra, * strb;
  if (! j)
  {
    stra = g_strdup_printf ("Not picked yet !");
    strb = g_strdup_printf (DELETEB);
    this_reader -> lmislab[i] = 0;
    this_reader -> stolab ++;
  }
  else
  {
    stra = g_strdup_printf ("%s", periodic_table_info[j].name);
    strb = g_strdup_printf (APPLY);
    this_reader -> lmislab[i] = j;
    this_reader -> stolab --;
  }
  set_image_from_icon_name (img_cif[i], strb);
  gtk_button_set_label (but, stra);
  gtk_widget_set_size_request (GTK_WIDGET(but), 150, -1);
  g_free (stra);
  g_free (strb);
}

gboolean get_missing_z_from_user ()
{
  this_reader -> lmislab = allocint (this_reader -> stolab);
  cif_search = g_malloc0 (sizeof*cif_search);
  GtkWidget * info = dialogmodal ("Error while reading CIF file", GTK_WINDOW(MainWindow));
  GtkWidget * vbox, * hbox;
  gchar * str;
  vbox = dialog_get_content_area (info);
  gchar * labpick = "<b>To continue and build the crystal according to the information of the CIF file\n"
                    "it is required to provide a suitable value for each and every missing parameter(s).</b>"
                    "\n\nPlease select an atom type for the following object(s):";
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, markup_label (labpick, 200, -1, 0.5, 0.5), FALSE, FALSE, 10);
  img_cif = g_malloc0(this_reader -> tolab*sizeof*img_cif);
  GtkWidget * but;
  int j;
  for (j=0; j<this_reader -> tolab; j++)
  {
    hbox = create_hbox(0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 5);
    str = g_strdup_printf ("Type N°%d:\t<b>%s</b>", j+1, this_reader -> label[j]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label(str, 150, -1, 0.0, 0.5), FALSE, FALSE, 20);
    g_free (str);
    img_cif[j] = stock_image (DELETEB);
    but = create_button ("Not picked yet !", IMG_NONE, NULL, 150, -1, GTK_RELIEF_NORMAL, G_CALLBACK(select_cif_species), GINT_TO_POINTER(j));
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, but, FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, img_cif[j], FALSE, FALSE, 30);
  }

  gchar * endpick = "In case of a molecule: insert an extra type of atom and run a substitution afterwards.";
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, markup_label (endpick, 200, -1, 0.5, 0.5), FALSE, FALSE, 10);
  run_this_gtk_dialog (info, G_CALLBACK(run_destroy_dialog), NULL);
  g_free (img_cif);
  g_free (cif_search);
  destroy_this_widget (info);
  return (! this_reader -> stolab) ? TRUE : FALSE;
}

#ifndef OPENMP
void file_get_to_line (int line_id)
{
  int i;
  tail = head;
  for (i=0; i<line_id; i++) tail = tail -> next;
}
#endif

int cif_get_value (gchar * kroot, gchar * keyw, int linec, int lstart, gchar ** cif_word,
                   gboolean rec_val, gboolean all_ligne, gboolean in_loop)
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
          this_word = strtok_r (NULL, " ", & saved_line);
          if (! this_word)
          {
            if (rec_val || all_ligne)
            {
              str = g_strdup_printf ("Wrong file format: searching for <b>%s</b> - error at line <b>%d</b> !", keyw, i+1);
              add_reader_info (str);
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
              add_reader_info (str);
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

int num_key (gchar * keyw, int linec)
{
  int res = 0;
  int i;
  keylines = NULL;
#ifdef OPENMP
  int numth = omp_get_max_threads ();
  gchar * saved_line;
  #pragma omp parallel for num_threads(numth) private(i,this_line,saved_line,this_word) shared(this_reader,res,coord_line,keylines)
  for (i=0; i<linec; i++)
  {
    this_line = g_strdup_printf ("%s", coord_line[i]);
    saved_line = g_strdup_printf ("%s", this_line);
    this_word = strtok_r (this_line, " ", & saved_line);
    while (this_word)
    {
      if (g_strcmp0(get_cif_word(this_word), keyw) == 0)
      {
        #pragma omp critical
        {
          if (keylines)
          {
            keylines = g_realloc(keylines, (res+1)*sizeof*keylines);
          }
          else
          {
            keylines = allocint (1);
          }
          g_debug ("Key= %s, line= %d\n", keyw, i);
          keylines[res] = i+1;
          res ++;
        }
      }
      this_word = strtok_r (NULL, " ", & saved_line);
    }
  }
  sort (res, keylines);
#else
  tail = head;
  i = 0;
  while (tail)
  {
    i ++;
    this_line = g_strdup_printf ("%s", tail -> line);
    this_word = strtok (this_line, " ");
    while (this_word)
    {
      if (g_strcmp0(get_cif_word(this_word), keyw) == 0)
      {
        if (keylines)
        {
          keylines = g_realloc(keylines, (res+1)*sizeof*keylines);
        }
        else
        {
          keylines = allocint (1);
        }
        keylines[res] = i+1;
        res ++;
      }
    }
    tail = tail -> next;
  }
#endif
  return res;
}

void check_for_to_lab (int ato, gchar * stlab)
{
  int i, j;
  j = -1;
  for (i=0; i<this_reader -> stolab; i++)
  {
    if (g_strcmp0(this_reader -> label[i], stlab) == 0)
    {
      j = i;
      break;
    }
  }
  if (this_reader -> mislab)
  {
    this_reader -> mislab = g_realloc (this_reader -> mislab, (this_reader -> tolab+1)*sizeof*this_reader -> mislab);
    this_reader -> smislab = g_realloc (this_reader -> smislab, (this_reader -> tolab+1)*sizeof*this_reader -> smislab);
  }
  else
  {
    this_reader -> mislab = g_malloc0 (1*sizeof*this_reader -> mislab);
    this_reader -> smislab = g_malloc0 (1*sizeof*this_reader -> smislab);
  }
  this_reader -> mislab[this_reader -> tolab] = ato;
  this_reader -> smislab[this_reader -> tolab] = (j < 0) ? this_reader -> stolab : j;
  this_reader -> tolab ++;
  if (j < 0)
  {
    if (this_reader -> label)
    {
      this_reader -> label = g_realloc (this_reader -> label, (this_reader -> stolab+1)*sizeof*this_reader -> label);
    }
    else
    {
      this_reader -> label = g_malloc0 (1*sizeof*this_reader -> label);
    }
    this_reader -> label[this_reader -> stolab] = g_strdup_printf ("%s", stlab);
    this_reader -> stolab ++;
  }
}

gboolean cif_file_get_atoms_data (int lin, int cid[8])
{
  int i, j, k, l;
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
    str = get_atom_label (cline, (cid[0] != -1) ? cid[0] : cid[1]);
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
    str = get_atom_label (cline, (cid[0] != -1) ? cid[0] : cid[1]);
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
    done = get_missing_z_from_user ();
    if (done)
    {
      for (i=0; i<this_reader -> tolab; i++)
      {
        j = this_reader -> mislab[i];
        k = this_reader -> smislab[i];
        l = this_reader -> lmislab[k];
        check_for_species ((double)l, j);
      }
    }
  }
  return done;
}

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

int get_loop_line_for_key (gchar * key_a, gchar * key_b, int linec)
{
  int i;
  gchar * str;
  i = cif_get_value (key_a, key_b, linec, 0, & str, FALSE, FALSE, FALSE);
  return (i) ? get_loop_line_id (linec, i) : 0;
}

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
    add_reader_info ("<b>Atomic coordinates</b>: impossible to find atomic label(s) ...");
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
      add_reader_info (str);
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
        add_reader_info (str);
        g_free (str);
      }
    }
    if (j < 3)
    {
      add_reader_info ("<b>Atomic coordinates</b>: no complete cartesian coordinates !");
      add_reader_info ("<b>Atomic coordinates</b>: no complete fractional coordinates !");
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
                add_reader_info ("<b>Atomic coordinates</b>: a site was found to have an occupancy > 1.0 !");
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
    if (this_reader -> lattice.sp_group -> id > 2 && this_reader -> lattice.sp_group -> id< 16)
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
            add_reader_info (str);
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
            add_reader_info (str);
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
            add_reader_info (str);
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
    add_reader_info (str);
    g_free (str);
  }
  if (this_reader -> setting < 0) this_reader -> setting = 0;
  return (spg) ? (j) ? j : spg : j;
}

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
      add_reader_info (str);
      g_free (str);
      return FALSE;
    }
    this_reader -> lattice.box[0].param[0][i] = atof(str);
#ifdef DEBUG
    g_debug ("CIF:: box[0][%d]= %f", i, this_reader -> lattice.box[0].param[0][i]);
#endif
    if (! cif_get_value ("_cell", cellangs[i], linec, 0, & str, TRUE, FALSE, FALSE))
    {
      str = g_strdup_printf ("<b>Lattice parameters</b>: impossible to retrieve the '%s' parameter !", box_prop[1][i]);
      add_reader_info (str);
      g_free (str);
      return FALSE;
    }
    this_reader -> lattice.box[0].param[1][i] = atof(str);
#ifdef DEBUG
    g_debug ("CIF:: box[1][%d]= %f", i, this_reader -> lattice.box[0].param[1][i]);
#endif
  }
  this_reader -> lattice.ltype = 0;
  compute_lattice_properties (& this_reader -> lattice);
  for (i=0; i<3; i++) this_reader -> lattice.cextra[i] = 1;
  return TRUE;
}

gboolean cif_get_space_group (int linec)
{
  gchar * symkey[2] = {"int_tables_number", "group_it_number"};
  gchar * str = NULL;
  int spg = 0;
  int i, j, k, l;
  for (i=0; i<2; i++)
  {
    if (cif_get_value ("_symmetry", symkey[i], linec, 0, & str, TRUE, FALSE, FALSE))
    {
      spg = (int)atof(str);
      break;
    }
  }
  if (! spg)
  {
    if (cif_get_value ("_space_group", "it_number", linec, 0, & str, TRUE, FALSE, FALSE))
    {
      spg = (int)atof(str);
    }
  }
  gchar * hmkey = NULL;
  if (! cif_get_value ("_symmetry", "space_group_name_h-m", linec, 0, & hmkey, FALSE, TRUE, FALSE))
  {
    cif_get_value ("_space_group", "name_h-m_alt", linec, 0, & hmkey, FALSE, TRUE, FALSE);
  }
  if (! hmkey && ! spg)
  {
    add_reader_info ("<b>Space group</b>: no space group and no H-M symbol found !");
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
      add_reader_info ("<b>Space group</b>: no space group found, unknown H-M symbol !");
#ifdef DEBUG
      g_debug ("CIF:: No space group found, unknown H-M symbol !");
#endif
    }
    else if (spg && ! i)
    {
      str = g_strdup_printf ("<b>Space group</b>: space group and H-M symbol do not match !\n"
                             " CIF file space group: <b>%s</b>, CIF file H-M symbol: <b>%s</b>", groups[spg-1], hmkey);
      add_reader_info (str);
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
  gboolean res = TRUE;
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
              res = FALSE;
              break;
            }
            if (str[0] == '-')
            {
              k = (int) atof (g_strdup_printf ("%c", str[2]));
              str = g_strdup_printf ("%c%c", str[0], str[1]);
            }
            else
            {
              k = (int) atof (g_strdup_printf ("%c", str[1]));
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
                if (j < this_reader -> setting) add_reader_info ("<b>Space group</b>: ambiguous space group setting !");
                this_reader -> setting = j;
                break;
              }
            }
          }
          else if (i < 36)
          {
            if (spg < 16 || spg > 74)
            {
              res = FALSE;
              break;
            }
            k = 0;
            if (str[0] == '1' || str[0]=='2')
            {
              k = (int) atof(g_strdup_printf ("%c", str[0]));
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
                  add_reader_info ("<b>Space group</b>: ambiguous space group setting !");
                }
                this_reader -> setting = j;
                l = 1;
                break;
              }
            }
            if (! l) add_reader_info ("<b>Space group</b>: ambiguous space group setting !");
          }
          else if (i < 38)
          {
            if (spg < 75 || (spg > 142 && spg < 195))
            {
              res = FALSE;
              break;
            }
            j = i - 36;
            if (j < this_reader -> setting) add_reader_info ("<b>Space group</b>: ambiguous space group setting !");
            this_reader -> setting = j;
          }
          else
          {
            if (spg < 143 || spg > 165)
            {
              res = FALSE;
              break;
            }
            j = i - 38;
            if (j < this_reader -> setting) add_reader_info ("<b>Space group</b>: ambiguous space group setting !");
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
                               spg, groups[spg-1], setc[this_reader -> setting], setc[!this_reader -> setting]);
        add_reader_info (str);
        g_free (str);
        this_reader -> setting = ! this_reader -> setting;
      }
    }
  }
  this_reader -> lattice.sp_group -> sid = this_reader -> setting;
  get_origin (this_reader -> lattice.sp_group);
  return res;
}

int open_cif_file (int linec)
{
  int res;
  int i, j;
  if (cif_get_cell_data (linec))
  {
    if (cif_get_space_group (linec))
    {
#ifdef DEBUG
      g_debug ("CIF:: SP setting:: %d, name= %s", this_reader -> setting+1, this_reader -> lattice.sp_group -> settings[this_reader -> setting].name);
#endif
      if (this_reader -> lattice.sp_group) get_origin (this_reader -> lattice.sp_group);
    }
    else
    {
      // No space group found
#ifdef DEBUG
      g_debug ("CIF:: Impossible to retrieve space group information !");
#endif
    }
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
