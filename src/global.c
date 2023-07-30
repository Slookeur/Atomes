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
* This file: 'global.c'
*
*  Contains:
*

 - Global variables declarations
 - Convenience global subroutines

*
*  List of subroutines:

  int * allocint (int  val);
  int ** allocdint (int xal, int yal);
  int *** alloctint (int xal, int yal, int zal);
  int **** allocqint (int wal, int xal, int yal, int zal);
  int * duplicate_int (int num, int * old_val);

  float * allocfloat (int  val);
  float ** allocdfloat (int xal, int yal);
  float *** alloctfloat (int xal, int yal, int zal);
  float * duplicate_float (int num, float * old_val);

  double * allocdouble (int val);
  double ** allocddouble (int xal, int yal);
  double *** alloctdouble (int xal, int yal, int zal);
  double **** allocqdouble (int wal, int xal, int yal, int zal);
  double * duplicate_double (int num, double * old_val);
  double get_calc_time (struct timespec start, struct timespec stop);

  gboolean * allocbool (int  val);
  gboolean ** allocdbool (int xal, int yal);
  gboolean *** alloctbool (int xal, int yal, int zal);
  gboolean * duplicate_bool (int num, gboolean * old_val);

  gchar ** duplicate_strings (int num, gchar ** old_val);
  gchar * calculation_time (gboolean modelv, double ctime);

*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>

#define NCALCS 12
#define NGRAPHS 10
#define OGL_STYLES 6
#define FILLED_STYLES 4
#define NCFORMATS 11
#define NDOTS 8

#define BILLION  1000000000L;

#ifdef G_OS_WIN32
gchar * PACKAGE_PREFIX = NULL;
gchar * PACKAGE_LIB_DIR = NULL;
gchar * PACKAGE_DATA_DIR = NULL;
gchar * PACKAGE_LOCALE_DIR = NULL;
gchar * PACKAGE_IMP = NULL;
gchar * PACKAGE_CON = NULL;
gchar * PACKAGE_IMG = NULL;
gchar * PACKAGE_PDF = NULL;
gchar * PACKAGE_SVG = NULL;
gchar * PACKAGE_EPS = NULL;
gchar * PACKAGE_PNG = NULL;
gchar * PACKAGE_JPG = NULL;
gchar * PACKAGE_BMP = NULL;
gchar * PACKAGE_TIFF = NULL;
gchar * PACKAGE_VOID = NULL;
gchar * PACKAGE_GR = NULL;
gchar * PACKAGE_SQ = NULL;
gchar * PACKAGE_BD = NULL;
gchar * PACKAGE_AN = NULL;
gchar * PACKAGE_RI = NULL;
gchar * PACKAGE_CH = NULL;
gchar * PACKAGE_SP = NULL;
gchar * PACKAGE_MS = NULL;
gchar * PACKAGE_TD = NULL;
gchar * PACKAGE_MOL = NULL;
gchar * PACKAGE_OGL = NULL;
gchar * PACKAGE_OGLM = NULL;
gchar * PACKAGE_OGLC = NULL;
gchar * PACKAGE_PRO = NULL;
gchar * PACKAGE_SET = NULL;
gchar * PACKAGE_LOGO = NULL;
gchar * PACKAGE_LAGPL = NULL;
gchar * PACKAGE_LABOUT = NULL;
gchar * PACKAGE_DOTA = NULL;
gchar * PACKAGE_DOTB = NULL;
gchar * PACKAGE_DOTC = NULL;
gchar * PACKAGE_DOTD = NULL;
gchar * PACKAGE_DOTE = NULL;
gchar * PACKAGE_DOTF = NULL;
gchar * PACKAGE_DOTG = NULL;
gchar * PACKAGE_DOTH = NULL;
gchar * PACKAGE_DFBD = NULL;
gchar * PACKAGE_DFAN = NULL;
gchar * PACKAGE_DFDI = NULL;
gchar * PACKAGE_DFTD = NULL;
gchar * PACKAGE_DFIN = NULL;
gchar * PACKAGE_SGCP = NULL;
gchar * PACKAGE_SGCI = NULL;
gchar * PACKAGE_SGCF = NULL;
gchar * PACKAGE_SGHP = NULL;
gchar * PACKAGE_SGTR = NULL;
gchar * PACKAGE_SGTI = NULL;
gchar * PACKAGE_SGTP = NULL;
gchar * PACKAGE_SGOP = NULL;
gchar * PACKAGE_SGOI = NULL;
gchar * PACKAGE_SGOC = NULL;
gchar * PACKAGE_SGOF = NULL;
gchar * PACKAGE_SGMP = NULL;
gchar * PACKAGE_SGMI = NULL;
gchar * PACKAGE_SGTC = NULL;
#endif
gchar * ATOMES_CONFIG = NULL;
gchar * ATOMES_URL = "https://atomes.ipcms.fr";

gchar * mode_name[2]={"Analysis", "Edition"};
gchar * calc_img[NCALCS-2];
gchar * graph_img[NGRAPHS];
gchar * dots[NDOTS];
gchar * bravais_img[14];
gchar * ifield[8];
gchar * projfile = NULL;
char * ifbug="\nIf this is a bug please report it to:";

char * coord_files[NCFORMATS+1] = {"XYZ file",
                                   "XYZ file - NPT",
                                   "Chem3D file",
                                   "CPMD trajectory",
                                   "CPMD trajectory - NPT",
                                   "VASP trajectory",
                                   "VASP trajectory - NPT",
                                   "Protein Data Bank file",
                                   "Protein Data Bank file",
                                   "Crystallographic information",
                                   "DL-POLY HISTORY file",
                                   "ISAACS Project File"};

char * file_ext[NCFORMATS+1]={"xyz", "xyz", "c3d", "trj", "trj", "xdatcar", "xdatcar",
                              "pdb", "ent", "cif", "hist", "ipf"};

char * text_styles[OGL_STYLES] = {"Ball and stick",
                                  "Wireframe",
                                  "Spacefilled",
                                  "Spheres",
                                  "Cylinders",
                                  "Dots"};

char * text_filled[FILLED_STYLES] = {"Covalent radius",
                                     "Ionic radius",
                                     "van Der Waals radius",
                                     "In crystal radius"};

char * calc_name[NCALCS-2] = {"g(r)/G(r)",
                              "S(q) from FFT[g(r)]",
                              "S(q) from Debye equation",
                              "g(r)/G(r) from FFT[S(q)]",
                              "Bonds and angles",
                              "Ring statistics",
                              "Chain statistics",
                              "Spherical harmonics",
                              "Mean Squared Displacement",
                              "Bond valence"};

char * graph_name[NGRAPHS] = {"g(r)/G(r)",
                              "S(q) from FFT[g(r)]",
                              "S(q) from Debye equation",
                              "g(r)/G(r) from FFT[S(q)]",
                              "Bonds properties",
                              "Angle distributions",
                              "Ring statistics",
                              "Chain statistics",
                              "Spherical harmonics",
                              "Mean Squared Displacement"};

char * rings_type[5] = {"All (No rules)",
                        "King's",
                        "Guttman's",
                        "Primitive",
                        "Strong"};

char * untime[6] = {"fs",
                    "ps",
                    "ns",
                    "µs",
                    "ms",
                    " "};

gchar * workspacefile = NULL;

int nprojects = 0;
int activep = 0;
int activev = 0;
int activef = 0;
int inactep = 0;
int activew = 0;
int statusval;
// Loop Id for the dialogs
int dialog_id = -1;

int bonds_update = 0;
int frag_update = 0;
int mol_update = 0;

int tmp_pixels[2];
int * pasted_todo = NULL;
struct insert_object * copied_object = NULL;

double * xsk = NULL;

GMainLoop * Event_loop[5];

gboolean in_movie_encoding = FALSE;
gboolean newspace = TRUE;
gboolean reading_input;
gboolean tmp_adv_bonding[2];
gboolean column_label = FALSE;
gboolean check_label = TRUE;
gboolean object_motion = FALSE;
gboolean selected_status = FALSE;
gboolean silent_input = FALSE;

struct timespec start_time;
struct timespec stop_time;

double opac = 0.75;
double pi = 3.141592653589793238462643383279502884197;

gchar * edition_action_names[3] = {"edit.chemistry",
                                   "edit.periodicity",
                                   "edit.cutoffs"};

gchar * analyze_action_names[9] = {"analyze.gr",
                                   "analyze.sq",
                                   "analyze.sk",
                                   "analyze.gk",
                                   "analyze.bonds",
                                   "analyze.rings",
                                   "analyze.chains",
                                   "analyze.sp",
                                   "analyze.msd"};
GSimpleAction * edition_actions[3];
GSimpleAction * analyze_actions[9];

GtkApplication * AtomesApp = NULL;
GtkWidget * MainWindow = NULL;
GtkWidget * MainView = NULL;
GtkWidget * MainFrame[2];
GtkWidget * pop = NULL;
GtkWidget * curvetoolbox = NULL;
GtkWidget * progressbar = NULL;
GtkWidget * MainScrol[2];
GtkWidget * atomes_logo = NULL;
GtkWidget * calc_dialog = NULL;
GtkWidget * register_button = NULL;

GtkTextTag * tag = NULL;
GtkStatusbar * statusbar;

GdkPixbuf * THETD = NULL;
GdkPixbuf * THEMO = NULL;
GdkPixbuf * THEBD = NULL;
GdkPixbuf * SETTING = NULL;
GdkPixbuf * SETTINGS = NULL;
GdkPixbuf * OGL = NULL;
GdkPixbuf * OGLM = NULL;
GdkPixbuf * OGLC = NULL;
GdkPixbuf * RUN = NULL;

// Basic utility functions

/*
*  int StringLength (char * str)
*
*  Usage: return the length of a string
*
*  char * str : string to measure
* /
int StringLength (char * str)
{
  int na, nb;
  char c;

  for (na = 0; (c = * str) != '\0'; ++ str, na ++);
  -- str;
  for (nb = na; (c = * str) == ' '; -- str, nb --);
  return nb;
}
*/

/*
*  gboolean * allocbool (int  val)
*
*  Usage: allocate a gboolean * pointer
*
*  int  val : size of the pointer to allocate
*/
gboolean * allocbool (int  val)
{
  gboolean * var = NULL;

  var = g_malloc0 (val*sizeof*var);
  return var;
}

/*
*  gboolean ** allocdbool (int xal, int yal)
*
*  Usage: allocate a gboolean ** pointer
*
*  int xal : 1st dimension size of the pointer to allocate
*  int yal : 2nd dimension size of the pointer to allocate
*/
gboolean ** allocdbool (int xal, int yal)
{
  gboolean ** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocbool(yal);
  }
  return var;
}

/*
*  gboolean *** alloctbool (int xal, int yal, int zal)
*
*  Usage: allocate a gboolean *** pointer
*
*  int xal : 1st dimension size of the pointer to allocate
*  int yal : 2nd dimension size of the pointer to allocate
*  int zal : 3rd dimension size of the pointer to allocate
*/
gboolean *** alloctbool (int xal, int yal, int zal)
{
  gboolean *** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocdbool(yal, zal);
  }
  return var;
}

/*
*  int * allocint (int  val)
*
*  Usage: allocate an int * pointer
*
*  int val : size of the pointer to allocate
*/
int * allocint (int  val)
{
  int * var = NULL;

  var = g_malloc0 (val*sizeof*var);
  return var;
}

/*
*  int ** allocdint (int xal, int yal)
*
*  Usage: allocate an int ** pointer
*
*  int xal : 1st dimension size of the pointer to allocate
*  int yal : 2nd dimension size of the pointer to allocate
*/
int ** allocdint (int xal, int yal)
{
  int ** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocint(yal);
  }
  return var;
}

/*
*  int *** alloctint (int xal, int yal, int zal)
*
*  Usage: allocate an int *** pointer
*
*  int xal : 1st dimension size of the pointer to allocate
*  int yal : 2nd dimension size of the pointer to allocate
*  int zal : 3rd dimension size of the pointer to allocate
*/
int *** alloctint (int xal, int yal, int zal)
{
  int *** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocdint(yal, zal);
  }
  return var;
}

/*
*  int **** allocqint (int wal, int xal, int yal, int zal)
*
*  Usage: allocate an int **** pointer
*
*  int wal : 1st dimension size of the pointer to allocate
*  int xal : 2nd dimension size of the pointer to allocate
*  int yal : 3rd dimension size of the pointer to allocate
*  int zal : 4th dimension size of the pointer to allocate
*/
int **** allocqint (int wal, int xal, int yal, int zal)
{
  int **** var = NULL;
  int i;

  var = g_malloc (wal*sizeof*var);
  for ( i = 0 ; i < wal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = alloctint(xal, yal, zal);
  }
  return var;
}

/*
*  float * allocfloat (int  val)
*
*  Usage: allocate a float * pointer
*
*  int  val : size of the pointer to allocate
*/
float * allocfloat (int  val)
{
  float * var = NULL;

  var = g_malloc0 (val*sizeof*var);
  return var;
}

/*
*  float ** allocdfloat (int xal, int yal)
*
*  Usage: allocate a float ** pointer
*
*  int wal : 1st dimension size of the pointer to allocate
*  int xal : 2nd dimension size of the pointer to allocate
*/
float ** allocdfloat (int xal, int yal)
{
  float ** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocfloat(yal);
  }
  return var;
}

/*
*  float *** alloctfloat (int xal, int yal, int zal)
*
*  Usage: allocate a float *** pointer
*
*  int xal : 1st dimension size of the pointer to allocate
*  int yal : 2nd dimension size of the pointer to allocate
*  int zal : 3rd dimension size of the pointer to allocate
*/
float *** alloctfloat (int xal, int yal, int zal)
{
  float *** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocdfloat (yal, zal);
  }
  return var;
}


/*
*  double * allocdouble (int val)
*
*  Usage: allocate a double * pointer
*
*  int val : size of the pointer to allocate
*/
double * allocdouble (int val)
{
  double * var = NULL;

  var = g_malloc0 (val*sizeof*var);
  return var;
}

/*
*  double ** allocddouble (int xal, int yal)
*
*  Usage: allocate a double ** pointer
*
*  int xal : 1st dimension size of the pointer to allocate
*  int yal : 2nd dimension size of the pointer to allocate
*/
double ** allocddouble (int xal, int yal)
{
  double ** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocdouble (yal);
  }
  return var;
}

/*
*  double *** alloctdouble (int xal, int yal, int zal)
*
*  Usage: allocate a double *** pointer
*
*  int xal : 1st dimension size of the pointer to allocate
*  int yal : 2nd dimension size of the pointer to allocate
*  int zal : 3rd dimension size of the pointer to allocate
*/
double *** alloctdouble (int xal, int yal, int zal)
{
  double *** var = NULL;
  int i;

  var = g_malloc (xal*sizeof*var);
  for ( i = 0 ; i < xal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = allocddouble (yal, zal);
  }
  return var;
}

/*
*  double **** allocqdouble (int wal, int xal, int yal, int zal)
*
*  Usage: allocate a double **** pointer
*
*  int wal : 1st dimension size of the pointer to allocate
*  int xal : 2nd dimension size of the pointer to allocate
*  int yal : 3rd dimension size of the pointer to allocate
*  int zal : 4th dimension size of the pointer to allocate
*/
double **** allocqdouble (int wal, int xal, int yal, int zal)
{
  double **** var = NULL;
  int i;

  var = g_malloc (wal*sizeof*var);
  for ( i = 0 ; i < wal ; i ++ )
  {
    /* allocation d'un tableau de tableau */
    var[i] = alloctdouble (xal, yal, zal);
  }
  return var;
}

/*
*  gchar ** duplicate_strings (int num, gchar ** old_val)
*
*  Usage: copy a list of strings
*
*  int num          : number of elements in the list
*  gchar ** old_val : the list to copy
*/
gchar ** duplicate_strings (int num, gchar ** old_val)
{
  gchar ** new_val = g_malloc0 (num*sizeof*new_val);
  int i;
  for (i=0; i<num; i++) new_val[i] = g_strdup_printf ("%s", old_val[i]);
  return new_val;
}

/*
*  int * duplicate_int (int num, int * old_val)
*
*  Usage: copy a list of int
*
*  int num       : number of elements in the list
*  int * old_val : the list to copy
*/
int * duplicate_int (int num, int * old_val)
{
  int * new_val = allocint (num);
  int i;
  for (i=0; i<num; i++) new_val[i] = old_val[i];
  return new_val;
}

/*
*  gboolean * duplicate_bool (int num, gboolean * old_val)
*
*  Usage: copy a list of gboolean
*
*  int num       : number of elements in the list
*  int * old_val : the list to copy
*/
gboolean * duplicate_bool (int num, gboolean * old_val)
{
  gboolean * new_val = allocbool (num);
  int i;
  for (i=0; i<num; i++) new_val[i] = old_val[i];
  return new_val;
}

/*
*  float * duplicate_float (int num, float * old_val)
*
*  Usage: copy a list of float
*
*  int num       : number of elements in the list
*  int * old_val : the list to copy
*/
float * duplicate_float (int num, float * old_val)
{
  float * new_val = allocfloat (num);
  int i;
  for (i=0; i<num; i++) new_val[i] = old_val[i];
  return new_val;
}

/*
*  double * duplicate_double (int num, double * old_val)
*
*  Usage: copy a list of double
*
*  int num       : number of elements in the list
*  int * old_val : the list to copy
*/
double * duplicate_double (int num, double * old_val)
{
  double * new_val = allocdouble (num);
  int i;
  for (i=0; i<num; i++) new_val[i] = old_val[i];
  return new_val;
}

/*
*  double get_calc_time (struct timespec start, struct timespec stop)
*
*  Usage: get calculation time in s
*
*  struct timespec start : the initial time
*  struct timespec stop  : the final time
*/
double get_calc_time (struct timespec start, struct timespec stop)
{
  return (double)(stop.tv_sec - start.tv_sec) + (double)(stop.tv_nsec - start.tv_nsec)/BILLION;
}

/*
*  gchar * calculation_time (gboolean modelv, double ctime)
*
*  Usage: get calculation time, human readable
*
*  gboolean modelv : was an analysis performed ?
*  double ctime    : the calculation time
*/
gchar * calculation_time (gboolean modelv, double ctime)
{
  int i, j, k;
  gchar * t_string = (modelv) ? "\n \tAnalysis was performed in: ": "";
  if (ctime < 60.0)
  {
    return g_strdup_printf ("%s%f s", t_string, ctime);
  }
  else if (ctime < 3600.0)
  {
    i = ctime/60.0;
    return g_strdup_printf ("%s%d m %f s", t_string, i, ctime-i*60.0);
  }
  else if (ctime < 86400.0)
  {
    // Really ?!
    i = ctime/3600.0;
    j = (ctime - i*3600.0)/60.0;
    return g_strdup_printf ("%s%d h %d m %f s", t_string, i, j, ctime-i*3600.0-j*60.0);
  }
  else
  {
    // Seriously ?!
    i = ctime/86400.0;
    j = (ctime - i*86400.0)/3600.0;
    k = (ctime - i*86400.0 - j*3600.0)/60.0;
    return g_strdup_printf ("%s%d d %d h %d m %f s", t_string, i, j, k, ctime-i*86400.0-j*3600.0-k*60.0);
  }
}
