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
* This file: 'interface.c'
*
*  Contains:
*

 -

*
*  List of subroutines:

  int dummy_ask_ (char * question);
  int iask (char * question, char * lab, int id, GtkWidget * win);

  gboolean ask_yes_no (gchar * title, gchar * text, int type, GtkWidget * widg);

  G_MODULE_EXPORT gboolean leaving_question (GtkWindow * widget, gpointer data);
  G_MODULE_EXPORT gboolean leaving_question (GtkWidget * widget, GdkEvent * event, gpointer data);

  gchar * exact_name (gchar * name);
  gchar * cask (char * question,  char * lab, int id, char * old, GtkWidget * win);
  gchar * textcolor (int i);
  gchar * env_name (struct project * this_proj, int g, int s, int f, GtkTextBuffer * buffer);

  void show_web (GtkWidget * dialog, int id);
  void show_info (char * information, int val, GtkWidget * win);
  void show_warning (char * warning, GtkWidget * win);
  void show_warning_ (char * warning, char * sub, char * tab);
  void show_error (char * error, int val, GtkWidget * win);
  void show_error_ (char * error, char * sub, char * tab);
  void init_data_ (int * nats, int * nspc, int * stps, int * cid);
  void print_info  (gchar * str, gchar * stag, GtkTextBuffer * buffer);
  void lattice_info_ (int * bid, double * volume, double * density,
                       double dvects[3][3], double rvects[3][3], double mod[3], double ang[3],
                       double f_to_c[3][3], double c_to_f[3][3]);
  void send_chem_info_ (int prop[active_project -> nspec]);
  void update_after_calc (int calc);

  G_MODULE_EXPORT void create_about_dialog (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void run_yes_no (GtkDialog * dial, gint response_id, gpointer data);
  G_MODULE_EXPORT void on_answer_changed (GtkWidget * widg, gpointer data);
  G_MODULE_EXPORT void run_iask (GtkDialog * iask, gint response_id, gpointer data);
  G_MODULE_EXPORT void run_cask (GtkDialog * cask, gint response_id, gpointer data);

  GtkWidget * addweb (int id);
  GtkWidget * show_pop (char * pop, GtkWidget * pwin);

*/

#include "global.h"
#include "bind.h"
#include "callbacks.h"
#include "project.h"
#include "curve.h"
#include "affero.h"

extern gchar * substitute_string (gchar * init, gchar * o_motif, gchar * n_motif);

GtkWidget * answer;

/* Old version to make url/email in about dialog clickable */
/* void about_dialog_handle_url (GtkAboutDialog * dialog, const gchar * link, gpointer data)
{
  GtkWidget * error_dialog;
  gchar * url, * escaped;
  g_return_if_fail (GTK_IS_ABOUT_DIALOG (dialog));
  g_return_if_fail (link != NULL);

/ * simple check if this is an email address * /
  if (! g_str_has_prefix (link, "mailto:") && strchr (link, '@') != NULL)
  {
    escaped = g_uri_escape_string (link, NULL, FALSE);
    url = g_strdup_printf ("mailto:%s", escaped);
    g_free (escaped);
  }
  else
  {
    url = g_strdup_printf ("%s", link);
  }

#ifdef G_OS_WIN32
 HINSTANCE h;
 h = ShellExecute (NULL, "open", url, NULL, NULL, SW_SHOWNORMAL);
 if ((int)h <= 32)
#else
#ifdef GTK4
  // gtk_show_uri (GTK_WINDOW (dialog), url, gtk_event_controller_get_current_event_time ( , ));
#else
  GError * error = NULL;
  gtk_show_uri_on_window (GTK_WINDOW (dialog), url, gtk_get_current_event_time (), & error);
  if (error != NULL)
#endif
#endif
  {
    gchar * uri = g_strdup_printf ("Failed to open link:\n%s", url);
    error_dialog = gtk_message_dialog_new (GTK_WINDOW (dialog),
                                           GTK_DIALOG_DESTROY_WITH_PARENT,
                                           GTK_MESSAGE_ERROR, GTK_BUTTONS_OK,
                                           "Impossible to open link: ");
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), "%s", uri);
    run_this_gtk_dialog (error_dialog, G_CALLBACK(run_destroy_dialog), NULL);
    g_free (uri);
  }
  g_free (url);
} */

/*
*  GtkWidget * addweb (int id)
*
*  Usage: create a widget to present
*
*  int id : Add contact info (1) or not (0)
*/
GtkWidget * addweb (int id)
{
  GtkWidget * web;
  const gchar * contact = " Please contact me at:\n";
  const gchar * seb     = " Dr. Sébastien Le Roux ";
  GtkWidget * eseb;
  GtkWidget * vbox;
  GtkWidget * hbox;
  const gchar * or = "\n Or check the website for information\n";
  if (id)
  {
    web = gtk_link_button_new_with_label (ATOMES_URL, "Visit the project's website");
  }
  gchar * mailto = g_strdup_printf ("mailo:%s", PACKAGE_BUGREPORT);
  gchar * mailsh = g_strdup_printf ("<%s>",PACKAGE_BUGREPORT);
  eseb = gtk_link_button_new_with_label (mailto, mailsh);
  g_free (mailto);
  g_free (mailsh);
  vbox = create_vbox (BSEP);
  if (id) add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, gtk_label_new (contact), FALSE, FALSE, 0);
  hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, FALSE, FALSE, 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, gtk_label_new (seb), FALSE, FALSE, 10);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, eseb, FALSE, FALSE, 0);
  if (id)
  {
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, gtk_label_new (or), FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, web, FALSE, FALSE, 0);
  }
  return (vbox);
}

/*
*  G_MODULE_EXPORT void create_about_dialog (GtkWidget * widg, gpointer data)
*
*  Usage: create the about dialog
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void create_about_dialog (GtkWidget * widg, gpointer data)
{
  GtkWidget * aboutdialog;
#ifdef GTK4
  GdkPaintable * atomes_logo = gtk_image_get_paintable (GTK_IMAGE(gtk_image_new_from_file(PACKAGE_LAGPL)));
#else
  GdkPixbuf * atomes_logo = gdk_pixbuf_new_from_file (PACKAGE_LAGPL, NULL);
#endif
  const gchar *authors[] = {"Dr. Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>", "", NULL};
  const gchar * weblabel = "https://atomes.ipcms.fr";
  const gchar * comments = "Welcome to Atomes: visualization, analyzis, creation/edition and post-processing of atomistic models !";
  const gchar * copyrights = "Copyright © 2023 \nDr. Sébastien Le Roux";

  // gtk_about_dialog_set_url_hook (about_dialog_handle_url, NULL, NULL);
  // gtk_about_dialog_set_email_hook (about_dialog_handle_url, NULL, NULL);

  aboutdialog = gtk_about_dialog_new ();
  gtk_about_dialog_set_logo (GTK_ABOUT_DIALOG(aboutdialog), atomes_logo);
  gtk_about_dialog_set_version (GTK_ABOUT_DIALOG(aboutdialog), VERSION);
  gchar * str = g_strdup_printf ("%s", PACKAGE);
  gtk_about_dialog_set_program_name (GTK_ABOUT_DIALOG(aboutdialog), str);
  g_free (str);
  gtk_about_dialog_set_comments (GTK_ABOUT_DIALOG(aboutdialog), comments);
  gtk_about_dialog_set_website (GTK_ABOUT_DIALOG(aboutdialog), g_strdup_printf("%s", ATOMES_URL));
  gtk_about_dialog_set_website_label (GTK_ABOUT_DIALOG(aboutdialog), weblabel);
  gtk_about_dialog_set_authors (GTK_ABOUT_DIALOG(aboutdialog), authors);
  gtk_about_dialog_set_copyright (GTK_ABOUT_DIALOG(aboutdialog), copyrights);
  gtk_about_dialog_set_license (GTK_ABOUT_DIALOG(aboutdialog), affero_license);
  gtk_about_dialog_set_license_type (GTK_ABOUT_DIALOG(aboutdialog), GTK_LICENSE_CUSTOM);
  gtk_about_dialog_set_wrap_license (GTK_ABOUT_DIALOG(aboutdialog), FALSE);
  run_this_gtk_dialog (aboutdialog,  G_CALLBACK(destroy_this_dialog), NULL);
}

/*
*  void show_web (GtkWidget * dialog, int id)
*
*  Usage: add / show web information to widget
*
*  GtkWidget * dialog : the GtkWidget to modify
*  int id             : Add contact info (1) or not (0)
*/
void show_web (GtkWidget * dialog, int id)
{
  GtkWidget * box  = dialog_get_content_area (dialog);
  GtkWidget * theweb = addweb (id);
  show_the_widgets (theweb);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box, theweb, FALSE, FALSE, 0);
  show_the_widgets (box);
}

/*
*  void show_info (char * information, int val, GtkWidget * win)
*
*  Usage: add / show information message to widget
*
*  char * information : Message
*  int val            : Add contact info (1) or not (0)
*  GtkWidget * win    : the GtkWidget to modify
*/
void show_info (char * information, int val, GtkWidget * win)
{
  gchar * info=NULL;
  if (val < 0)
  {
    info = g_strdup_printf ("%s\n%s", information, ifbug);
  }
  else
  {
    info = g_strdup_printf ("%s", information);
  }

  GtkWidget * dialog = message_dialogmodal (info, "Information", GTK_MESSAGE_INFO, GTK_BUTTONS_OK, win);
  if (val != 0) show_web (dialog, (val < 0) ? 0 : val);
  run_this_gtk_dialog (dialog, G_CALLBACK(run_destroy_dialog), NULL);
  g_free (info);
}

/*
*  void show_warning (char * warning, GtkWidget * win)
*
*  Usage: show warning
*
*  char * warning  : Message
*  GtkWidget * win : Parent GtkWidget, if any
*/
void show_warning (char * warning, GtkWidget * win)
{
  GtkWidget * dialog = message_dialogmodal (warning,  "Warning", GTK_MESSAGE_WARNING, GTK_BUTTONS_OK, win);
  run_this_gtk_dialog (dialog, G_CALLBACK(run_destroy_dialog), NULL);
}

/*
*  void show_warning_ (char * warning, char * sub, char * tab)
*
*  Usage: show warning from Fortran90
*
*  char * warning : Message
*  char * sub     : Fortan90 subroutine
*  char * tab     : Fortran90 pointer
*/
void show_warning_ (char * warning, char * sub, char * tab)
{
  /* This function is called from fortran 90 */
  gchar * wtot=NULL;
  wtot = g_strdup_printf ("%s\n%s\n%s", warning, sub, tab);
  show_warning (wtot, MainWindow);
  g_free (wtot);
}

/*
*  void show_error (char * error, int val, GtkWidget * win)
*
*  Usage: show error message
*
*  char * error    : Message
*  int val         : Add contact info (1) or not (0)
*  GtkWidget * win : Parent GtkWidget, if any
*/
void show_error (char * error, int val, GtkWidget * win)
{
  gchar * etot=NULL;
  if (val)
  {
    etot = g_strdup_printf ("%s", error);
  }
  else
  {
    etot = g_strdup_printf ("%s\n%s", error, ifbug);
  }
  GtkWidget * dialog = message_dialogmodal (etot, "Error", GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, win);
  show_web (dialog, val);
  g_warning ("%s", etot);
  run_this_gtk_dialog (dialog, G_CALLBACK(run_destroy_dialog), NULL);
  g_free (etot);
}

/*
*  void show_error_ (char * error, char * sub, char * tab)
*
*  Usage: show error from Fortran90
*
*  char * error : Message
*  char * sub   : Fortan90 subroutine
*  char * tab   : Fortran90 pointer
*/
void show_error_ (char * error, char * sub, char * tab)
{
  /* This function is called from fortran 90 */
  gchar * etot=NULL;
  etot = g_strdup_printf ("%s\n\t%s\n\t%s", error, sub, tab);
  show_error (etot, 0, MainWindow);
  g_free (etot);
}

gboolean res_yes_no;

/*
*  G_MODULE_EXPORT void run_yes_no (GtkDialog * dial, gint response_id, gpointer data)
*
*  Usage: ask yes or no for something: running dialog
*
*  GtkDialog * dial : the GtkDialog sending the signal
*  gint response_id : the response id
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void run_yes_no (GtkDialog * dial, gint response_id, gpointer data)
{
  res_yes_no = (response_id == GTK_RESPONSE_YES) ? TRUE : FALSE;
  destroy_this_dialog (dial);
}

/*
*  gboolean ask_yes_no (gchar * title, gchar * text, int type, GtkWidget * widg)
*
*  Usage: ask yes or no for something: prepare dialog
*
*  gchar * title    : Title
*  gchar * text     : Message
*  int type         : the type of message window
*  GtkWidget * widg : the parent GtkWidget, if any
*/
gboolean ask_yes_no (gchar * title, gchar * text, int type, GtkWidget * widg)
{
  GtkWidget * dialog = message_dialogmodal (text, title, type, GTK_BUTTONS_YES_NO, widg);
  run_this_gtk_dialog (dialog, G_CALLBACK(run_yes_no), NULL);
  return res_yes_no;
}

/*
*  gchar * exact_name (gchar * name)
*
*  Usage: short cut to print string without spaces
*
*  gchar * name : the initial string
*/
gchar * exact_name (gchar * name)
{
  return substitute_string (name, " ", NULL);
}

/*
*  GtkWidget * show_pop (char * pop, GtkWidget * pwin)
*
*  Usage: display pop information window
*
*  char * pop       : Message
*  GtkWidget * pwin : Parent widget, if any
*/
GtkWidget * show_pop (char * pop, GtkWidget * pwin)
{
  GtkWidget * wpop = create_win ("Information", pwin, TRUE, FALSE);
  gtk_widget_set_size_request (wpop, 600, 80);
#ifdef GTK3
  gtk_window_set_position (GTK_WINDOW (wpop), GTK_WIN_POS_CENTER);
#endif
  GtkWidget * hpop = create_hbox (0);
  add_container_child (CONTAINER_WIN, wpop, hpop);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hpop, stock_image(EXECUTE), TRUE, TRUE, 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hpop, gtk_label_new (pop), TRUE, TRUE, 0);
  show_the_widgets (wpop);
  return (wpop);
}

#ifdef GTK4
/*
*  G_MODULE_EXPORT gboolean leaving_question (GtkWindow * widget, gpointer data)
*
*  Usage: Leaving atomes ?
*
*  GtkWindow * widget : the GtkWidget sending the signal
*  gpointer data      : the associated data pointer
*/
G_MODULE_EXPORT gboolean leaving_question (GtkWindow * widget, gpointer data)
#else
/*
*  G_MODULE_EXPORT gboolean leaving_question (GtkWidget * widget, GdkEvent * event, gpointer data)
*
*  Usage: Leaving atomes ?
*
*  GtkWidget * widget : the GtkWidget sending the signal
*  GdkEvent * event   : the GdkEvent triggering the signal
*  gpointer data      : the associated data pointer
*/
G_MODULE_EXPORT gboolean leaving_question (GtkWidget * widget, GdkEvent * event, gpointer data)
#endif
{
  if (ask_yes_no ("Leaving ?!", "Are you sure you want to quit ?", GTK_MESSAGE_QUESTION, MainWindow))
  {
    quit_gtk ();
  }
  else
  {
    gtk_widget_show (MainWindow);
  }
  return TRUE;
}

/*
*  int dummy_ask_ (char * question)
*
*  Usage: Ask to use dummy atoms or not from Fortran90
*
*  char * question : Message
*/
int dummy_ask_ (char * question)
{
  GtkWidget * dask = message_dialogmodal (question, "Parameter required", GTK_MESSAGE_INFO, GTK_BUTTONS_YES_NO, MainWindow);
  run_this_gtk_dialog (dask, G_CALLBACK(run_yes_no), NULL);
  return (res_yes_no) ? 0 : -1;
}

extern  gchar * field_init[3];
gchar * coord_type[3]={"Cartesian",
                       "Atomic units",
                       "Fractional"};
gchar * npt_type[4]={"A\tB\tC\t&#x3B1;\t&#x3B2;\t&#x263;",
                     "A\tB\tC\n&#x3B1;\t&#x3B2;\t&#x263;",
                     "a<sub>x</sub>\ta<sub>y</sub>\ta<sub>z</sub>\tb<sub>x</sub>\tb<sub>y</sub>\tb<sub>z</sub>\tc<sub>x</sub>\tc<sub>y</sub>\tc<sub>z</sub>",
                     "a<sub>x</sub>\ta<sub>y</sub>\ta<sub>z</sub>\nb<sub>x</sub>\tb<sub>y</sub>\tb<sub>z</sub>\nc<sub>x</sub>\tc<sub>y</sub>\tc<sub>z</sub>"};
gchar * npt_info[3]={"1 line by step, as many lines as MD steps",
                     "2 lines by step, twice as many lines as MD steps",
                     "3 lines by step, three times as many lines as MD steps"};
GtkWidget * answer_info;

/*
*  G_MODULE_EXPORT void on_answer_changed (GtkWidget * widg, gpointer data)
*
*  Usage: Handling the GtkComboBox in 'int iask'
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void on_answer_changed (GtkWidget * widg, gpointer data)
{
  int i, j;
  i = gtk_combo_box_get_active (GTK_COMBO_BOX(widg));
  j = (! i || i == 2) ? 0 : (i == 1) ? 1 : 2;
  gtk_label_set_text (GTK_LABEL(answer_info), npt_info[j]);
}

int res_int;

/*
*  G_MODULE_EXPORT void run_iask (GtkDialog * iask, gint response_id, gpointer data)
*
*  Usage: enter an integer value - running the dialog
*
*  GtkDialog * iask : the GtkDialog sending the signal
*  gint response_id : the response id
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void run_iask (GtkDialog * iask, gint response_id, gpointer data)
{
  int i = GPOINTER_TO_INT(data);
  gboolean done = FALSE;
  const gchar * riask;
  if (response_id == GTK_RESPONSE_OK)
  {
    if (i == 0 || i > 3)
    {
      riask = entry_get_text (GTK_ENTRY(answer));
      res_int = atof(riask);
      if (i > 4)
      {
        if (res_int > 0 && res_int < i+1)
        {
          done = TRUE;
          res_int --;
        }
      }
      else if (res_int > 0)
      {
        done = TRUE;
      }
    }
    else
    {
      done = TRUE;
      res_int = gtk_combo_box_get_active (GTK_COMBO_BOX(answer));
    }
  }
  if (done) destroy_this_dialog (iask);
}

/*
*  int iask (char * question, char * lab, int id, GtkWidget * win)
*
*  Usage: enter an integer value - prepare the dialog
*
*  char * question : Message
*  char * lab      : Text to use for label
*  int id          : the required parameter id
*  GtkWidget * win : the parent GtkWidget, if any
*/
int iask (char * question, char * lab, int id, GtkWidget * win)
{
  GtkWidget * iask;
  GtkWidget * vbox;
  GtkWidget * hboxa;
  GtkWidget * quest;
  int i;
  iask = message_dialogmodal (question, "Parameter Required", GTK_MESSAGE_INFO, GTK_BUTTONS_OK, win);
  vbox = dialog_get_content_area (iask);
  gtk_box_set_spacing (GTK_BOX(vbox), 15);
  hboxa = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hboxa, TRUE, TRUE, 0);
  quest = gtk_label_new (lab);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, quest, TRUE, TRUE, 0);

  if (id == 0 || id > 3)
  {
    answer = gtk_entry_new ();
    gtk_widget_set_size_request (answer, 100, -1);
    gtk_entry_set_alignment (GTK_ENTRY(answer), 1.0);
  }
  else
  {
    if (id < 3)
    {
      answer = create_combo ();
      gtk_widget_set_size_request (answer, -1, 40);
      if (id < 0) for (i=0; i<3; i++) combo_text_append (answer, field_init[i]);
      if (id == 1) for (i=0; i<3; i++) combo_text_append (answer, coord_type[i]);
      if (id == 2) for (i=0; i<NCFORMATS; i++) combo_text_append (answer, coord_files[i]);
    }
    else
    {
      GtkTreeIter iter;
      GtkTreeStore * store = store = gtk_tree_store_new (1, G_TYPE_STRING);
      for (i=0; i<4; i++)
      {
        gtk_tree_store_append (store, & iter, NULL);
        gtk_tree_store_set (store, & iter, 0, npt_type[i], -1);
      }
      answer = gtk_combo_box_new_with_model (GTK_TREE_MODEL(store));
      GtkCellRenderer * renderer = gtk_cell_renderer_combo_new ();
      gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (answer), renderer, TRUE);
      gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (answer), renderer, "text", 0, NULL);
      GList * cell_list = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT(answer));
      if(cell_list && cell_list -> data)
      {
        gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(answer), cell_list -> data, "markup", 0, NULL);
      }
    }
    gtk_combo_box_set_active (GTK_COMBO_BOX(answer), 0);
  }
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, answer, FALSE, FALSE, 10);
  if (id == 3)
  {
    answer_info = markup_label(npt_info[0], -1, -1, 0.5, 0.5);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, answer_info, FALSE, FALSE, 5);
    g_signal_connect(G_OBJECT(answer), "changed", G_CALLBACK(on_answer_changed), NULL);
  }

  run_this_gtk_dialog (iask, G_CALLBACK(run_iask), GINT_TO_POINTER(id));
  return res_int;
}

gchar * res_char;

/*
*  G_MODULE_EXPORT void run_cask (GtkDialog * cask, gint response_id, gpointer data)
*
*  Usage: enter a string - running the dialog
*
*  GtkDialog * cask : the GtkDialog sending the signal
*  gint response_id : the response id
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void run_cask (GtkDialog * cask, gint response_id, gpointer data)
{
  int i = GPOINTER_TO_INT(data);
  gboolean done = FALSE;
  if (i < 0)
  {
    while (! done)
    {
      if (response_id == GTK_RESPONSE_OK)
      {
        res_char = g_strdup_printf ("%s", entry_get_text (GTK_ENTRY(answer)));
        done = TRUE;
      }
    }
  }
  else
  {
    if (response_id == GTK_RESPONSE_OK)
    {
      res_char = g_strdup_printf ("%s", entry_get_text (GTK_ENTRY(answer)));
    }
    else
    {
      res_char = NULL;
    }
    done = TRUE;
  }
  if (done) destroy_this_dialog (cask);
}

/*
*  gchar * cask (char * question,  char * lab, int id, char * old, GtkWidget * win)
*
*  Usage: enter a string - prepare the dialog
*
*  char * question : Message
*  char * lab      : Text to use for label
*  int id          : the required parameter id
*  char * old      : the initial value for the string
*  GtkWidget * win : the parent GtkWidget, if any
*/
gchar * cask (char * question,  char * lab, int id, char * old, GtkWidget * win)
{
  GtkWidget * cask;
  GtkWidget * dialog_ask;
  GtkWidget * hboxa;
  GtkWidget * quest;
  res_char = NULL;
  cask = message_dialogmodal (question, "Parameter required", GTK_MESSAGE_INFO, GTK_BUTTONS_OK, win);
  dialog_ask = dialog_get_content_area (cask);
  gtk_box_set_spacing (GTK_BOX(dialog_ask), 15);

  hboxa = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, dialog_ask, hboxa, TRUE, TRUE, 0);
  gtk_widget_show(hboxa);
  quest = gtk_label_new (lab);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, quest, TRUE, TRUE, 0);
  gtk_widget_show(quest);
  answer = gtk_entry_new ();
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, answer, TRUE, TRUE, 0);
  gtk_widget_set_size_request (answer, 50, -1);
  gtk_entry_set_alignment (GTK_ENTRY(answer), 1.0);
  update_entry_text (GTK_ENTRY(answer), old);
  run_this_gtk_dialog (cask, G_CALLBACK(run_cask), GINT_TO_POINTER(id));

  return (res_char);
}

/*
*  void init_data_ (int * nats, int * nspc, int * stps, int * cid)
*
*  Usage: update project data using information from Fortran90
*
*  int * nats : number of atoms
*  int * nspc : number of species
*  int * stps : number of steps
*  int * cid  : allocate chemistry data (1) or not (0)
*/
void init_data_ (int * nats, int * nspc, int * stps, int * cid)
{
  active_project -> natomes = * nats;
  active_project -> nspec = * nspc;
  active_project -> steps = * stps;
  alloc_proj_data (active_project, * cid);
  if (* cid) active_chem = active_project -> chemistry;
}

/*
*  void spec_data_ (int * status, int * ind, int * atd, int * nsp,
*                   char * lbel, char * el_nme,
*                   double * amss, double * rdus,
*                   double * nscatt, double * xscatt)
*
*  Usage: update project data using information from Fortran90
*
*  int * status    : Update data (1) or not (0)
*  int * ind       : the chemical species
*  int * atd       : Z
*  int * nsp       : Number of atoms of this species
*  char * lbel     : Symbol
*  char * el_nme   : Element
*  double * amss   : M
*  double * rdus   : Radius
*  double * nscatt : Neutron scattering length
*  double * xscatt : X scattering length
*/
void spec_data_ (int * status, int * ind, int * atd, int * nsp,
                 char * lbel, char * el_nme,
                 double * amss, double * rdus,
                 double * nscatt, double * xscatt)
{
  int id = * ind;
  active_chem -> label[id]= g_strdup_printf("%s", lbel);
  active_chem -> element[id]= g_strdup_printf("%s", el_nme);
  if (* status)
  {
    active_chem -> nsps[id] = * nsp;
    active_chem -> chem_prop[CHEM_Z][id] = (double) * atd;
    active_chem -> chem_prop[CHEM_M][id] = * amss;
    active_chem -> chem_prop[CHEM_R][id] = * rdus;
    active_chem -> chem_prop[CHEM_N][id] = * nscatt;
    active_chem -> chem_prop[CHEM_X][id] = * xscatt;
  }
}

/*
*  void print_info  (gchar * str, gchar * stag, GtkTextBuffer * buffer)
*
*  Usage: print information in GtkTextBuffer
*
*  gchar * str            : the text
*  gchar * stag           : the tags
*  GtkTextBuffer * buffer : the GtkTextBuffer to print to
*/
void print_info  (gchar * str, gchar * stag, GtkTextBuffer * buffer)
{
  GtkTextIter bEnd;
  GtkTextTag * tag;

  gtk_text_buffer_get_end_iter (buffer, &bEnd);
  if (stag != NULL)
  {
    tag = gtk_text_tag_table_lookup (gtk_text_buffer_get_tag_table(buffer), stag);
    gtk_text_buffer_insert_with_tags (buffer, &bEnd, str, -1, tag, NULL);
  }
  else
  {
    tag = gtk_text_tag_table_lookup (gtk_text_buffer_get_tag_table(buffer), "default-size");
    gtk_text_buffer_insert_with_tags (buffer, &bEnd, str, -1, tag, NULL);
  }
}

/*
*  gchar * textcolor (int i)
*
*  Usage: setup text color keyword
*
*  int i : color id
*/
gchar * textcolor (int i)
{
  gchar * col = NULL;
  switch (i - i * (i / 9))
  {
    case 0:
      col = g_strdup_printf ("red");
      break;
    case 1:
      col = g_strdup_printf ("blue");
      break;
    case 2:
      col = g_strdup_printf ("cyan");
      break;
    case 3:
      col = g_strdup_printf ("green");
      break;
    case 4:
      col = g_strdup_printf ("light_green");
      break;
    case 5:
      col = g_strdup_printf ("yellow");
      break;
    case 6:
      col = g_strdup_printf ("orange");
      break;
    case 7:
      col = g_strdup_printf ("violet");
      break;
    case 8:
      col = g_strdup_printf ("pink");
      break;
  }
  return col;
}

/*
*  void lattice_info_ (int * bid, double * volume, double * density,
                       double dvects[3][3], double rvects[3][3], double mod[3], double ang[3],
                       double f_to_c[3][3], double c_to_f[3][3])
*
*  Usage: lattice data from Fortran90
*
*  int * bid           : 0 or MD step if NPT
*  double * volume     : volume
*  double * density    : density
*  double dvects[3][3] : direct space lattice vectors
*  double rvects[3][3] : reciprocal lattice vectors
*  double mod[3]       : modulus of lattice vectors (a,b,c)
*  double ang[3]       : lattice angles (alpha, beta, gamma)
*  double f_to_c[3][3] : fractional to cartesian matrix
*  double c_to_f[3][3] : cartesian to fractional matrix
*/
void lattice_info_ (int * bid, double * volume, double * density,
                    double dvects[3][3], double rvects[3][3], double mod[3], double ang[3],
                    double f_to_c[3][3], double c_to_f[3][3])
{
  int i, j;
  for ( i=0; i<3; i++)
  {
    for (j=0; j<3; j++)
    {
      active_cell -> box[* bid].vect[i][j] = dvects[j][i];
      active_cell -> box[* bid].rvect[i][j] = rvects[j][i];
    }
    active_cell -> box[* bid].param[0][i] = mod[i];
    active_cell -> box[* bid].param[1][i] = ang[i];
  }
  active_cell -> box[* bid].frac_to_cart = mat4(f_to_c[0][0], f_to_c[0][1], f_to_c[0][2], 0.0,
                                                f_to_c[1][0], f_to_c[1][1], f_to_c[1][2], 0.0,
                                                f_to_c[2][0], f_to_c[2][1], f_to_c[2][2], 0.0,
                                                0.0, 0.0, 0.0, 0.0);
  active_cell -> box[* bid].cart_to_frac = mat4(c_to_f[0][0], c_to_f[0][1], c_to_f[0][2], 0.0,
                                                c_to_f[1][0], c_to_f[1][1], c_to_f[1][2], 0.0,
                                                c_to_f[2][0], c_to_f[2][1], c_to_f[2][2], 0.0,
                                                0.0, 0.0, 0.0, 0.0);
#ifdef DEBUG
  // m4_print (active_cell -> box[* bid].frac_to_cart);
  // m4_print (active_cell -> box[* bid].cart_to_frac);
#endif
  active_cell -> box[* bid].vol = * volume;
  active_cell -> box[* bid].dens = * density;
  if ((active_cell -> npt && * bid == active_project -> steps-1) || ! active_cell -> npt)
  {
    active_project -> max[GR] = fdmax_ (& active_cell -> ltype);
    active_project -> min[SQ] = active_project -> min[SK] = fkmin_ (& active_cell -> ltype);
    int i, j;
    active_cell -> volume = active_cell -> density = 0.0;
    i = (active_cell -> npt) ? active_project -> steps : 1;
    for (j=0; j<i; j++)
    {
      active_cell -> volume += active_cell -> box[j].vol;
      active_cell -> density += active_cell -> box[j].dens;
    }
    active_cell -> volume /= i;
    active_cell -> density /= i;
  }
}

/*
*  void send_chem_info_ (int prop[active_project -> nspec])
*
*  Usage: getting chemistry formula information from Fortran90
*
*  int prop[active_project -> nspec] : the formula
*/
void send_chem_info_ (int prop[active_project -> nspec])
{
  int i;
  for (i=0; i<active_project -> nspec; i++)
  {
    active_chem -> formula[i] = prop[i];
  }
}

/*
*  gchar * env_name (struct project * this_proj, int g, int s, int f, GtkTextBuffer * buffer)
*
*  Usage: ouput the name of a coordination sphere
*
*  struct project * this_proj : the target project
*  int g                      : the coordination (0 = total, 1 = partial)
*  int s                      : the chemical species
*  int f                      : With markup or not
*  GtkTextBuffer * buffer     : Output in a GtkTextBuffer, or not if NULL
*/
gchar * env_name (struct project * this_proj, int g, int s, int f, GtkTextBuffer * buffer)
{
  int l, m;
  gchar * spec = exact_name(this_proj -> chemistry -> label[s]);
  gchar * stra;
  gchar * strb;

  m = 0;
  for (l=0; l<this_proj -> nspec; l++)
  {
    m += this_proj -> coord -> partial_geo[s][g][l];
  }
  if (m > 0)
  {
    stra = g_strdup_printf ("%s [", spec);
    if (buffer != NULL)
    {
      print_info (spec, textcolor(s), buffer);
      print_info ("[", "bold", buffer);
    }
    for (l=0; l<this_proj -> nspec; l++)
    {
      m = this_proj -> coord -> partial_geo[s][g][l];
      if (m > 1)
      {
        if (f == 1)
        {
          strb = g_strdup_printf ("%s%s<sub>%d</sub>", stra, exact_name(this_proj -> chemistry -> label[l]), m);
        }
        else
        {
          strb = g_strdup_printf ("%s%s%d", stra, exact_name(this_proj -> chemistry -> label[l]), m);
        }
        if (buffer != NULL)
        {
          print_info (exact_name(this_proj -> chemistry -> label[l]), "bold", buffer);
          g_free (strb);
          strb = g_strdup_printf ("%d", m);
          print_info (strb, "sub", buffer);
        }
      }
      else if (m > 0)
      {
        strb = g_strdup_printf ("%s%s", stra, exact_name(this_proj -> chemistry -> label[l]));
        if (buffer != NULL)
        {
          print_info (exact_name(this_proj -> chemistry -> label[l]), "bold", buffer);
        }
      }
      else
      {
        strb = g_strdup_printf ("%s", stra);
      }
      g_free (stra);
      stra = g_strdup_printf ("%s", strb);
      g_free (strb);
    }
    strb = g_strdup_printf ("%s]", stra);
    if (buffer != NULL)
    {
      print_info ("]", "bold", buffer);
    }
    g_free (stra);
  }
  else
  {
    strb = g_strdup_printf ("%s - isolated", spec);
    if (buffer != NULL)
    {
      print_info (spec, textcolor(s), buffer);
      print_info (" - isolated", "bold", buffer);
    }
  }
  if (buffer != NULL)
  {
    strb = NULL;
  }
  g_free (spec);
  return strb;
}

/*
*  void update_after_calc (int calc)
*
*  Usage: To update all curve plots in the workspace after a calculation
*
*  int calc : Analysis id
*/
void update_after_calc (int calc)
{
  int i, j;
  tint cd;
  struct project * this_proj;
  for (i=0; i<nprojects; i++)
  {
    this_proj = get_project_by_id(i);
    if (this_proj -> initok[calc])
    {
      for (j= 0; j < this_proj -> numc[calc]; j++)
      {
        if (this_proj -> curves[calc][j] -> plot != NULL)
        {
          cd.a = i;
          cd.b = calc;
          cd.c = j;
          update_curve ((gpointer)& cd);
        }
      }
    }
  }
}
