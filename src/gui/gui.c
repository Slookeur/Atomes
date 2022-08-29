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
#include "callbacks.h"
#include "interface.h"
#include "project.h"
#include "workspace.h"
#include "valid.h"

extern int objects[3];
extern int * object_was_selected[3];
extern int ** tmp_object_id[3];
extern GtkWidget * curvetbox (void);
extern GtkWidget * work_menu (int id, int p, int c);

extern G_MODULE_EXPORT void compute_this_prop (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void on_create_new_project (GtkWidget * widg, gpointer data);
extern int get_atom_id_from_periodic_table (atom_search * asearch);
extern G_MODULE_EXPORT void leaving_from_menu (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void on_edit_activate (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void on_calc_activate (GtkWidget * widg, gpointer data);

#ifdef GTK3
GtkWidget * MainEvent;
#endif

dint davect[9];
ColRGBA std[6];

#ifdef GTK3
G_MODULE_EXPORT gboolean pop_menu (GtkWidget * widget, GdkEventButton * event, gpointer data)
{
  if (event -> button == 3)
  {
    pop_menu_at_pointer (work_menu (1, -1, -1), (GdkEvent *)event);
  }
  return FALSE;
}
#endif

void clean_view ()
{
  MainView = destroy_this_widget (MainView);
  atomes_logo = destroy_this_widget (atomes_logo);
#ifdef GTK4
  atomes_logo = gtk_picture_new_for_filename (PACKAGE_LOGO);
  gtk_widget_set_size_request (atomes_logo, 550, -1);
  add_container_child (CONTAINER_SCR, MainScrol[1], atomes_logo);
#else
  atomes_logo = gtk_image_new_from_file (PACKAGE_LOGO);
  MainScrol[1] = destroy_this_widget (MainScrol[1]);
  gtk_container_add (GTK_CONTAINER(MainEvent), atomes_logo);
  show_the_widgets (MainEvent);
#endif
}

void view_buffer (GtkTextBuffer * buffer)
{
  if (atomes_logo)
  {
#ifdef GTK4
    add_container_child (CONTAINER_SCR, MainScrol[1], NULL);
#endif
    atomes_logo = destroy_this_widget (atomes_logo);
  }
  gboolean add = FALSE;
  if (! MainView)
  {
    MainView = gtk_text_view_new ();
    if (! registered_atomes || testing_atomes) widget_set_sensitive (MainView, 0);
    gtk_text_view_set_editable (GTK_TEXT_VIEW(MainView), 0);
    text_view_set_monospace (MainView);
    add = TRUE;
  }
  gtk_text_view_set_buffer (GTK_TEXT_VIEW(MainView), buffer);
#ifdef GTK3
  if (! MainScrol[1])
  {
    MainScrol[1] = create_scroll (NULL, -1, -1, GTK_SHADOW_ETCHED_IN);
    gtk_container_add (GTK_CONTAINER(MainEvent), MainScrol[1]);
    add = TRUE;
  }
  if (add) gtk_container_add (GTK_CONTAINER(MainScrol[1]), MainView);
#else
  g_debug ("view buffer 6");
  if (add)
  {
     add_container_child (CONTAINER_SCR, MainScrol[1], MainView);
     g_debug ("view buffer 7");
  }
#endif
  show_the_widgets (MainScrol[1]);
}

void atomes_key_pressed (guint keyval, GdkModifierType state)
{
  if (state & GDK_CONTROL_MASK)
  {
    switch (keyval)
    {
      case GDK_KEY_a:
        create_about_dialog (NULL, NULL);
        break;
      case GDK_KEY_c:
        on_close_workspace (NULL, GINT_TO_POINTER(1));
        break;
      case GDK_KEY_n:
        on_create_new_project (NULL, NULL);
        break;
      case GDK_KEY_o:
        on_open_save_activate (NULL, GINT_TO_POINTER(0));
        break;
      case GDK_KEY_p:
        get_atom_id_from_periodic_table (NULL);
        break;
      case GDK_KEY_q:
#ifdef GTK4
        leaving_question (NULL, NULL);
#else
        leaving_question (NULL, NULL, NULL);
#endif
        break;
      case GDK_KEY_s:
        on_save_as_activate (NULL, GINT_TO_POINTER(3));
        break;
      case GDK_KEY_t:
        on_show_curve_toolbox (NULL, NULL);
        break;
      case GDK_KEY_w:
        on_open_save_activate (NULL, GINT_TO_POINTER(2));
        break;
    }
  }
}

#ifdef GTK3
/* gboolean on_atomes_pressed (GtkWidget * widg, GdkEventKey * event, gpointer data)
{
  if (event -> type == GDK_KEY_PRESS)
  {
    atomes_key_pressed (event-> keyval, event -> state);
  }
  return FALSE;
} */
#endif

void add_action (GSimpleAction * action)
{
  g_action_map_add_action (G_ACTION_MAP(AtomesApp), G_ACTION(action));
}

void remove_action (gchar * action_name)
{
  g_action_map_remove_action (G_ACTION_MAP(AtomesApp), (const gchar *)action_name);
}

void remove_edition_actions ()
{
  int i;
  for (i=0; i<3; i++) remove_action (edition_action_names[i]);
}

void remove_edition_and_analyze_actions ()
{
  remove_edition_actions ();
  int i;
  for (i=0; i<9; i++) remove_action (analyze_action_names[i]);
}

G_MODULE_EXPORT void show_periodic_table (GtkWidget * widg, gpointer data)
{
  get_atom_id_from_periodic_table (NULL);
}

G_MODULE_EXPORT void register_atomes (GtkWidget * widg, gpointer data)
{
  registered_atomes = validate ();
}

G_MODULE_EXPORT void atomes_menu_bar_action (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  gchar * name = g_strdup_printf ("%s", g_action_get_name(G_ACTION(action)));
  if (g_strcmp0 (name, "workspace.open") == 0)
  {
    on_open_save_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "workspace.save") == 0)
  {
    on_open_save_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "workspace.save-as") == 0)
  {
    on_save_as_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "workspace.close") == 0)
  {
    on_close_workspace (NULL, data);
  }
  else if (g_strcmp0 (name, "project.new") == 0)
  {
    on_create_new_project (NULL, data);
  }
  else if (g_strcmp0 (name, "project.edit") == 0)
  {
    change_project_name (NULL, data);
  }
  else if (g_strcmp0 (name, "project.active") == 0)
  {
    activate_project (NULL, data);
  }
  else if (g_strcmp0 (name, "project.compute") == 0)
  {
    compute_this_prop (NULL, data);
  }
  else if (g_strcmp0 (name, "project.open") == 0)
  {
    on_open_save_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "project.save") == 0)
  {
    on_open_save_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "project.save-as") == 0)
  {
    on_save_as_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "project.close") == 0)
  {
    on_close_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "export.isaacs") == 0)
  {
    on_isaacs_port (NULL, data);
  }
  else if (g_strcmp0 (name, "export.coordinates") == 0)
  {
    on_coord_port (NULL, data);
  }
  else if (g_strcmp0 (name, "import.isaacs") == 0)
  {
    on_isaacs_port (NULL, data);
  }
  else if (g_strcmp0 (name, "import.coordinates") == 0)
  {
    on_coord_port (NULL, data);
  }
  else if (g_strcmp0 (name, "program.quit") == 0)
  {
    leaving_from_menu (NULL, data);
  }
  else if (g_strcmp0 (name, "edit.chemistry") == 0)
  {
    on_edit_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "edit.periodicity") == 0)
  {
    on_edit_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "edit.cutoffs") == 0)
  {
    on_edit_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.gr") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.sq") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.sk") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.gk") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.bonds") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.rings") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.chains") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.sp") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.msd") == 0)
  {
    on_calc_activate (NULL, data);
  }
  else if (g_strcmp0 (name, "analyze.tool-box") == 0)
  {
    on_show_curve_toolbox (NULL, data);
  }
  else if (g_strcmp0 (name, "help.periodic") == 0)
  {
    show_periodic_table (NULL, data);
  }
  else if (g_strcmp0 (name, "help.about") == 0)
  {
    create_about_dialog (NULL, data);
  }
  else if (g_strcmp0 (name, "help.register") == 0)
  {
    register_atomes (NULL, data);
  }
  g_free (name);
}

GIcon * get_gicon_from_data (int format, const gchar * icon)
{
  switch (format)
  {
    case IMG_FILE:
      return g_file_icon_new (g_file_new_for_path (icon));
      break;
    case IMG_STOCK:
      return g_themed_icon_new (icon);
      break;
    default:
      return NULL;
      break;
  }
}

G_MODULE_EXPORT void change_radio_state (GSimpleAction * action, GVariant * state, gpointer data)
{
  g_simple_action_set_state (action, state);
}

void widget_add_action (GSimpleActionGroup * action_group, const gchar * act, GCallback handler, gpointer data,
                        gboolean check, gboolean status, gboolean radio, const gchar * stat)
{
   GSimpleAction * action;
   GVariant * action_state;
   if (radio)
   {
     action_state = g_variant_new_string (stat);
     action = g_simple_action_new_stateful (act, G_VARIANT_TYPE_STRING, action_state);
   }
   else if (check)
   {
     action_state = g_variant_new_boolean (status);
     action = g_simple_action_new_stateful (act, NULL, action_state);
   }
   else
   {
     action = g_simple_action_new (act, NULL);
   }
   g_signal_connect (action, "activate", handler, data);
   g_action_map_add_action (G_ACTION_MAP(action_group), G_ACTION(action));
}

GMenuItem * create_gmenu_item (const gchar * label, const gchar * action, const gchar * accel,
                               const gchar * custom, int format, const gchar * icon,
                               gboolean check, gboolean status, gboolean radio, const gchar * rstatus)
{
  GMenuItem * item;
  if (custom)
  {
    item = g_menu_item_new (NULL, NULL);
    g_menu_item_set_attribute (item, "custom", "s", custom, NULL);
    // g_menu_item_set_attribute_value (item, "custom", g_variant_new_string (custom));
    // GVariant * cust = g_menu_item_get_attribute_value (item, "custom",  g_variant_type_new("s"));
    // if (cust) g_print ("item custom is:: %s\n", g_variant_get_string  (cust, NULL));
  }
  else
  {
    item = g_menu_item_new (label, action);
    g_menu_item_set_attribute (item, "use-markup", "b", TRUE, NULL);
    if (accel) g_menu_item_set_attribute (item, "accel", "s", accel, NULL);
    // if (check) g_menu_item_set_attribute (item, "target", "b", status, NULL);
    if (radio) g_menu_item_set_attribute (item, "target", "s", rstatus, NULL);
#ifdef MENU_ICONS
    if (icon)
    {
      GIcon * gicon = get_gicon_from_data (format, icon);
      if (gicon)
      {
        g_menu_item_set_icon (item, gicon);
        g_object_unref (gicon);
      }
    }
#endif
  }
  return item;
}

void append_menu_item (GMenu * menu, const gchar * label, const gchar * action, const gchar * accel,
                                     const gchar * custom, int format, const gchar * icon,
                                     gboolean check, gboolean status, gboolean radio, const gchar * rstatus)
{
  GMenuItem * item = create_gmenu_item (label, action, accel, custom, format, icon, check, status, radio, rstatus);
  g_menu_append_item (menu, item);
  g_object_unref (item);
}

GMenu * workspace_section (gchar * act, int pop)
{
  GMenu * menu = g_menu_new ();
  gchar * str;
  str =  g_strdup_printf ("%s.workspace.open", act);
  append_menu_item (menu, "Open", (const gchar *)str, "<CTRL>W", NULL, IMG_STOCK, FOPEN, FALSE, FALSE, FALSE, NULL);
  g_free (str);
  if (! pop || nprojects)
  {
    str =  g_strdup_printf ("%s.workspace.save", act);
    append_menu_item (menu, "Save", (const gchar *)str, NULL, NULL, IMG_STOCK, FSAVE, FALSE, FALSE, FALSE, NULL);
    g_free (str);
    str =  g_strdup_printf ("%s.workspace.save-as", act);
    append_menu_item (menu, "Save As", (const gchar *)str, "<CTRL>S", NULL, IMG_STOCK, FSAVEAS, FALSE, FALSE, FALSE, NULL);
    g_free (str);
    str =  g_strdup_printf ("%s.workspace.close", act);
    append_menu_item (menu, "Close", (const gchar *)str, "<CTRL>C", NULL, IMG_STOCK, FCLOSE, FALSE, FALSE, FALSE, NULL);
    g_free (str);
  }
  return menu;
}

GMenu * port_section (gchar * act, int pop, int port)
{
  gchar * port_action[2]={"export", "import"};
  GMenu * menu = g_menu_new ();
  gchar * str;
  str =  g_strdup_printf ("%s.%s.isaacs", act, port_action[port]);
  append_menu_item (menu, "ISAACS Project File (*.ipf)", (const gchar *)str, NULL, NULL, IMG_FILE, PACKAGE_MOL, FALSE, FALSE, FALSE, NULL);
  g_free (str);
  str =  g_strdup_printf ("%s.%s.coordinates", act, port_action[port]);
  append_menu_item (menu, "Atomic Coordinates", (const gchar *)str, NULL, NULL, IMG_FILE, (port) ? PACKAGE_IMP : PACKAGE_CON, FALSE, FALSE, FALSE, NULL);
  g_free (str);
  return menu;
}

GMenu * project_section (gchar * act, int pop_up, int proj, int calc)
{
  GMenu * menu = g_menu_new ();
  gchar * str, * str_n;
  if (pop_up && nprojects && proj > -1)
  {
    if (activep != proj)
    {
      str = g_strdup_printf ("%s.project.active", act);
      append_menu_item (menu, "Make Active", (const gchar *)str, NULL, NULL, IMG_STOCK, YES, FALSE, FALSE, FALSE, NULL);
      g_free (str);
    }
    if (calc > -1)
    {
      str = g_strdup_printf ("%s.project.compute", act);
      str_n = g_strdup_printf ("Analyze: %s", work_menu_items[calc]);
      append_menu_item (menu, str_n, (get_project_by_id(proj) -> runok[calc]) ? (const gchar *)str : NULL, NULL, NULL, IMG_FILE, graph_img[calc], FALSE, FALSE, FALSE, NULL);
      g_free (str);
      g_free (str_n);
    }
    str = g_strdup_printf ("%s.project.edit", act);
    append_menu_item (menu, "Edit Name", (const gchar *)str, NULL, NULL, IMG_STOCK, EDITA, FALSE, FALSE, FALSE, NULL);
    g_free (str);
  }
  str = g_strdup_printf ("%s.project.new", act);
  append_menu_item (menu, "New", (const gchar *)str, "<CTRL>N", NULL, IMG_STOCK, FNEW, FALSE, FALSE, FALSE, NULL);
  g_free (str);
  str = g_strdup_printf ("%s.project.open", act);
  append_menu_item (menu, "Open", (const gchar *)str, "<CTRL>O", NULL, IMG_STOCK, FOPEN, FALSE, FALSE, FALSE, NULL);
  g_free (str);
  if (! pop_up || proj > -1)
  {
    str = g_strdup_printf ("%s.project.save", act);
    append_menu_item (menu, "Save", (const gchar *)str, NULL, NULL, IMG_STOCK, FSAVE, FALSE, FALSE, FALSE, NULL);
    g_free (str);
    str = g_strdup_printf ("%s.project.save-as", act);
    append_menu_item (menu, "Save As", (const gchar *)str, NULL, NULL, IMG_STOCK, FSAVEAS, FALSE, FALSE, FALSE, NULL);
    g_free (str);
    str = g_strdup_printf ("%s.project.close", act);
    append_menu_item (menu, "Close", (const gchar *)str, NULL, NULL, IMG_STOCK, FCLOSE, FALSE, FALSE, FALSE, NULL);
    g_free (str);
    g_menu_append_submenu (menu, "Export", (GMenuModel*)port_section(act, pop_up, 0));
  }
  return menu;
}

GMenu * import_section (gchar * act)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Import", (GMenuModel*)port_section(act, 0, 1));
  return menu;
}

GMenu * quit_section (gchar * act)
{
  GMenu * menu = g_menu_new ();
  gchar * str = g_strdup_printf ("%s.program.quit", act);
  append_menu_item (menu, "Quit", (const gchar *)str, "<CTRL>Q", NULL, IMG_STOCK, FEXIT, FALSE, FALSE, FALSE, NULL);
  g_free (str);
  return menu;
}

GMenu * workspace_title ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Workspace", "None", NULL, NULL, IMG_FILE, PACKAGE_TD, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu * project_title (int pop_up, int proj)
{
  GMenu * menu = g_menu_new ();
  if (pop_up && nprojects && proj > -1)
  {
    gchar * str = g_strdup_printf ("<b>%s</b>", get_project_by_id(proj) -> name);
    append_menu_item (menu, str, "None", NULL, NULL, IMG_FILE, PACKAGE_TD, FALSE, FALSE, FALSE, NULL);
    g_free (str);
  }
  else
  {
    append_menu_item (menu, "Project(s)", "None", NULL, NULL, IMG_FILE, PACKAGE_TD, FALSE, FALSE, FALSE, NULL);
  }
  return menu;
}

GMenu * create_workspace_menu (gchar * act, int pop_up, int proj, int calc)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_section (menu, NULL, (GMenuModel*)workspace_title());
  g_menu_append_section (menu, NULL, (GMenuModel*)workspace_section(act, pop_up));
  g_menu_append_section (menu, NULL, (GMenuModel*)project_title(pop_up, proj));
  g_menu_append_section (menu, NULL, (GMenuModel*)project_section(act, pop_up, proj, calc));
  g_menu_append_section (menu, NULL, (GMenuModel*)import_section(act));
  g_menu_append_section (menu, NULL, (GMenuModel*)quit_section(act));
  return menu;
}

GMenu * create_edit_menu ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Chemistry and Physics", "app.edit.chemistry", NULL, NULL, IMG_STOCK, DPROPERTIES, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "Box and Periodicity", "app.edit.periodicity", NULL, NULL, IMG_STOCK, DPROPERTIES, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "Bond Cutoffs", "app.edit.cutoffs", NULL, NULL, IMG_STOCK, DPROPERTIES, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu * tool_box_section ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Toolboxes", "app.analyze.tool-box", "<CTRL>T", NULL, IMG_STOCK, PAGE_SETUP, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu * create_analyze_menu ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "g(r) / G(r)", "app.analyze.gr", NULL, NULL, IMG_FILE, PACKAGE_GR, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "S(q) from FFT[g(r)]", "app.analyze.sq", NULL, NULL, IMG_FILE, PACKAGE_SQ, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "S(q) from Debye Eq.", "app.analyze.sk", NULL, NULL, IMG_FILE, PACKAGE_SQ, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "g(r) / G(r) from FFT[S(q)]", "app.analyze.gk", NULL, NULL, IMG_FILE, PACKAGE_GR, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "Bonds and Angles", "app.analyze.bonds", NULL, NULL, IMG_FILE, PACKAGE_BD, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "Ring statistics", "app.analyze.rings", NULL, NULL, IMG_FILE, PACKAGE_RI, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "Chain statistics", "app.analyze.chains", NULL, NULL, IMG_FILE, PACKAGE_CH, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "Spherical Harmonics", "app.analyze.sp", NULL, NULL, IMG_FILE, PACKAGE_SP, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "Mean Squared Displacement", "app.analyze.msd", NULL, NULL, IMG_FILE, PACKAGE_MS, FALSE, FALSE, FALSE, NULL);
  g_menu_append_section (menu, NULL, (GMenuModel*)tool_box_section());
  return menu;
}

GMenu * create_help_menu ()
{
  GMenu * menu = g_menu_new ();
  append_menu_item (menu, "Periodic Table", "app.help.periodic", "<CTRL>P", NULL, IMG_STOCK, ABOUT, FALSE, FALSE, FALSE, NULL);
  append_menu_item (menu, "About", "app.help.about", "<CTRL>A", NULL, IMG_STOCK, ABOUT, FALSE, FALSE, FALSE, NULL);
  // append_menu_item (menu, "Register", "app.help.register", NULL, NULL, IMG_STOCK, ABOUT, FALSE, FALSE, FALSE, NULL);
  return menu;
}

GMenu * atomes_menu_bar ()
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Workspace", (GMenuModel*)create_workspace_menu("app", 0, -1, -1));
  g_menu_append_submenu (menu, "Edit", (GMenuModel*)create_edit_menu());
  g_menu_append_submenu (menu, "Analyze", (GMenuModel*)create_analyze_menu());
  g_menu_append_submenu (menu, "Help", (GMenuModel*)create_help_menu());
  return menu;
}

#ifdef GTK4
G_MODULE_EXPORT void atomes_popup_menu (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  if (gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture) == GDK_BUTTON_SECONDARY)
  {
    GdkRectangle rect;
    rect.x = x;
    rect.y = y;
    rect.width = 1;
    rect.height = 1;
    GtkWidget * popover = work_menu (1, -1, -1);
    gtk_widget_set_parent (popover, MainWindow);
    gtk_popover_set_has_arrow (GTK_POPOVER(popover), FALSE);
    gtk_popover_set_pointing_to (GTK_POPOVER(popover), & rect);
    gtk_popover_popup (GTK_POPOVER(popover));
  }
}

G_MODULE_EXPORT gboolean on_atomes_pressed (GtkEventControllerKey * self, guint keyval, guint keycode, GdkModifierType state, gpointer data)
{
  atomes_key_pressed (keyval, state);
  return TRUE;
}
#endif

GtkWidget * create_main_window (GApplication * atomes)
{
  int i;
  THETD = gdk_pixbuf_new_from_file (PACKAGE_TD, NULL);
#ifdef GTK3
  gtk_window_set_default_icon (THETD);
#endif
  THEMO = gdk_pixbuf_new_from_file (PACKAGE_MOL, NULL);
  THEBD = gdk_pixbuf_new_from_file (PACKAGE_BD, NULL);
  SETTING = gdk_pixbuf_new_from_file (PACKAGE_PRO, NULL);
  SETTINGS = gdk_pixbuf_new_from_file (PACKAGE_SET, NULL);
  OGL = gdk_pixbuf_new_from_file (PACKAGE_OGL, NULL);
  OGLM = gdk_pixbuf_new_from_file (PACKAGE_OGLM, NULL);
  OGLC = gdk_pixbuf_new_from_file (PACKAGE_OGLC, NULL);

  GtkWidget * window = gtk_application_window_new (GTK_APPLICATION(atomes));
  gtk_window_set_title (GTK_WINDOW(window), PACKAGE);
  gtk_window_set_resizable (GTK_WINDOW(window), TRUE);
  gtk_widget_set_size_request (window, 900, 450);

  GSimpleAction * main_act[17];
  main_act[0]  = g_simple_action_new ("workspace.open", NULL);
  main_act[1]  = g_simple_action_new ("workspace.save", NULL);
  main_act[2]  = g_simple_action_new ("workspace.save-as", NULL);
  main_act[3]  = g_simple_action_new ("workspace.close", NULL);
  main_act[4]  = g_simple_action_new ("project.new", NULL);
  main_act[5]  = g_simple_action_new ("project.open", NULL);
  main_act[6]  = g_simple_action_new ("project.save", NULL);
  main_act[7]  = g_simple_action_new ("project.save-as", NULL);
  main_act[8]  = g_simple_action_new ("project.close", NULL);
  main_act[9]  = g_simple_action_new ("export.isaacs", NULL);
  main_act[10] = g_simple_action_new ("export.coordinates", NULL);
  main_act[11] = g_simple_action_new ("import.isaacs", NULL);
  main_act[12] = g_simple_action_new ("import.coordinates", NULL);
  main_act[13] = g_simple_action_new ("program.quit", NULL);
  main_act[14] = g_simple_action_new ("analyze.tool-box", NULL);
  main_act[15] = g_simple_action_new ("help.periodic", NULL);
  main_act[16] = g_simple_action_new ("help.about", NULL);

  for (i=0; i<3; i++) edition_actions[i] = g_simple_action_new (edition_action_names[i], NULL);
  for (i=0; i<9; i++) analyze_actions[i] = g_simple_action_new (analyze_action_names[i], NULL);

  for (i=0; i<17; i++) add_action (main_act[i]);

  g_signal_connect (main_act[0], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(2));
  g_signal_connect (main_act[1], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(3));
  g_signal_connect (main_act[2], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(3));
  g_signal_connect (main_act[3], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(1));
  g_signal_connect (main_act[4], "activate", G_CALLBACK(atomes_menu_bar_action), NULL);
  g_signal_connect (main_act[5], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(0));
  g_signal_connect (main_act[6], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(1));
  g_signal_connect (main_act[7], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(1));
  g_signal_connect (main_act[8], "activate", G_CALLBACK(atomes_menu_bar_action), NULL);
  g_signal_connect (main_act[9], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(1));
  g_signal_connect (main_act[10], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(1));
  g_signal_connect (main_act[11], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(0));
  g_signal_connect (main_act[12], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(0));
  g_signal_connect (main_act[13], "activate", G_CALLBACK(atomes_menu_bar_action), NULL);
  for (i=0; i<3; i++)
  {
    g_signal_connect (edition_actions[i], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(i));
  }
  for (i=0; i<NCALCS-3; i++)
  {
    g_signal_connect (analyze_actions[i], "activate", G_CALLBACK(atomes_menu_bar_action), GINT_TO_POINTER(i));
  }
  for (i=0; i<3; i++)
  {
    g_signal_connect (main_act[14+i], "activate", G_CALLBACK(atomes_menu_bar_action), NULL);
  }

// #endif

  /*GtkBuilder * builder = gtk_builder_new_from_file ("menus/main.ui");
  GMenuModel * model = G_MENU_MODEL (gtk_builder_get_object (builder, "atomes_menu_bar"));
  gtk_application_set_menubar (GTK_APPLICATION(atomes), G_MENU_MODEL(model));
  g_object_unref (model);*/
  gtk_application_set_menubar (GTK_APPLICATION(atomes), G_MENU_MODEL(atomes_menu_bar()));
  gtk_application_window_set_show_menubar (GTK_APPLICATION_WINDOW(window), TRUE);

#ifdef GTK3
  // g_signal_connect (G_OBJECT(window), "key-press-event", G_CALLBACK(on_atomes_pressed), NULL);
  MainEvent = gtk_event_box_new ();
  gtk_widget_add_events (MainEvent,
                         GDK_EXPOSURE_MASK | GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
  g_signal_connect(G_OBJECT(MainEvent), "button_press_event", G_CALLBACK(pop_menu), NULL);
  g_signal_connect(G_OBJECT(MainEvent), "button_release_event", G_CALLBACK(pop_menu), NULL);
#else
  add_widget_gesture_and_key_action (window, "atomes-context-click", G_CALLBACK(atomes_popup_menu), NULL,
                                             NULL, NULL, NULL,
                                             "atomes-key-pressed", G_CALLBACK(on_atomes_pressed), NULL,
                                             NULL, NULL, NULL, NULL, NULL, NULL);
#endif
  add_gtk_close_event (window, G_CALLBACK(leaving_question), NULL);
  GtkWidget * hpaned = create_hpaned ();
  int frame_size[2]={200, 550};
  for (i=0; i<2; i++)
  {
    MainFrame[i] = gtk_frame_new (NULL);
    gtk_widget_set_size_request (MainFrame[i], frame_size[i], -1);
    MainScrol[i] = create_scroll (NULL, -1, -1, GTK_SHADOW_ETCHED_IN);
#ifdef GTK3
    if (! i)
    {
      add_container_child (CONTAINER_FRA, MainFrame[i], MainScrol[i]);
    }
    else
    {
      add_container_child (CONTAINER_FRA, MainFrame[i], MainEvent);
      gtk_container_add (GTK_CONTAINER(MainEvent), MainScrol[i]);
    }
#else
    gtk_scrolled_window_set_propagate_natural_height ((GtkScrolledWindow *)MainScrol[i], TRUE);
    gtk_scrolled_window_set_propagate_natural_width ((GtkScrolledWindow *)MainScrol[i], TRUE);
    add_container_child (CONTAINER_FRA, MainFrame[i], MainScrol[i]);
#endif
  }
#ifdef GTK4
  gtk_paned_set_start_child (GTK_PANED (hpaned), MainFrame[0]);
  gtk_paned_set_resize_start_child (GTK_PANED (hpaned), FALSE);
  gtk_paned_set_shrink_start_child (GTK_PANED (hpaned), FALSE);
  gtk_paned_set_end_child (GTK_PANED (hpaned), MainFrame[1]);
  gtk_paned_set_resize_end_child (GTK_PANED (hpaned), TRUE);
  gtk_paned_set_shrink_end_child (GTK_PANED (hpaned), FALSE);
#else
  gtk_paned_pack1 (GTK_PANED (hpaned), MainFrame[0], FALSE, FALSE);
  gtk_paned_pack2 (GTK_PANED (hpaned), MainFrame[1], FALSE, FALSE);
#endif
  curvetoolbox = curvetbox ();
  add_project_to_workspace ();
  add_container_child (CONTAINER_WIN, window, hpaned);

  clean_view ();

  show_the_widgets (window);

  for (i=0; i<9; i++)
  {
    davect[i].a = i/3;
    davect[i].b = i-3*davect[i].a;
  }

  calc_img[0] = g_strdup_printf ("%s", PACKAGE_GR);
  calc_img[1] = g_strdup_printf ("%s", PACKAGE_SQ);
  calc_img[2] = g_strdup_printf ("%s", PACKAGE_SQ);
  calc_img[3] = g_strdup_printf ("%s", PACKAGE_GR);
  calc_img[4] = g_strdup_printf ("%s", PACKAGE_AN);
  calc_img[5] = g_strdup_printf ("%s", PACKAGE_RI);
  calc_img[6] = g_strdup_printf ("%s", PACKAGE_CH);
  calc_img[7] = g_strdup_printf ("%s", PACKAGE_SP);
  calc_img[8] = g_strdup_printf ("%s", PACKAGE_MS);
  calc_img[9] = g_strdup_printf ("%s", PACKAGE_BD);

  dots[0] = g_strdup_printf ("%s", PACKAGE_DOTA);
  dots[1] = g_strdup_printf ("%s", PACKAGE_DOTB);
  dots[2] = g_strdup_printf ("%s", PACKAGE_DOTC);
  dots[3] = g_strdup_printf ("%s", PACKAGE_DOTD);
  dots[4] = g_strdup_printf ("%s", PACKAGE_DOTE);
  dots[5] = g_strdup_printf ("%s", PACKAGE_DOTF);
  dots[6] = g_strdup_printf ("%s", PACKAGE_DOTG);
  dots[7] = g_strdup_printf ("%s", PACKAGE_DOTH);

  ifield[0] = g_strdup_printf ("%s", PACKAGE_DFBD);
  ifield[1] = g_strdup_printf ("%s", PACKAGE_DFBD);
  ifield[2] = g_strdup_printf ("%s", PACKAGE_DFAN);
  ifield[3] = g_strdup_printf ("%s", PACKAGE_DFAN);
  ifield[4] = g_strdup_printf ("%s", PACKAGE_DFDI);
  ifield[5] = g_strdup_printf ("%s", PACKAGE_DFDI);
  ifield[6] = g_strdup_printf ("%s", PACKAGE_DFTD);
  ifield[7] = g_strdup_printf ("%s", PACKAGE_DFIN);

  bravais_img[0] = g_strdup_printf ("%s", PACKAGE_SGTC);
  bravais_img[1] = g_strdup_printf ("%s", PACKAGE_SGMP);
  bravais_img[2] = g_strdup_printf ("%s", PACKAGE_SGMI);
  bravais_img[3] = g_strdup_printf ("%s", PACKAGE_SGOP);
  bravais_img[4] = g_strdup_printf ("%s", PACKAGE_SGOI);
  bravais_img[5] = g_strdup_printf ("%s", PACKAGE_SGOC);
  bravais_img[6] = g_strdup_printf ("%s", PACKAGE_SGOF);
  bravais_img[7] = g_strdup_printf ("%s", PACKAGE_SGTP);
  bravais_img[8] = g_strdup_printf ("%s", PACKAGE_SGTI);
  bravais_img[9] = g_strdup_printf ("%s", PACKAGE_SGTR);
  bravais_img[10] = g_strdup_printf ("%s", PACKAGE_SGHP);
  bravais_img[11] = g_strdup_printf ("%s", PACKAGE_SGCP);
  bravais_img[12] = g_strdup_printf ("%s", PACKAGE_SGCI);
  bravais_img[13] = g_strdup_printf ("%s", PACKAGE_SGCF);

  // Rouge
  std[0].red = 1.0;
  std[0].green = 0.0;
  std[0].blue = 0.0;
  // Jaune
  std[1].red = 1.0;
  std[1].green = 1.0;
  std[1].blue = 0.0;
  // Vert
  std[2].red = 0.0;
  std[2].green = 1.0;
  std[2].blue = 0.0;
  // Cyan
  std[3].red = 0.0;
  std[3].green = 1.0;
  std[3].blue = 1.0;
  // Bleu
  std[4].red = 0.0;
  std[4].green = 0.0;
  std[4].blue = 1.0;
  // Rose
  std[5].red = 1.0;
  std[5].green = 0.0;
  std[5].blue = 1.0;

  for (i=0; i<3; i++)
  {
    objects[i] = 0;
    object_was_selected[i] = NULL;
    tmp_object_id[i] = NULL;
  }
  return window;
}
