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
#include "interface.h"
#include "glview.h"
#include "glwindow.h"

#define LABEL_FORMATS 4

gchar * lab_formats[LABEL_FORMATS] = {"Element name", "Atomic symbol", "Atomic symbol + ID number", "ID number"};

GtkWidget * atom_color_box;
GtkWidget ** color_title;
GtkWidget * tilt;

G_MODULE_EXPORT void set_measure_style (GtkComboBox * box, gpointer data)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  this_proj -> modelgl -> anim -> last -> img -> mpattern = gtk_combo_box_get_active (box);
  this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT void set_labels_format (GtkWidget * widg, gpointer data)
{
  tint * id = (tint *) data;
  int i = gtk_combo_box_get_active (GTK_COMBO_BOX(widg));
  struct project * this_proj = get_project_by_id(id -> a);
  if (i != this_proj -> modelgl -> anim -> last -> img -> labels_format[id -> b])
  {
    this_proj -> modelgl -> anim -> last -> img -> labels_format[id -> b] = i;
    if (id -> b < 2) this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
    if (id -> b == 3 || id -> b == 4) this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
    update (this_proj -> modelgl);
  }
}

G_MODULE_EXPORT void set_labels_render (GtkWidget * widg, gpointer data)
{
  tint * id = (tint *) data;
  int i = gtk_combo_box_get_active (GTK_COMBO_BOX(widg));
  struct project * this_proj = get_project_by_id(id -> a);
  if (i != this_proj -> modelgl -> anim -> last -> img -> labels_render[id -> b])
  {
    this_proj -> modelgl -> anim -> last -> img -> labels_render[id -> b] = i;
    if (id -> b < 2)
    {
      this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
    }
    else if (id -> b == 2)
    {
      this_proj -> modelgl -> create_shaders[MAXIS] = TRUE;
    }
    else if (id -> b == 3 || id -> b == 4)
    {
      gtk_combo_box_set_active (GTK_COMBO_BOX(tilt), this_proj -> modelgl -> anim -> last -> img -> mtilt);
      this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
    }
    update (this_proj -> modelgl);
  }
}

void init_labels_colors (image * img, int sp, int id)
{
  int i;
  for (i = 0; i < sp; i++)
  {
    img -> labels_color[id][i].red = img -> at_color[i+id*sp].red;
    img -> labels_color[id][i].green = img -> at_color[i+id*sp].green;
    img -> labels_color[id][i].blue = img -> at_color[i+id*sp].blue;
    img -> labels_color[id][i].alpha = 1.0;
    GdkRGBA col = colrgba_togtkrgba (img -> labels_color[id][i]);
    gtk_color_chooser_set_rgba (GTK_COLOR_CHOOSER(color_title[i]), & col);
  }
}

#ifdef GTK4
G_MODULE_EXPORT void use_atom_default_colors (GtkCheckButton * but, gpointer data)
#else
G_MODULE_EXPORT void use_atom_default_colors (GtkToggleButton * but, gpointer data)
#endif
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  int b = id -> b;
  gboolean val;
#ifdef GTK4
  val = gtk_check_button_get_active (but);
#else
  val = gtk_toggle_button_get_active (but);
#endif
  if (val)
  {
    if (this_proj -> modelgl -> anim -> last -> img -> labels_color[b] != NULL)
    {
      init_labels_colors (this_proj -> modelgl -> anim -> last -> img, this_proj -> nspec, b);
      g_free (this_proj -> modelgl -> anim -> last -> img -> labels_color[b]);
    }
    this_proj -> modelgl -> anim -> last -> img -> labels_color[b] = NULL;
  }
  else
  {
    this_proj -> modelgl -> anim -> last -> img -> labels_color[b] = g_malloc (2*this_proj -> nspec
                                                                    *sizeof*this_proj -> modelgl -> anim -> last -> img -> labels_color[b]);
    init_labels_colors (this_proj -> modelgl -> anim -> last -> img, this_proj -> nspec, b);
  }
  widget_set_sensitive (atom_color_box, ! val);
  this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT void set_labels_font (GtkFontButton * fontb, gpointer data)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  g_free (this_proj -> modelgl -> anim -> last -> img -> labels_font[id -> b]);
  this_proj -> modelgl -> anim -> last -> img -> labels_font[id -> b] = g_strdup_printf ("%s", gtk_font_chooser_get_font (GTK_FONT_CHOOSER(fontb)));
  if (id -> b < 2)
  {
    this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  }
  else if (id -> b == 2)
  {
    this_proj -> modelgl -> create_shaders[MAXIS] = TRUE;
  }
  else if (id -> b == 3 || id -> b == 4)
  {
    this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
  }
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT void set_label_color (GtkColorChooser * colob, gpointer data)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  this_proj -> modelgl -> anim -> last -> img -> labels_color[id -> b][id -> c] = get_button_color (colob);
  if (id -> b < 2) this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  if (id -> b == 3 || id -> b == 4) this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT void set_labels_position (GtkComboBox * box, gpointer data)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  this_proj -> modelgl -> anim -> last -> img -> labels_position[id -> b] = gtk_combo_box_get_active (box);
  if (id -> b < 2) this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  if (id -> b == 3 || id -> b == 4) this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
  update (this_proj -> modelgl);
}

void label_shift_has_changed (gpointer data, double value)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  int i = id -> b / 10;
  int j = id -> b - i * 10;
  this_proj -> modelgl -> anim -> last -> img -> labels_shift[i][j] = value;
  if (id -> b < 2) this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT gboolean scroll_set_label_shift (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  label_shift_has_changed (data, value);
  return FALSE;
}

G_MODULE_EXPORT void set_label_shift (GtkRange * range, gpointer data)
{
  label_shift_has_changed (data, gtk_range_get_value (range));
}

#ifdef GTK4
G_MODULE_EXPORT void set_labels_scale (GtkCheckButton * but, gpointer data)
#else
G_MODULE_EXPORT void set_labels_scale (GtkToggleButton * but, gpointer data)
#endif
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
#ifdef GTK4
  this_proj -> modelgl -> anim -> last -> img -> labels_scale[id -> b] = gtk_check_button_get_active (but);
#else
  this_proj -> modelgl -> anim -> last -> img -> labels_scale[id -> b] = gtk_toggle_button_get_active (but);
#endif
  if (id -> b < 2) this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  if (id -> b == 2) this_proj -> modelgl -> create_shaders[MAXIS] = TRUE;
  if (id -> b == 3 || id -> b == 4) this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT void set_labels_tilt (GtkComboBox * box, gpointer data)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  this_proj -> modelgl -> anim -> last -> img -> mtilt = gtk_combo_box_get_active (box);
  if (id -> b < 2) this_proj -> modelgl -> create_shaders[LABEL] = TRUE;
  if (id -> b == 3 || id -> b == 4) this_proj -> modelgl  -> create_shaders[MEASU] = TRUE;
  update (this_proj -> modelgl);
}

void mesure_factor_has_changed (gpointer data, double value)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  this_proj -> modelgl -> anim -> last -> img -> mfactor = (int)value;
  this_proj -> modelgl  -> create_shaders[MEASU] = TRUE;
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT gboolean scroll_set_measure_factor (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  mesure_factor_has_changed (data, value);
  return FALSE;
}

G_MODULE_EXPORT void set_measure_factor (GtkRange * range, gpointer data)
{
  mesure_factor_has_changed (data, gtk_range_get_value (range));
}

void measure_width_has_changed (gpointer data, double value)
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  this_proj -> modelgl -> anim -> last -> img -> mwidth = value;
  this_proj -> modelgl  -> create_shaders[MEASU] = TRUE;
  update (this_proj -> modelgl);
}

G_MODULE_EXPORT gboolean scroll_set_measure_width (GtkRange * range, GtkScrollType scroll, gdouble value, gpointer data)
{
  measure_width_has_changed (data, value);
  return FALSE;
}

G_MODULE_EXPORT void set_measure_width (GtkRange * range, gpointer data)
{
  measure_width_has_changed (data, gtk_range_get_value (range));
}

GtkWidget * line_box;
GtkWidget * lstyle;

#ifdef GTK4
G_MODULE_EXPORT void enable_lines (GtkCheckButton * but, gpointer data)
#else
G_MODULE_EXPORT void enable_lines (GtkToggleButton * but, gpointer data)
#endif
{
  tint * id = (tint *) data;
  struct project * this_proj = get_project_by_id(id -> a);
  int i;
#ifdef GTK4
  i = gtk_check_button_get_active (but);
#else
  i = gtk_toggle_button_get_active (but);
#endif
  widget_set_sensitive (line_box, i);
  if (i)
  {
    this_proj -> modelgl -> anim -> last -> img -> mpattern = 0;
  }
  else
  {
    this_proj -> modelgl -> anim -> last -> img -> mpattern = -1;
  }
  gtk_combo_box_set_active (GTK_COMBO_BOX(lstyle), this_proj -> modelgl -> anim -> last -> img -> mpattern);
  this_proj -> modelgl -> create_shaders[MEASU] = TRUE;
  update (this_proj -> modelgl);
}

GtkWidget * labels_tab (glwin * view, int id)
{
  int i;
  gchar * lpos[3] = {"x", "y", "z"};

  struct project * this_proj = get_project_by_id(view -> proj);

  GtkWidget * tbox = create_vbox (BSEP);
  GtkWidget * vbox = create_vbox (5);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, tbox, vbox, FALSE, FALSE, 5);

  GtkWidget * box;
  if (id < 3)
  {
    box = abox (vbox, "Templates: ", 0);
    GtkWidget * formats  = create_combo ();
    for (i=0; i<LABEL_FORMATS; i++)
    {
      combo_text_append (formats, lab_formats[i]);
    }
    gtk_combo_box_set_active (GTK_COMBO_BOX(formats), view -> anim -> last -> img -> labels_format[id]);
    gtk_widget_set_size_request (formats, 220, -1);
    g_signal_connect (G_OBJECT (formats), "changed", G_CALLBACK(set_labels_format), & view -> colorp[id][0]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, formats, FALSE, FALSE, 10);
  }
  else
  {
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, markup_label("<b><u>Label(s):</u></b>", -1, 40, 0.0, 0.5), FALSE, FALSE, 0);
  }
  // Rendering
  box = abox (vbox, "Rendering: ", 0);
  GtkWidget * config  = create_combo ();
  combo_text_append (config, "Basic text");
  combo_text_append (config, "Highlighted");
  gtk_combo_box_set_active (GTK_COMBO_BOX(config), view -> anim -> last -> img -> labels_render[id]);
  gtk_widget_set_size_request (config, 220, -1);
  g_signal_connect (G_OBJECT (config), "changed", G_CALLBACK(set_labels_render), & view -> colorp[id][0]);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, config, FALSE, FALSE, 10);

  // Font
  box = abox (vbox, "Font:", 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box,
                       font_button (view -> anim -> last -> img -> labels_font[id], 220, -1, G_CALLBACK(set_labels_font), & view -> colorp[id][0]),
                       FALSE, FALSE, 10);

  if (id == 3)
  {
    box = abox (vbox, "Font color:", 0);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box,
                         color_button(view -> anim -> last -> img -> labels_color[id][0], TRUE, 220, -1, G_CALLBACK(set_label_color), & view -> colorp[id][0]),
                         FALSE, FALSE, 10);
  }

  // Position
  box = abox (vbox, "Position:", 0);
  GtkWidget * position = create_combo ();
  combo_text_append (position, "Always visible");
  combo_text_append (position, "Normal");
  gtk_combo_box_set_active (GTK_COMBO_BOX(position), view -> anim -> last -> img -> labels_position[id]);
  gtk_widget_set_size_request (position, 220, -1);
  g_signal_connect (G_OBJECT (position), "changed", G_CALLBACK(set_labels_position), & view -> colorp[id][0]);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, position, FALSE, FALSE, 10);

  // Size / scale
  box = abox (vbox, "Size:", 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box,
                       check_button ("scale with zoom in/out", 220, -1, view -> anim -> last -> img -> labels_scale[id], G_CALLBACK(set_labels_scale), & view -> colorp[id][0]),
                       FALSE, FALSE, 10);

  if (id == 3)
  {
    // Tilt
    box = abox (vbox, "Tilt:", 0);
    tilt = create_combo ();
    combo_text_append (tilt, "None");
    combo_text_append (tilt, "Adapted");
    gtk_combo_box_set_active (GTK_COMBO_BOX(tilt), view -> anim -> last -> img -> mtilt);
    gtk_widget_set_size_request (tilt, 220, -1);
    g_signal_connect (G_OBJECT (tilt), "changed", G_CALLBACK(set_labels_tilt), & view -> colorp[id][0]);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, tilt, FALSE, FALSE, 10);
  }

  GtkWidget * chbox;
  if (id < 3)
  {
    box = abox (vbox, "Distance to atom [Å]:", 0);
    chbox = create_hbox (0);
    for (i=0; i<2; i++)
    {
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, chbox, markup_label(lpos[i], 30, -1, 0.5, 0.5), FALSE, FALSE, 10);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, chbox,
                           create_hscale(-5.0, 5.0, 0.01, view -> anim -> last -> img -> labels_shift[id][i], GTK_POS_TOP,
                                         3, 100, G_CALLBACK(set_label_shift), G_CALLBACK(scroll_set_label_shift), & view -> colorp[id*10+i][0]),
                           FALSE, FALSE, 0);
    }
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, chbox, FALSE, FALSE, 0);
  }

  // Colors
  gboolean ac;
  if (view -> anim -> last -> img -> labels_color[id] == NULL)
  {
    ac = TRUE;
  }
  else
  {
    ac = FALSE;
  }
  if (id < 3)
  {
    box = abox (vbox, "Color(s):", 0);
    GtkWidget * col_box = create_vbox (BSEP);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, col_box, FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, col_box,
                         check_button ("Use atom colors", 100, 40, ac,
                                       G_CALLBACK(use_atom_default_colors), (gpointer)& view -> colorp[id][0]), FALSE, FALSE, 0);
    atom_color_box = create_vbox (5);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, col_box, atom_color_box, FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, atom_color_box, markup_label ("Please select atom label colors:", -1, -1, 0.25, 0.5), FALSE, FALSE, 5);
    color_title = g_malloc (this_proj -> nspec*sizeof*color_title);
    for (i=0; i< this_proj -> nspec; i++)
    {
      chbox = create_hbox (0);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, chbox, markup_label(this_proj -> chemistry -> label[i], 120, -1, 0.5, 0.5), FALSE, FALSE, 20);
      if (view -> anim -> last -> img -> labels_color[id] == NULL)
      {
        color_title[i] = color_button(view -> anim -> last -> img -> at_color[i], TRUE, 80, -1, G_CALLBACK(set_label_color), & view -> colorp[id][i]);
      }
      else
      {
        color_title[i] = color_button(view -> anim -> last -> img -> labels_color[id][i], TRUE, 80, -1, G_CALLBACK(set_label_color), & view -> colorp[id][i]);
      }
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, chbox, color_title[i], FALSE, FALSE, 0);
      add_box_child_start (GTK_ORIENTATION_VERTICAL, atom_color_box, chbox, FALSE, FALSE, 0);
    }
    widget_set_sensitive (atom_color_box, ! ac);
  }
  else
  {
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, markup_label("<b><u>Line(s):</u></b>", -1, 40, 0.0, 0.5), FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, check_button ("Show / hide: ", -1, 40,
                                                      view -> anim -> last -> img -> mpattern+1, G_CALLBACK(enable_lines),
                                                      & view -> colorp[id][0]), FALSE, FALSE, 0);
    line_box = create_vbox (BSEP);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, line_box, TRUE, TRUE, 0);
    box = abox (line_box, "Pattern:", 0);
    GtkListStore * store = gtk_list_store_new (1, GDK_TYPE_PIXBUF);
    GtkTreeIter iter;
    for (i=0; i<NDOTS; i++)
    {
      gtk_list_store_append (store, & iter);
      gtk_list_store_set (store, & iter, 0, gdk_pixbuf_new_from_file (dots[i], NULL), -1);
    }
    lstyle = gtk_combo_box_new_with_model (GTK_TREE_MODEL(store));
    g_object_unref (G_OBJECT(store));
    GtkCellRenderer * renderer;
    renderer = gtk_cell_renderer_pixbuf_new();
    gtk_cell_layout_pack_start( GTK_CELL_LAYOUT(lstyle), renderer, FALSE );
    gtk_cell_layout_set_attributes( GTK_CELL_LAYOUT(lstyle), renderer, "pixbuf", 0, NULL );
    gtk_combo_box_set_active (GTK_COMBO_BOX(lstyle), view -> anim -> last -> img -> mpattern);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, lstyle, TRUE, TRUE, 10);
    gtk_widget_set_size_request (lstyle, 100, 35);
    g_signal_connect (G_OBJECT (lstyle), "changed", G_CALLBACK(set_measure_style), & view -> colorp[id][0]);

    box = abox (line_box, "Factor:", 0);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box,
                         create_hscale(1.0, 10.0, 1.0, (double)view -> anim -> last -> img -> mfactor, GTK_POS_RIGHT, 0, 100,
                                       G_CALLBACK(set_measure_factor), G_CALLBACK(scroll_set_measure_factor), & view -> colorp[id][0]),
                         TRUE, TRUE, 0);

    box = abox (line_box, "Width:", 0);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box,
                         create_hscale(1.0, 10.0, 1.0, view -> anim -> last -> img -> mwidth, GTK_POS_RIGHT, 0, 100,
                                       G_CALLBACK(set_measure_width), G_CALLBACK(scroll_set_measure_width), & view -> colorp[id][0]),
                         TRUE, TRUE, 0);
  }
  return tbox;
}
