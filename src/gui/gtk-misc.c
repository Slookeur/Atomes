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

void show_the_widgets (GtkWidget * widg)
{
#ifdef GTK4
  gtk_widget_show (widg);
#else
  gtk_widget_show_all (widg);
#endif
}

void widget_set_sensitive (GtkWidget * widg, gboolean sensitive)
{
  if (widg != NULL)
  {
    if (GTK_IS_WIDGET(widg))
    {
      gtk_widget_set_sensitive (widg, sensitive);
    }
  }
}

void add_container_child (int type, GtkWidget * widg, GtkWidget * child)
{
#ifdef GTK3
  gtk_container_add (GTK_CONTAINER(widg), child);
#else
  switch (type)
  {
    case CONTAINER_WIN:
      // gtk_window_set_child ((GtkWindow *)widg, NULL);
      gtk_window_set_child ((GtkWindow *)widg, child);
      break;
    case CONTAINER_SCR:
      // Bug in the management of scrolled window child
/* Frequently getting warning messages at this point with GTK4
(atomes:?????): GLib-GObject-CRITICAL **: ??:??:??.???: g_object_set: assertion 'G_IS_OBJECT (object)' failed
(atomes:?????): Gtk-CRITICAL **: ??:??:??.???: gtk_widget_unparent: assertion 'GTK_IS_WIDGET (widget)' failed
*/
      gtk_scrolled_window_set_child ((GtkScrolledWindow *)widg, NULL);
      gtk_scrolled_window_set_child ((GtkScrolledWindow *)widg, child);
      break;
    case CONTAINER_VIE:
      // gtk_viewport_set_child ((GtkViewport *)widg, NULL);
      gtk_viewport_set_child ((GtkViewport *)widg, child);
      break;
    case CONTAINER_BUT:
      // gtk_button_set_child ((GtkButton *)widg, NULL);
      gtk_button_set_child ((GtkButton *)widg, child);
      break;
    case CONTAINER_FRA:
      // gtk_frame_set_child ((GtkFrame *)widg, NULL);
      gtk_frame_set_child ((GtkFrame *)widg, child);
      break;
    case CONTAINER_EXP:
      // gtk_expander_set_child ((GtkExpander *)widg, NULL);
      gtk_expander_set_child ((GtkExpander *)widg, child);
      break;
  }
#endif
}

void add_box_child_end (GtkWidget * widg, GtkWidget * child, gboolean expand, gboolean fill, int padding)
{
#ifdef GTK4
  // gtk_widget_set_hexpand (child, TRUE);
  gtk_box_append (GTK_BOX(widg), child);
#else
  gtk_box_pack_end (GTK_BOX(widg), child, expand, fill, padding);
#endif
}

void add_box_child_start (int orientation, GtkWidget * widg, GtkWidget * child, gboolean expand, gboolean fill, int padding)
{
#ifdef GTK4
  if (orientation == GTK_ORIENTATION_HORIZONTAL && padding)
  {
    gtk_box_append (GTK_BOX(widg), markup_label (" ", padding, -1, 0.0, 0.0));
  }
  else if (orientation == GTK_ORIENTATION_VERTICAL && padding > 10)
  {
    gtk_box_append (GTK_BOX(widg), markup_label (" ", -1, padding/2, 0.0, 0.0));
  }
  gtk_box_append (GTK_BOX(widg), child);
  if (orientation == GTK_ORIENTATION_HORIZONTAL && padding)
  {
    gtk_box_append (GTK_BOX(widg), markup_label (" ", padding, -1, 0.0, 0.0));
  }
  else if (orientation == GTK_ORIENTATION_VERTICAL && padding > 10)
  {
    gtk_box_append (GTK_BOX(widg), markup_label (" ", -1, padding/2, 0.0, 0.0));
  }
#else
  gtk_box_pack_start (GTK_BOX(widg), child, expand, fill, padding);
#endif
}

void check_menu_item_set_active (gpointer item, gboolean active)
{
#ifdef GTK3
  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM((GtkWidget *)item), active);
#else

#endif
}

gboolean check_menu_item_get_active (gpointer item)
{
#ifdef GTK4
  return FALSE;
#else
  return gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM((GtkWidget *)item));
#endif
}

#ifdef GTK3
void add_menu_child (GtkWidget * widg, GtkWidget * child)
{
  gtk_menu_shell_append (GTK_MENU_SHELL(widg), child);
}

void menu_item_set_submenu (GtkWidget * item, GtkWidget * sub_menu)
{
  gtk_menu_item_set_submenu (GTK_MENU_ITEM(item), sub_menu);
}

GtkWidget * menu_item_new_with_submenu (gchar * name, gboolean active, GtkWidget * sub_menu)
{
   GtkWidget * item =  create_menu_item (FALSE, name);
   menu_item_set_submenu (item, sub_menu);
   widget_set_sensitive (item, active);
   return item;
}
#endif

void add_item_child (GtkWidget * widg, GtkWidget * child)
{
#ifdef GTK4

#else
  gtk_container_add (GTK_CONTAINER(widg), child);
#endif
}

GtkWidget * new_gtk_window ()
{
#ifdef GTK4
  return gtk_window_new ();
#else
  return gtk_window_new (GTK_WINDOW_TOPLEVEL);
#endif
}

#ifdef GTK4
void add_widget_gesture_and_key_action (GtkWidget * widget,
                                        gchar * cp_name, GCallback cp_handler, gpointer cp_data,
                                        gchar * cr_name, GCallback cr_handler, gpointer cr_data,
                                        gchar * kp_name, GCallback kp_handler, gpointer kp_data,
                                        gchar * mo_name, GCallback mo_handler, gpointer mo_data,
                                        gchar * sc_name, GCallback sc_handler, gpointer sc_data)
{
  GtkGesture * gesture;
  GtkEventController * key;
  GtkEventController * motion;
  if (cp_name)
  {
    gesture = gtk_gesture_click_new ();
    gtk_event_controller_set_name (GTK_EVENT_CONTROLLER (gesture), cp_name);
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (gesture), 0);
    if (cp_handler) g_signal_connect (gesture, "pressed", cp_handler, cp_data);
    gtk_widget_add_controller (widget, GTK_EVENT_CONTROLLER (gesture));
  }
  if (cr_name)
  {
    gesture = gtk_gesture_click_new ();
    gtk_event_controller_set_name (GTK_EVENT_CONTROLLER (gesture), cr_name);
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (gesture), 0);
    if (cr_handler) g_signal_connect (gesture, "released", cr_handler, cr_data);
    gtk_widget_add_controller (widget, GTK_EVENT_CONTROLLER (gesture));
  }
  if (kp_name)
  {
    key = gtk_event_controller_key_new ();
    gtk_event_controller_set_name (key, kp_name);
    if (kp_handler) g_signal_connect (key, "key-pressed", kp_handler, kp_data);
    gtk_widget_add_controller (widget, key);
  }
  if (mo_name)
  {
    motion = gtk_event_controller_motion_new ();
    gtk_event_controller_set_name (motion, mo_name);
    if (mo_handler) g_signal_connect (motion, "motion", mo_handler, mo_data);
    gtk_widget_add_controller (widget, motion);
  }
  if (sc_name)
  {
    motion = gtk_event_controller_scroll_new (GTK_EVENT_CONTROLLER_SCROLL_VERTICAL);
    gtk_event_controller_set_name (motion, sc_name);
    if (sc_handler) g_signal_connect (motion, "scroll", sc_handler, sc_data);
    gtk_widget_add_controller (widget, motion);
  }
}
#endif

GtkWidget * create_win (gchar * str, GtkWidget * parent, gboolean modal, gboolean resiz)
{
  GtkWidget * win;
  win = new_gtk_window ();
  gtk_window_set_title (GTK_WINDOW(win), prepare_for_title(str));
  gtk_window_set_resizable (GTK_WINDOW (win), TRUE);
#ifdef GTK3
  gtk_window_set_attached_to (GTK_WINDOW (win), parent);
  gtk_window_set_icon (GTK_WINDOW (win), THETD);
#endif
  // gtk_window_set_transient_for (GTK_WINDOW (win), GTK_WINDOW (parent));
  gtk_window_set_modal (GTK_WINDOW(win), modal);
  gtk_window_set_resizable (GTK_WINDOW(win), resiz);
  gtk_window_set_destroy_with_parent (GTK_WINDOW (win), TRUE);
  return win;
}

void run_this_gtk_native_dialog (GtkNativeDialog * dial, GCallback handler, gpointer data)
{
  gtk_native_dialog_set_modal (dial, TRUE);
  if (handler) g_signal_connect (G_OBJECT(dial), "response", handler, data);
  gtk_native_dialog_show (dial);
  dialog_id ++;
  Event_loop[dialog_id] = g_main_loop_new (NULL, FALSE);
  g_main_loop_run (Event_loop[dialog_id]);
}

void run_this_gtk_dialog (GtkWidget * dial, GCallback handler, gpointer data)
{
  gtk_window_set_modal (GTK_WINDOW(dial), TRUE);
  if (handler) g_signal_connect (G_OBJECT(dial), "response", handler, data);
  show_the_widgets (dial);
  dialog_id ++;
  Event_loop[dialog_id] = g_main_loop_new (NULL, FALSE);
  g_main_loop_run (Event_loop[dialog_id]);
}

GtkWidget * dialogmodal (gchar * str, GtkWindow * parent)
{
  GtkWidget * win = gtk_dialog_new_with_buttons (str, parent, GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, _("_Close"), GTK_RESPONSE_CLOSE, NULL);
  gtk_window_set_resizable (GTK_WINDOW (win), FALSE);
#ifndef GTK4
  gtk_window_set_icon (GTK_WINDOW (win), THETD);
#endif
  return win;
}

#ifdef GTK3
GtkWidget * message_popover (gchar * message, GtkWidget * parent)
{
  GtkWidget * popover = gtk_popover_new (parent);
  // gtk_popover_set_position (GTK_POPOVER (popover), pos);
  GtkWidget * child = markup_label(message, -1, -1, -1, -1);
  gtk_widget_show (child);
  gtk_container_add (GTK_CONTAINER (popover), child);
  gtk_container_set_border_width (GTK_CONTAINER (popover), 6);
  return popover;
}
#endif

GtkWidget * message_dialogmodal (gchar * message, gchar * title, GtkMessageType mtype, GtkButtonsType buttons, GtkWidget * parent)
{
  GtkWidget * win;
  if (parent)
  {
    win = gtk_message_dialog_new (GTK_WINDOW(parent), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, mtype, buttons, NULL);
  }
  else
  {
    win = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, mtype, buttons, NULL);
  }
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (win), message);
  gtk_window_set_title (GTK_WINDOW(win), title);
  gtk_window_set_resizable (GTK_WINDOW (win), FALSE);
#ifndef GTK4
  gtk_window_set_icon (GTK_WINDOW (win), THETD);
#endif
  show_the_widgets (win);
  return win;
}

GtkWidget * dialog_cancel_apply (gchar * title, GtkWidget * parent, gboolean resiz)
{
  GtkWidget * dca = gtk_dialog_new_with_buttons (title, GTK_WINDOW(parent), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                                 "Cancel", GTK_RESPONSE_CANCEL, "Apply", GTK_RESPONSE_APPLY, NULL);
  gtk_window_set_resizable (GTK_WINDOW(dca), resiz);
#ifndef GTK4
  gtk_window_set_icon (GTK_WINDOW (dca), THETD);
#endif
  return dca;
}

void resize_this_window (GtkWidget * window, int x, int y)
{
#ifdef GTK3
  gtk_window_resize (GTK_WINDOW(window), x, y);
#else
  gtk_window_set_default_size (GTK_WINDOW(window), x, y);
#endif
}

const gchar * entry_get_text (GtkEntry * entry)
{
#ifdef GTK4
  return gtk_editable_get_text (GTK_EDITABLE(entry));
#else
  return gtk_entry_get_text (entry);
#endif
}

void update_entry_int (GtkEntry * entry, int intval)
{
  gchar * value = g_strdup_printf("%d", intval);
#ifdef GTK4
  gtk_editable_set_text (GTK_EDITABLE(entry), (const gchar *)value);
#else
  gtk_entry_set_text (entry, (const gchar *)value);
#endif
  g_free (value);
}

void update_entry_double (GtkEntry * entry, double doubleval)
{
  gchar * value = g_strdup_printf("%f", doubleval);
#ifdef GTK4
  gtk_editable_set_text (GTK_EDITABLE(entry), (const gchar *)value);
#else
  gtk_entry_set_text (entry, (const gchar *)value);
#endif
  g_free (value);
}

void update_entry_long_double (GtkEntry * entry, double doubleval)
{
  gchar * value = g_strdup_printf("%15.10lf", doubleval);
#ifdef GTK4
  gtk_editable_set_text (GTK_EDITABLE(entry), (const gchar *)value);
#else
  gtk_entry_set_text (entry, (const gchar *)value);
  g_free (value);
#endif
}

void update_entry_text (GtkEntry * entry, gchar * text)
{
#ifdef GTK4
  gtk_editable_set_text (GTK_EDITABLE(entry), (const gchar *)text);
#else
  gtk_entry_set_text (entry, (const gchar *)text);
#endif
}

void text_view_set_monospace (GtkWidget * view)
{
  gtk_text_view_set_monospace (GTK_TEXT_VIEW(view), TRUE);
}

void gtk_label_align (GtkWidget * lab, float ax, float ay)
{
  gtk_label_set_xalign (GTK_LABEL (lab), ax);
  gtk_label_set_yalign (GTK_LABEL (lab), ay);
}

gboolean is_the_widget_visible (GtkWidget * widg)
{
  if (GTK_IS_WIDGET(widg))
  {
    return gtk_widget_is_visible (widg);
  }
  else
  {
    return FALSE;
  }
}

GtkWidget * create_hscale (float min, float max, float delta,
                           float val, int pos, int round, int size,
                           GCallback handler, GCallback scroll_handler, gpointer data)
{
  GtkWidget * hscale;
  hscale = gtk_scale_new_with_range (GTK_ORIENTATION_HORIZONTAL, min, max, delta);
  gtk_scale_set_draw_value (GTK_SCALE(hscale), TRUE);
  gtk_scale_set_value_pos (GTK_SCALE(hscale), pos);
  gtk_widget_set_size_request (hscale, size, -1);
  gtk_range_set_value (GTK_RANGE(hscale), val);
  gtk_range_set_round_digits (GTK_RANGE(hscale), round);
  if (handler != NULL) g_signal_connect (G_OBJECT(hscale), "value-changed", handler, data);
  if (scroll_handler != NULL) g_signal_connect (G_OBJECT(hscale), "change-value", scroll_handler, data);
  return hscale;
}

GtkWidget * create_vscale (float min, float max, float delta,
                           float val, int pos, int round, int size,
                           GCallback handler, GCallback scroll_handler, gpointer data)
{
  GtkWidget * vscale;
  vscale = gtk_scale_new_with_range (GTK_ORIENTATION_VERTICAL, min, max, delta);
  gtk_scale_set_draw_value (GTK_SCALE(vscale), TRUE);
  gtk_scale_set_value_pos (GTK_SCALE(vscale), pos);
  gtk_widget_set_size_request (vscale, size, size);
  gtk_range_set_value (GTK_RANGE(vscale), val);
  gtk_range_set_round_digits (GTK_RANGE(vscale), round);
  if (handler != NULL) g_signal_connect (G_OBJECT(vscale), "value-changed", handler, data);
  if (scroll_handler != NULL) g_signal_connect (G_OBJECT(vscale), "change-value", scroll_handler, data);
  return vscale;
}

GtkWidget * create_hsep ()
{
  return gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
}

GtkWidget * create_hpaned ()
{
  return gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
}

GtkWidget * create_vbox (int spacing)
{
  return gtk_box_new (GTK_ORIENTATION_VERTICAL, spacing);
}

GtkWidget * create_hbox (int spacing)
{
  return gtk_box_new (GTK_ORIENTATION_HORIZONTAL, spacing);
}

GtkWidget * dialog_get_content_area (GtkWidget * widg)
{
#ifdef GTK3
  return gtk_dialog_get_content_area (GTK_DIALOG(widg));
#else
  GtkWidget * vbox = create_vbox (BSEP);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, gtk_dialog_get_content_area (GTK_DIALOG(widg)), vbox, TRUE, TRUE, 0);
  return vbox;
#endif
}

void layout_add_widget (GtkWidget * layout, GtkWidget * child, int x_pos, int y_pos)
{
#ifdef GTK3
  gtk_layout_put (GTK_LAYOUT(layout), child, x_pos, y_pos);
#else
  gtk_fixed_put (GTK_FIXED(layout), child, x_pos, y_pos);
#endif
}

GtkWidget * add_vbox_to_layout (GtkWidget * layout, int size_x, int size_y)
{
  GtkWidget * vbox = create_vbox (BSEP);
  gtk_widget_set_size_request (vbox, size_x, size_y);
  layout_add_widget (layout, vbox, 0, 5);
  return vbox;
}

GtkWidget * create_layout (int x, int y)
{
  GtkWidget * layout;
#ifdef GTK3
  layout = gtk_layout_new (NULL, NULL);
#else
  layout = gtk_fixed_new ();
#endif
  gtk_widget_set_size_request (layout, x, y);
  return layout;
}

void combo_text_append (GtkWidget * combo, gchar * text)
{
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(combo), text);
}

void combo_text_prepend (GtkWidget * combo, gchar * text)
{
  gtk_combo_box_text_prepend_text (GTK_COMBO_BOX_TEXT(combo), text);
}

GtkWidget * create_combo ()
{
  return gtk_combo_box_text_new ();
}

void setup_text_tags (GtkTextBuffer * buffer)
{
  GtkTextTagTable * textags = gtk_text_buffer_get_tag_table(buffer);

  /* Sans font */
  tag = gtk_text_tag_new ("sans");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "family", "sans", NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* Monospace font */
  tag = gtk_text_tag_new ("monospace");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "family", "monospace", NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* Default Font size */
  tag = gtk_text_tag_new ("default-size");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* <u> */
  tag = gtk_text_tag_new ("underline");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "underline", PANGO_UNDERLINE_SINGLE, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* <uu> */
  tag = gtk_text_tag_new ("underline_double");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "underline", PANGO_UNDERLINE_DOUBLE, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* <i> italic */
  tag = gtk_text_tag_new ("italic");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "style", PANGO_STYLE_ITALIC, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* <b> bold */
  tag = gtk_text_tag_new ("bold");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "weight", PANGO_WEIGHT_BOLD, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

   /* <b> bold italic */
  tag = gtk_text_tag_new ("bold_italic");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "style", PANGO_STYLE_ITALIC,
                     "weight", PANGO_WEIGHT_BOLD, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* <sup> superscript */
  tag = gtk_text_tag_new ("sup");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "scale", .6, "rise", 6000, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* <sub> subscript */
  tag = gtk_text_tag_new ("sub");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "scale", .6, "rise", -6000, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* justify-left */
  tag = gtk_text_tag_new("justify-left");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "justification", GTK_JUSTIFY_LEFT, NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref(tag);

  /* justify-center */
  tag = gtk_text_tag_new("justify-center");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "justification", GTK_JUSTIFY_CENTER, NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref(tag);

  /* justify-right */
  tag = gtk_text_tag_new("justify-right");
  g_object_set(tag, "justification", GTK_JUSTIFY_RIGHT, NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* heading */
  tag = gtk_text_tag_new("heading");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                    "weight", PANGO_WEIGHT_BOLD,
                    "scale", 1.5,
                    "justification", GTK_JUSTIFY_CENTER,
                    "underline", PANGO_UNDERLINE_DOUBLE, NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* red font */
  tag = gtk_text_tag_new("red");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "red", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* orange font */
  tag = gtk_text_tag_new("orange");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "orange", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

   /* yellow font */
  tag = gtk_text_tag_new("yellow");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "yellow2", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

   /* green font */
  tag = gtk_text_tag_new("green");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "green", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* light green font */
  tag = gtk_text_tag_new("light_green");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "light green", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* cyan font */
  tag = gtk_text_tag_new("cyan");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "cyan", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* blue font */
  tag = gtk_text_tag_new("blue");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "blue", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* pink font */
  tag = gtk_text_tag_new("pink");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "pink", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* violet font */
  tag = gtk_text_tag_new("violet");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "foreground", "dark violet", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* grey background */
  tag = gtk_text_tag_new("grey_back");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE, "background", "lightgrey", NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* bold grey background */
  tag = gtk_text_tag_new("bold_grey_back");
  g_object_set(tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                    "background", "lightgrey",
                    "weight", PANGO_WEIGHT_BOLD, NULL);
  gtk_text_tag_table_add(textags, tag);
  g_object_unref (tag);

  /* bold red font */
  tag = gtk_text_tag_new ("bold_red");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "foreground", "red",
                     "weight", PANGO_WEIGHT_BOLD, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* bold blue font */
  tag = gtk_text_tag_new ("bold_blue");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "foreground", "blue",
                     "weight", PANGO_WEIGHT_BOLD, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* bold green font */
  tag = gtk_text_tag_new ("bold_green");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "foreground", "green",
                     "weight", PANGO_WEIGHT_BOLD, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* bold orange font */
  tag = gtk_text_tag_new ("bold_orange");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "foreground", "orange",
                     "weight", PANGO_WEIGHT_BOLD, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* bold pink font */
  tag = gtk_text_tag_new ("bold_pink");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "foreground", "pink",
                     "weight", PANGO_WEIGHT_BOLD, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

  /* subscript italic*/
  tag = gtk_text_tag_new ("sub_italic");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "style", PANGO_STYLE_ITALIC,
                     "scale", .6, "rise", -6000, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

   /* subscript bold */
  tag = gtk_text_tag_new ("sub_bold");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "weight", PANGO_WEIGHT_BOLD,
                     "scale", .6, "rise", -6000, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);

   /* superscript bold */
  tag = gtk_text_tag_new ("sup_bold");
  g_object_set (tag, "size", DEFAULT_FONT_SIZE * PANGO_SCALE,
                     "weight", PANGO_WEIGHT_BOLD,
                     "scale", .6, "rise", 6000, NULL);
  gtk_text_tag_table_add (textags, tag);
  g_object_unref (tag);
}

GtkTextBuffer * add_buffer (GCallback handler, gpointer data, gchar * text)
{
  GtkTextBuffer * buffer;
  GtkTextIter bStart;
  GtkTextIter bEnd;
  buffer = gtk_text_buffer_new (NULL);
  setup_text_tags (buffer);
  gtk_text_buffer_get_start_iter (buffer, &bStart);
  gtk_text_buffer_get_end_iter (buffer, &bEnd);
  gtk_text_buffer_delete (buffer, &bStart, &bEnd);
  if (text != NULL) gtk_text_buffer_set_text (buffer, text, -1);
  if (handler != NULL) g_signal_connect (G_OBJECT (buffer), "changed", handler, data);
  return buffer;
}

GtkWidget * create_text_view (int dimx, int dimy, int edit, int mono,
                              GCallback handler, gpointer data, gchar * text)
{
  GtkWidget * tview = gtk_text_view_new ();
  gtk_text_view_set_buffer (GTK_TEXT_VIEW(tview), add_buffer(handler, data, text));
  gtk_widget_set_size_request (tview, dimx, dimy);
  gtk_text_view_set_editable (GTK_TEXT_VIEW(tview), edit);
  if (mono) text_view_set_monospace (tview);
  return tview;
}

static void
convert_alpha (guchar * dest_data,
               int     dest_stride,
               guchar * src_data,
               int     src_stride,
               int     src_x,
               int     src_y,
               int     width,
               int     height)
{
  int x, y;

  src_data += src_stride * src_y + src_x * 4;

  for (y = 0; y < height; y++) {
    guint32 *src = (guint32 *) src_data;

    for (x = 0; x < width; x++) {
      guint alpha = src[x] >> 24;

      if (alpha == 0)
        {
          dest_data[x * 4 + 0] = 0;
          dest_data[x * 4 + 1] = 0;
          dest_data[x * 4 + 2] = 0;
        }
      else
        {
          dest_data[x * 4 + 0] = (((src[x] & 0xff0000) >> 16) * 255 + alpha / 2) / alpha;
          dest_data[x * 4 + 1] = (((src[x] & 0x00ff00) >>  8) * 255 + alpha / 2) / alpha;
          dest_data[x * 4 + 2] = (((src[x] & 0x0000ff) >>  0) * 255 + alpha / 2) / alpha;
        }
      dest_data[x * 4 + 3] = alpha;
    }

    src_data += src_stride;
    dest_data += dest_stride;
  }
}

GdkPixbuf * convert_to_pixbuf (cairo_surface_t * surf)
{
  int width  = cairo_image_surface_get_width (surf);
  int height = cairo_image_surface_get_height (surf);
  GdkPixbuf * pix = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, width , height);
  convert_alpha (gdk_pixbuf_get_pixels (pix),
                 gdk_pixbuf_get_rowstride (pix),
                 cairo_image_surface_get_data (surf),
                 cairo_image_surface_get_stride (surf),
                 0, 0, width, height);
  return pix;
}

#ifdef GTK3
G_MODULE_EXPORT gboolean to_activate_entry (GtkWidget * widg, GdkEventFocus * event, gpointer data)
{
  g_signal_emit_by_name (G_OBJECT(widg), "activate", data);
  return FALSE;
}
#else
G_MODULE_EXPORT void to_activate_entry (GtkEventControllerFocus * focus, gpointer data)
{
  g_signal_emit_by_name (G_OBJECT(gtk_event_controller_get_widget((GtkEventController *)focus)), "activate", data);
}
#endif

GtkWidget * create_entry (GCallback handler, int dim, int cdim, gboolean key_release, gpointer data)
{
  GtkWidget * entry = gtk_entry_new ();
  gtk_widget_set_size_request (entry, dim, -1);
#ifdef GTK4
  gtk_editable_set_width_chars (GTK_EDITABLE(entry), 0);
  gtk_editable_set_max_width_chars (GTK_EDITABLE(entry), cdim);
#else
  gtk_entry_set_width_chars (GTK_ENTRY(entry), 0);
  gtk_entry_set_max_width_chars (GTK_ENTRY(entry), cdim);
#endif
  gtk_entry_set_alignment (GTK_ENTRY(entry), 1.0);

  if (handler != NULL)
  {
    g_signal_connect (G_OBJECT (entry), "activate", handler, data);
#ifdef GTK3
    g_signal_connect (G_OBJECT (entry), "focus-out-event", G_CALLBACK(to_activate_entry), data);
#else
 /* Pour ajouter une icone dans l'entry:
    gtk_entry_set_icon_from_icon_name (GTK_ENTRY (entry), GTK_ENTRY_ICON_PRIMARY, "edit-find-symbolic");
    // Set up the search icon
    gtk_entry_set_icon_activatable (GTK_ENTRY (entry), GTK_ENTRY_ICON_PRIMARY, TRUE);
    gtk_entry_set_icon_sensitive (GTK_ENTRY (entry), GTK_ENTRY_ICON_PRIMARY, TRUE);
    // Then the press can have action see gtk4 demo
    g_signal_connect (entry, "icon-press", G_CALLBACK(icon_press_cb), NULL); */
    GtkEventController * focus = gtk_event_controller_focus_new ();
    gtk_event_controller_set_name (focus, "focus-out");
    g_signal_connect (G_OBJECT(focus), "leave", G_CALLBACK(to_activate_entry), data);
    gtk_widget_add_controller (entry, focus);
#endif
  }
  if (key_release)
  {
#ifdef GTK3
     g_signal_connect (G_OBJECT (entry), "key-release-event", G_CALLBACK(to_activate_entry), data);
#else
     // Key press controler
#endif
  }
  return entry;
}

GtkWidget * stock_image (const gchar * stock_id)
{
#ifdef GTK4
  return gtk_image_new_from_icon_name (stock_id);
#else
  return gtk_image_new_from_icon_name (stock_id, GTK_ICON_SIZE_MENU);
#endif
}

#ifdef GTK4
GMenuItem * create_menu_item (gboolean add_mnemo, gchar * name, gchar * action)
{
  return g_menu_item_new (name, action);
}
#else
GtkWidget * create_menu_item (gboolean add_mnemo, gchar * name)
{
  return (add_mnemo) ? gtk_menu_item_new_with_mnemonic (name) : gtk_menu_item_new_with_label (name);
}
#endif

#ifdef GTK3
GtkWidget * create_menu_item_from_widget (GtkWidget * widg, gboolean check, gboolean radio, gboolean status)
{
  GtkWidget * item;
  if (check)
  {
    item = gtk_check_menu_item_new ();
    if (radio) gtk_check_menu_item_set_draw_as_radio (GTK_CHECK_MENU_ITEM(item), TRUE);
    gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(item), status);
  }
  else
  {
    item = gtk_menu_item_new ();
  }
  gtk_container_add (GTK_CONTAINER(item), widg);
  return item;
}
#endif

/* GIcon*
g_icon_new_for_string (
  const gchar* str,
  GError** error
)

void
g_menu_item_set_icon (
  GMenuItem* menu_item,
  GIcon* icon
)

void
g_menu_item_set_attribute (
  GMenuItem* menu_item,
  const gchar* attribute,
  const gchar* format_string,
  ...
) */

GtkWidget * create_image_from_data (int format, gpointer item_image)
{
  GtkWidget * icon = NULL;
  switch (format)
  {
    case IMG_PIXBUF:
      icon = gtk_image_new_from_pixbuf ((GdkPixbuf*)item_image);
      break;
    case IMG_SURFACE:
#ifdef GTK4
      icon = gtk_image_new_from_pixbuf (convert_to_pixbuf ((cairo_surface_t *)item_image));
#else
      icon = gtk_image_new_from_surface ((cairo_surface_t *)item_image);
#endif
      break;
    case IMG_FILE:
      icon = gtk_image_new_from_file ((const gchar *)item_image);
      break;
    case IMG_STOCK:
      icon = stock_image ((const gchar *)item_image);
      break;
  }
  return icon;
}

#ifdef GTK3
GtkWidget * gtk3_menu_item (GtkWidget * menu, gchar * name,
                            int icon_format, gpointer item_icon,
                            GCallback handler, gpointer data,
                            gboolean accel, guint key, GdkModifierType mod,
                            gboolean check, gboolean radio, gboolean status)
{
  GtkWidget * item;
  GtkWidget * icon = NULL;
  GtkWidget * lab = NULL;

  if (name)
  {
    if (accel)
    {
      lab = gtk_accel_label_new (name);
      gtk_label_align (lab, 0.0, 0.5);
      gtk_widget_set_size_request (lab, -1, -1);
      gtk_label_set_use_markup (GTK_LABEL(lab), TRUE);
      gtk_accel_label_set_accel ((GtkAccelLabel *)lab, key, mod);
    }
    else
    {
      lab = markup_label(name, -1, -1, 0.0, 0.5);
    }
  }
  if (icon_format != IMG_NONE) icon = create_image_from_data (icon_format, item_icon);
  if (name)
  {
#ifdef MENU_ICONS
    if (icon)
    {
      GtkWidget * box = create_hbox (0);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, icon, FALSE, FALSE, 0);
      add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, lab, FALSE, FALSE, (icon_format == IMG_SURFACE) ? 0 : 6);
      item = create_menu_item_from_widget (box, check, radio, status);
    }
    else
    {
      item = create_menu_item_from_widget (lab, check, radio, status);
    }
#else
    item = create_menu_item_from_widget (lab, check, radio, status);
#endif
  }
  else if (icon)
  {
    item = create_menu_item_from_widget (icon, check, radio, status);
  }
  if (handler != NULL) g_signal_connect (G_OBJECT(item), "activate", handler, data);
  if (menu != NULL) add_menu_child (menu, item);
  return item;
}

GtkWidget * gtk3_image_menu_item (gchar * name, int icon_format, gpointer item_icon,
                                  GCallback handler, gpointer data, gchar * accel,
                                  gboolean check, gboolean radio, gboolean status)
{
  GtkWidget * lab = gtk_label_new (accel);
  widget_set_sensitive (lab, FALSE);
  GtkWidget * box = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, create_image_from_data (icon_format, item_icon), FALSE, FALSE, 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, box, markup_label(name, -1, -1, 0.0, 0.5), FALSE, FALSE, (icon_format == IMG_SURFACE) ? 0 : 6);
  add_box_child_end (box, lab, FALSE, FALSE, 4);
  GtkWidget * item = create_menu_item_from_widget (box, FALSE, FALSE, FALSE);
  g_signal_connect (G_OBJECT(item), "activate", handler, data);
  return item;
}

GtkWidget * add_advanced_item (GtkWidget * menu, GCallback handler, gpointer data, gboolean accel, guint key, GdkModifierType mod)
{
  return gtk3_menu_item (menu, "Advanced", IMG_STOCK, (gpointer)DPROPERTIES, handler, data, accel, key, mod, FALSE, FALSE, FALSE);
}

void add_menu_separator (GtkWidget * menu)
{
  add_menu_child (menu, gtk_separator_menu_item_new ());
}
#endif

GtkWidget * markup_label (gchar * text,
                          int dimx, int dimy,
                          float ax, float ay)
{
  GtkWidget * lab = gtk_label_new (text);
  gtk_label_align (lab, ax, ay);
  gtk_widget_set_size_request (lab, dimx, dimy);
  gtk_label_set_use_markup (GTK_LABEL(lab), TRUE);
  return lab;
}

float gdk_to_col (float val)
{
  return ((float) (val)) / 65535.0;
}

int to_gdk_col (float val)
{
  return (int) (val * 65535.0);
}

ColRGBA * duplicate_color (int num, ColRGBA * col)
{
  ColRGBA * new_col = g_malloc0 (num*sizeof*new_col);
  int i;
  for (i=0; i<num; i++) new_col[i] = col[i];
  return new_col;
}

ColRGBA gdkrgba_to_rgba (GdkRGBA colgdk)
{
  ColRGBA col;
  col.red = colgdk.red;
  col.green = colgdk.green;
  col.blue = colgdk.blue;
  col.alpha = colgdk.alpha;
  return col;
}

GdkRGBA colrgba_togtkrgba (ColRGBA col)
{
  GdkRGBA colo;
  colo.red = col.red;
  colo.green = col.green;
  colo.blue = col.blue;
  colo.alpha = col.alpha;
  return colo;
}

void set_renderer_color (int tocol, GtkCellRenderer * renderer, ColRGBA col)
{
  if (tocol)
  {
    GdkRGBA colo = colrgba_togtkrgba (col);
    g_object_set (renderer, "foreground-rgba", & colo, "foreground-set", TRUE, NULL);
    g_object_set(renderer, "weight", PANGO_WEIGHT_BOLD, "weight-set", TRUE, NULL);
  }
  else
  {
    g_object_set(renderer, "foreground-set", FALSE, "weight", FALSE, NULL);
  }
}

void button_set_image (GtkButton * but, gchar * text, int format, gpointer image)
{
#ifdef GTK4
  GtkWidget * img = create_image_from_data (format, image);
  if (text)
  {
    GtkWidget * hbox = create_hbox (5);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, img, FALSE, FALSE, 0);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label (text, -1, -1, 0.0, 0.5), FALSE, FALSE, 0);
    gtk_button_set_child (but, hbox);
  }
  else
  {
    gtk_button_set_child (but, img);
  }
#else
  gtk_button_set_image (but, create_image_from_data (format, image));
#endif
}

GtkWidget * color_button (ColRGBA col,
                          gboolean alpha,
                          int dimx, int dimy,
                          GCallback handler, gpointer data)
{
  GtkWidget * colorb;
  GdkRGBA colo = colrgba_togtkrgba (col);
  colorb = gtk_color_button_new_with_rgba (& colo);
  gtk_color_chooser_set_use_alpha (GTK_COLOR_CHOOSER(colorb), alpha);
  if (handler != NULL) g_signal_connect (G_OBJECT(colorb), "color-set", handler, data);
  gtk_widget_set_size_request (colorb, dimx, dimy);
  return colorb;
}

GtkWidget * font_button (gchar * font,
                         int dimx, int dimy,
                         GCallback handler, gpointer data)
{
  GtkWidget * fontb = gtk_font_button_new_with_font (font);
  g_signal_connect (G_OBJECT(fontb), "font-set", handler, data);
  gtk_widget_set_size_request (fontb, dimx, dimy);
  return fontb;
}

GtkWidget * spin_button (GCallback handler_a,
                         double value, double start, double end, double step, int digits,
                         int dim,
                         gpointer data)
{
  GtkWidget * spin = gtk_spin_button_new_with_range (start, end, step);
  gtk_spin_button_set_digits (GTK_SPIN_BUTTON(spin), digits);
#ifdef GTK4
  gtk_editable_set_alignment (GTK_EDITABLE(spin), 1.0);
#else
  gtk_entry_set_alignment (GTK_ENTRY(spin), 1.0);
#endif
  gtk_spin_button_set_value (GTK_SPIN_BUTTON(spin), value);
  gtk_widget_set_size_request (spin, dim, -1);
  if (handler_a != NULL) g_signal_connect (G_OBJECT (spin), "value-changed", handler_a, data);
  return spin;
}

void adjust_label (GtkWidget * lab, int dimx, int dimy, float ax, float ay)
{
  gtk_label_set_xalign (GTK_LABEL(lab), ax);
  gtk_label_set_yalign (GTK_LABEL(lab), ay);
  gtk_widget_set_size_request (lab, dimx, dimy);
  gtk_label_set_use_markup (GTK_LABEL(lab), TRUE);
}

GtkWidget * check_button (gchar * text,
                          int dimx, int dimy,
                          gboolean state,
                          GCallback handler,
                          gpointer data)
{
  GtkWidget * but = gtk_check_button_new ();
  if (text != NULL)
  {
#ifdef GTK4
    gtk_check_button_set_label (GTK_CHECK_BUTTON(but), text);
    GtkWidget * lab = gtk_widget_get_last_child (but);
    adjust_label (lab, -1, -1, 0.0, 0.5);
#else
    add_container_child (CONTAINER_BUT, but, markup_label(text, -1, -1, 0.0, 0.5));
#endif
  }
  gtk_widget_set_size_request (but, dimx, dimy);
#ifdef GTK4
  gtk_check_button_set_active (GTK_CHECK_BUTTON(but), state);
#else
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(but), state);
#endif
  if (handler != NULL) g_signal_connect (G_OBJECT(but), "toggled", handler, data);
  return but;
}

GtkWidget * radio_button (gchar * text,
                          int dimx, int dimy,
                          gboolean state,
                          GCallback handler,
                          gpointer data)
{
  GtkWidget * but;
#ifdef GTK4
  but = gtk_toggle_button_new ();
#else
  but = gtk_radio_button_new (NULL);
#endif
  if (text != NULL) add_container_child (CONTAINER_BUT, but, markup_label(text, -1, -1, 0.0, 0.5));
  gtk_widget_set_size_request (but, dimx, dimy);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(but), state);
  if (handler != NULL) g_signal_connect (G_OBJECT(but), "toggled", handler, data);
  return but;
}

GtkWidget * create_button (gchar * text,
                           int image_format,
                           gchar * image,
                           int dimx, int dimy,
                           int relief,
                           GCallback handler,
                           gpointer data)
{
  GtkWidget * but;
#ifdef GTK3
  but = (text) ? gtk_button_new_with_label (text) : gtk_button_new ();
#else
  but = (text && image_format == IMG_NONE) ? gtk_button_new_with_label (text) : gtk_button_new ();
#endif
  if (image_format != IMG_NONE) button_set_image (GTK_BUTTON(but), text, image_format, (gpointer)image);
  gtk_widget_set_size_request (but, dimx, dimy);
#ifdef GTK3
  gtk_button_set_relief (GTK_BUTTON(but), relief);
#endif
#ifdef GTK4
  if (relief == GTK_RELIEF_NONE) gtk_button_set_has_frame (GTK_BUTTON(but), FALSE);
#endif
  if (handler != NULL)
  {
    g_signal_connect (G_OBJECT (but), "clicked", handler, data);
  }
  return but;
}

void set_image_from_icon_name (GtkWidget * widg, gchar * icon)
{
#ifdef GTK4
  gtk_image_set_from_icon_name (GTK_IMAGE(widg), icon);
#else
  gtk_image_set_from_icon_name (GTK_IMAGE(widg), icon, GTK_ICON_SIZE_BUTTON);
#endif
}

GtkWidget * abox (GtkWidget * box, char * lab, int vspace)
{
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box, hbox, FALSE, FALSE, vspace);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label("<b>.</b>", 5, -1, 0.0, 0.5), FALSE, FALSE, 10);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label(lab, 150, 30, 0.0, 0.5), FALSE, FALSE, 0);
  return hbox;
}

GtkWidget * bbox (GtkWidget * box, char * lab)
{
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box, hbox, FALSE, FALSE, 2);
  GtkWidget * hhbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, hhbox, FALSE, FALSE, 40);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hhbox, markup_label(lab, 150, 30, 0.0, 0.5), FALSE, FALSE, 0);
  return hhbox;
}

GtkWidget * cbox (GtkWidget * box, char * lab)
{
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box, hbox, TRUE, TRUE, 2);
  GtkWidget * hhbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, hhbox, TRUE, TRUE, 40);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hhbox, markup_label(lab, -1, 30, 0.0, 0.5), TRUE, TRUE, 40);
  return hhbox;
}

GtkWidget * fbox (GtkWidget * box, char * lab)
{
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, box, hbox, FALSE, FALSE, 0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label("<b>.</b>", 5, -1, 0.0, 0.5), FALSE, FALSE, 10);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label(lab, 350, 30, 0.0, 0.5), FALSE, FALSE, 0);
  return hbox;
}

GtkWidget * create_scroll (GtkWidget * box, int dimx, int dimy, int shadow)
{
  GtkWidget * scroll;
#ifdef GTK4
  scroll = gtk_scrolled_window_new ();
#else
  scroll = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW(scroll), shadow);
#endif
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(scroll), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_size_request (scroll, dimx, dimy);
  // To check all create_scroll !
  if (box != NULL) add_box_child_start (GTK_ORIENTATION_VERTICAL, box, scroll, TRUE, TRUE, 0);
  return scroll;
}

GtkWidget * create_expander (gchar * name, gchar * file_img, int i)
{
  GtkWidget * expand = gtk_expander_new (name);
  GtkWidget * hbox = create_hbox (0);
  if (file_img != NULL)
  {
    GtkWidget * img = gtk_image_new_from_file (file_img);
    gtk_widget_set_size_request (img, 20, 20);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, img, TRUE, TRUE, 10);
  }
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, markup_label(name, 200, 20, 0.0, 0.5), FALSE, TRUE, 0);
  gtk_expander_set_label_widget (GTK_EXPANDER(expand), hbox);
  return expand;
}

void provide_gtk_css (gchar * css)
{
  GtkCssProvider * provider = gtk_css_provider_new ();
#ifdef GTK4
  gtk_css_provider_load_from_data (provider, css, -1);
  gtk_style_context_add_provider_for_display (gdk_display_get_default (),
                                              GTK_STYLE_PROVIDER(provider),
                                              GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
#else
  gtk_css_provider_load_from_data (provider, css, -1, NULL);
  gtk_style_context_add_provider_for_screen (gdk_screen_get_default (),
                                             GTK_STYLE_PROVIDER(provider),
                                             GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
#endif
  g_object_unref (provider);
}

GtkWidget * destroy_this_widget (GtkWidget * widg)
{
  if (widg != NULL)
  {
    if (GTK_IS_WIDGET(widg))
    {
      if (is_the_widget_visible(widg)) gtk_widget_hide (widg);
#ifdef GTK3
      gtk_widget_destroy (widg);
#else
      GtkWidget * wid = gtk_widget_get_parent (widg);
      if (wid != NULL)
      {
        if (GTK_IS_WIDGET(wid))
        {
          gtk_widget_unparent (widg);
        }
      }
#endif
    }
  }
  return NULL;
}

void destroy_this_dialog (GtkDialog * dialog)
{
#ifdef GTK4
  gtk_window_destroy (GTK_WINDOW(dialog));
#else
  destroy_this_widget (GTK_WIDGET(dialog));
#endif
  g_main_loop_quit (Event_loop[dialog_id]);
  g_main_loop_unref (Event_loop[dialog_id]);
  dialog_id --;
}

void destroy_this_native_dialog (GtkNativeDialog * dialog)
{
  gtk_native_dialog_destroy (dialog);
  g_object_unref (dialog);
  g_main_loop_quit (Event_loop[dialog_id]);
  g_main_loop_unref (Event_loop[dialog_id]);
  dialog_id --;
}

G_MODULE_EXPORT void run_destroy_dialog (GtkDialog * dialog, gint response_id, gpointer data)
{
  destroy_this_dialog (dialog);
}

#ifdef GTK4
GListModel * file_chooser_get_file_names (GtkFileChooser * chooser)
{
  return gtk_file_chooser_get_files (chooser);
}
#else
GSList * file_chooser_get_file_names (GtkFileChooser * chooser)
{
  return gtk_file_chooser_get_filenames (chooser);
}
#endif

gchar * file_chooser_get_file_name (GtkFileChooser * chooser)
{
#ifdef GTK4
  return g_file_get_parse_name (gtk_file_chooser_get_file (chooser));
#else
  return gtk_file_chooser_get_filename (chooser);
#endif
}

gchar * file_chooser_get_current_folder (GtkFileChooser * chooser)
{
#ifdef GTK4
  return g_file_get_parse_name (gtk_file_chooser_get_current_folder (chooser));
#else
  return gtk_file_chooser_get_current_folder (chooser);
#endif
}

gboolean file_chooser_set_file_name (GtkFileChooser * chooser, gchar * filename)
{
  GFile * default_file_for_saving = g_file_new_for_path (filename);
  gboolean res = gtk_file_chooser_set_file (chooser, default_file_for_saving, NULL);
  if (! res)
  {
    gchar * str = g_strdup_printf ("Impossible to locate file: %s", filename);
    show_error (str, 0, (GtkWidget *)chooser);
  }
  return res;
}

void file_chooser_set_current_folder (GtkFileChooser * chooser)
{
#ifdef GTK4
  gtk_file_chooser_set_current_folder (chooser, g_file_new_for_path ("./"), NULL);
#else
  gtk_file_chooser_set_current_folder (chooser, "");
#endif
}

#ifdef GTK4
GtkFileChooserNative * create_file_chooser (const gchar * title, GtkWindow * parent, GtkFileChooserAction act, const gchar * act_name)
{
  return gtk_file_chooser_native_new (title, parent, act, act_name, "Cancel");
}
#else
GtkWidget * create_file_chooser (const gchar * title, GtkWindow * parent, GtkFileChooserAction act, const gchar * act_name)
{
  return gtk_file_chooser_dialog_new (title, parent, act, "Cancel", GTK_RESPONSE_CANCEL, act_name, GTK_RESPONSE_ACCEPT, NULL);
}
#endif

ColRGBA get_button_color (GtkColorChooser * colob)
{
  GdkRGBA col;
  gtk_color_chooser_get_rgba (colob, & col);
  return gdkrgba_to_rgba (col);
}

ColRGBA get_window_color (GtkWidget * color_win)
{
  GdkRGBA col;
  gtk_color_chooser_get_rgba (GTK_COLOR_CHOOSER(color_win), & col);
  return gdkrgba_to_rgba (col);
}

void set_color_chooser_color (GtkWidget * color_win, ColRGBA col)
{
  GdkRGBA colo = colrgba_togtkrgba(col);
  gtk_color_chooser_set_rgba (GTK_COLOR_CHOOSER(color_win), & colo);
}

#ifdef GTK4
void pop_menu_at_pointer (GtkWidget * pop, double x, double y)
{
  GdkRectangle rect;
  rect.x = x;
  rect.y = y;
  rect.width = 1;
  rect.height = 1;
  gtk_popover_set_has_arrow (GTK_POPOVER(pop), FALSE);
  gtk_popover_set_pointing_to (GTK_POPOVER(pop), & rect);
  gtk_popover_popup (GTK_POPOVER(pop));
}
#else
void pop_menu_at_pointer (GtkWidget * widg, GdkEvent * event)
{
  show_the_widgets (widg);
  gtk_menu_popup_at_pointer (GTK_MENU (widg), event);
}
#endif

GtkWidget * get_top_level (GtkWidget * widg)
{
#ifdef GTK4
  return (GtkWidget *)gtk_widget_get_root (widg);
#else
  return gtk_widget_get_toplevel (widg);
#endif
}

#ifdef GTK4
G_MODULE_EXPORT gboolean destroy_this_window (GtkWindow * win, gpointer data)
#else
G_MODULE_EXPORT gboolean destroy_this_window (GtkWidget * win, GdkEvent * event, gpointer data)
#endif
{
  destroy_this_widget (GTK_WIDGET(win));
  return TRUE;
}

#ifdef GTK4
G_MODULE_EXPORT gboolean hide_this_window (GtkWindow * win, gpointer data)
#else
G_MODULE_EXPORT gboolean hide_this_window (GtkWidget * win, GdkEvent * event, gpointer data)
#endif
{
  gtk_widget_hide (GTK_WIDGET(win));
  return TRUE;
}

void add_gtk_close_event (GtkWidget * widg, GCallback handler, gpointer data)
{
#ifdef GTK4
  g_signal_connect (G_OBJECT (widg), "close-request", handler, data);
#else
  g_signal_connect (G_OBJECT (widg), "delete-event", handler, data);
#endif
}
