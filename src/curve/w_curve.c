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
* @file w_curve.c
* @short Functions to create the graph/curve window
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'w_curve.c'
*
* Contains:
*

 - The functions to create the graph/curve window

*
* List of functions:

  int get_curve_shift (project * this_proj, int b, int c);

  G_MODULE_EXPORT gboolean view_curve_popup (GtkWidget * widget, gpointer data);
  G_MODULE_EXPORT gboolean on_motion_notify_event (GtkWidget * widget, GdkEventMotion * event, gpointer data);
  G_MODULE_EXPORT gboolean on_curve_button_event (GtkWidget * widget, GdkEvent * event, gpointer data);
  G_MODULE_EXPORT gboolean on_curve_key_pressed (GtkWidget * widg, GdkEventKey * event, gpointer data);
  G_MODULE_EXPORT gboolean on_curve_key_pressed (GtkEventControllerKey * self, guint keyval, guint keycode, GdkModifierType state, gpointer data);

  void curve_zoom_in_out (gboolean state, gdouble event_x, gdouble event_y, gpointer data);
  void curve_button_event (double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data);
  void curve_button_event (GdkEvent * event, double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data);
  void curve_key_pressed (guint keyval, GdkModifierType state, gpointer data);

  static void on_curve_pointer_motion (GtkEventControllerMotion * motion, gdouble x, gdouble y, gpointer data);

  G_MODULE_EXPORT void on_curve_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
  G_MODULE_EXPORT void on_curve_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data);
  G_MODULE_EXPORT void on_curve_realize (GtkWidget * widg, gpointer data);

  GtkWidget * create_curve (tint * data);

*/

#include <stdlib.h>
#include <math.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <cairo.h>

#include "global.h"
#include "callbacks.h"
#include "curve.h"
#include "cedit.h"
#include "datab.h"

extern void autoscale (gpointer data);
extern void curve_menu_bar_action (GSimpleAction * action, GVariant * parameter, gpointer data);

int curve_action_id = -1;
int activeg = 0;
int activec = 0;
int activer = 0;

#ifdef GTK3
/*!
  \fn G_MODULE_EXPORT gboolean view_curve_popup (GtkWidget * widget, gpointer data)

  \brief show curve popup menu GTK3

  \param widget the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean view_curve_popup (GtkWidget * widget, gpointer data)
{
  pop_menu_at_pointer (curve_popup_menu(data), NULL);
  return TRUE;
}
#endif

/*!
  \fn void curve_zoom_in_out (gboolean state, gdouble event_x, gdouble event_y, gpointer data)

  \brief curve zoom in or out

  \param state mouse button state (pressed or released)
  \param event_x x position
  \param event_y y position
  \param data the associated data pointer
*/
void curve_zoom_in_out (gboolean state, gdouble event_x, gdouble event_y, gpointer data)
{
  int a, b, c;
  int x, y;
  double r, g, d;
  double tmp, xp, yp;
  gchar * str;
  gint width, height;
  CurveState * cstate = (CurveState *)data;
  a = cstate -> id -> a;
  b = cstate -> id -> b;
  c = cstate -> id -> c;
  project * this_proj = get_project_by_id(a);
  int curve_shift = get_curve_shift (this_proj,b,c);
#ifdef GTK4
  event_y -= (double) curve_shift;
#endif
  if (state && cstate -> mouseState.MouseIsDown)
  {
    cairo_t * rec;
    cairo_region_t * reg;
#ifdef GTK3
    GdkWindow * win = gtk_widget_get_window (this_proj -> curves[b][c] -> plot);
    reg = gdk_window_get_visible_region (win);
    GdkDrawingContext * curve_context = gdk_window_begin_draw_frame (win, reg);
    if (gdk_drawing_context_is_valid (curve_context))
    {
      rec = gdk_drawing_context_get_cairo_context (curve_context);
      if (event_x >= x_min && event_x <= x_max && event_y <= y_min && event_y >= y_max)
#else
    GtkNative * native = gtk_widget_get_native (this_proj -> curves[b][c] -> plot);
    GdkSurface * surf = gtk_native_get_surface (native);
    cairo_surface_t * csurf = cairo_surface_create_for_rectangle (this_proj -> curves[b][c] -> surface, 0.0, (double)curve_shift,
                                                                  (double)gtk_widget_get_width(this_proj -> curves[b][c] -> plot),
                                                                  (double)gtk_widget_get_height(this_proj -> curves[b][c] -> plot));
    reg = gdk_cairo_region_create_from_surface (csurf);
    GdkDrawContext * curve_context = (GdkDrawContext *) gdk_surface_create_cairo_context (surf);
    gdk_draw_context_begin_frame (curve_context, reg);
    if (gdk_draw_context_is_in_frame (curve_context))
    {
       rec = gdk_cairo_context_cairo_create ((GdkCairoContext *)curve_context);
       if (event_x >= x_min && event_x <= x_max && event_y <= y_min+(double)curve_shift && event_y >= y_max)
#endif
      {
        width  = event_x - cstate -> mouseState.start_x;
        height = event_y - cstate -> mouseState.start_y;
#ifdef GTK3
        cairo_set_source_surface (rec, this_proj -> curves[b][c] -> surface, 0, -curve_shift);
#else
        height += (double) curve_shift;
        cairo_set_source_surface (rec, this_proj -> curves[b][c] -> surface, 0, +curve_shift);
#endif
        cairo_paint (rec);
        if (event_x < cstate -> mouseState.start_x)
        {
          r=0.0;
          x = cstate -> mouseState.start_x + 2;
          if (event_y < cstate -> mouseState.start_y)
          {
            g=0.0;
            d=1.0;
            y = cstate -> mouseState.start_y + 8;
            str = g_strdup_printf ("zoom: out (x) / in (y)");
          }
          else
          {
            g=1.0;
            d=0.0;
            y = cstate -> mouseState.start_y - 4;
            str = g_strdup_printf ("zoom: out (x) / out (y)");
          }
        }
        else
        {
          r=1.0;
          x = cstate -> mouseState.start_x - 100;
          if (event_y < cstate -> mouseState.start_y)
          {
            g=0.0;
            d=0.0;
            y = cstate -> mouseState.start_y + 8;
            str = g_strdup_printf ("zoom: in (x) / in (y)");
          }
          else
          {
            g=0.0;
            d=1.0;
            y = cstate -> mouseState.start_y - 4;
            str = g_strdup_printf ("zoom: in (x) / out (y)");
          }
        }
        cairo_set_source_rgba (rec, r, g, d, 0.05);
        cairo_rectangle (rec, cstate -> mouseState.start_x, cstate -> mouseState.start_y, width, height);
	    cairo_fill (rec);
        cairo_set_source_rgba (rec, r, g, d, 1.0);
        cairo_set_line_width (rec, 1.0);
        cairo_move_to (rec, cstate -> mouseState.start_x, cstate -> mouseState.start_y);
#ifdef GTK4
        event_y += (double) curve_shift;
#endif
        cairo_line_to (rec, cstate -> mouseState.start_x, event_y);
        cairo_line_to (rec, event_x, event_y);
        cairo_line_to (rec, event_x, cstate -> mouseState.start_y);
        cairo_line_to (rec, cstate -> mouseState.start_x, cstate -> mouseState.start_y);
        cairo_stroke (rec);
        if (abs(width) > 10 && abs(height) > 5)
        {
          cairo_move_to (rec, x, y);
          cairo_show_text (rec, str);
        }
        g_free (str);
        width  = event_x - x_min;
        tmp =  this_proj -> curves[b][c] -> axmax[0] - this_proj -> curves[b][c] -> axmin[0];
        xp = this_proj -> curves[b][c] -> axmin[0] + width * tmp / XDRAW;
#ifdef GTK4
        event_y -= (double) curve_shift;
#endif
        height = event_y - y_max;
        tmp =  this_proj -> curves[b][c] -> axmax[1] - this_proj -> curves[b][c] -> axmin[1];
        yp = this_proj -> curves[b][c] -> axmax[1] + height * tmp / YDRAW;
        str = g_strdup_printf ("(x= %f, y= %f)", xp, yp);
      }
      else
      {
        str = g_strdup_printf ("(Not in plot)");
      }
      gtk_label_set_text (GTK_LABEL(this_proj -> curves[b][c] -> pos), str);
      g_free (str);
    }
#ifdef GTK3
    gdk_window_end_draw_frame (win, curve_context);
#else
    gdk_draw_context_end_frame (curve_context);
#endif
  }
  else if (! cstate -> mouseState.MouseIsDown)
  {
#ifdef GTK3
    // gtk_widget_get_size_request (this_proj -> curves[b][c] -> plot, & this_proj -> curves[b][c] -> wsize[0], & this_proj -> curves[b][c] -> wsize[1]);
    gtk_window_get_size (GTK_WINDOW(this_proj -> curves[b][c] -> window),
                           & this_proj -> curves[b][c] -> wsize[0],
                           & y);
    this_proj -> curves[b][c] -> wsize[1] = y - curve_shift;
#else
    this_proj -> curves[b][c] -> wsize[0] = gtk_widget_get_width (this_proj -> curves[b][c] -> plot);
    this_proj -> curves[b][c] -> wsize[1] = gtk_widget_get_height (this_proj -> curves[b][c] -> plot);
#endif
    prep_plot (this_proj, b, c);
#ifdef GTK4
    if (event_x >= x_min && event_x <= x_max && event_y <= y_min+(double)curve_shift && event_y >= y_max)
#else
    if (event_x >= x_min && event_x <= x_max && event_y <= y_min && event_y >= y_max)
#endif
    {
      width  = event_x - x_min;
      tmp =  this_proj -> curves[b][c] -> axmax[0] - this_proj -> curves[b][c] -> axmin[0];
      xp = this_proj -> curves[b][c] -> axmin[0] + width * tmp / XDRAW;
      height = event_y - y_max;
      tmp =  this_proj -> curves[b][c] -> axmax[1] - this_proj -> curves[b][c] -> axmin[1];
      yp = this_proj -> curves[b][c] -> axmax[1] + height * tmp / YDRAW;
      str = g_strdup_printf ("(x= %f, y= %f)", xp, yp);
    }
    else
    {
      str = g_strdup_printf ("Not in plot");
    }
    gtk_label_set_text (GTK_LABEL(this_proj -> curves[b][c] -> pos), str);
    g_free (str);
  }
}

#ifdef GTK3
/*!
  \fn G_MODULE_EXPORT gboolean on_motion_notify_event (GtkWidget * widget, GdkEventMotion * event, gpointer data)

  \brief handle mouse motion event in the OpenGL window GTK3

  \param widget the GtkWidget sending the signal
  \param event the GdkEvent triggering the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean on_motion_notify_event (GtkWidget * widget, GdkEventMotion * event, gpointer data)
{
  curve_zoom_in_out ((event -> state & GDK_BUTTON1_MASK) ? TRUE : FALSE, event -> x, event -> y, data);
  return TRUE;
}
#else
/*!
  \fn static void on_curve_pointer_motion (GtkEventControllerMotion * motion, gdouble x, gdouble y, gpointer data)

  \brief handle mouse motion event in the curve window GTK4

  \param motion The GtkEvenController sending the signal
  \param x x position
  \param y y position
  \param data the associated data pointer
*/
static void on_curve_pointer_motion (GtkEventControllerMotion * motion, gdouble x, gdouble y, gpointer data)
{
  curve_zoom_in_out (((CurveState *)data) -> mouseState.MouseIsDown, x, y, data);
}
#endif

#ifdef GTK4
/*!
  \fn void curve_button_event (double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data)

  \brief handle mouse button event on the curve window GTK4

  \param event_x x position
  \param event_y y position
  \param event_button event button
  \param event_type event type
  \param event_time event time
  \param data the associated data pointer
*/
void curve_button_event (double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data)
#else
/*!
  \fn void curve_button_event (GdkEvent * event, double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data)

  \brief handle mouse button event on the curve window GTK3

  \param event the GdkEvent triggering the signal
  \param event_x x position
  \param event_y y position
  \param event_button event button
  \param event_type event type
  \param event_time event time
  \param data the associated data pointer
*/
void curve_button_event (GdkEvent * event, double event_x, double event_y, guint event_button, guint event_type, guint32 event_time, gpointer data)
#endif
{
  int x1, x2, y1, y2;
  double tmp;
  int a, b, c;
  CurveState * cstate = (CurveState *)data;
  a = activeg = cstate -> id -> a;
  b = activer = cstate -> id -> b;
  c = activec = cstate -> id -> c;
  project * this_proj = get_project_by_id(a);
#ifdef GTK4
  int curve_shift = get_curve_shift (this_proj, b, c);
#endif
  if (event_type == GDK_BUTTON_PRESS)
  {
    if (event_button == 1)
    {
#ifdef GTK3
      /*
        The following is not working:
          gtk_widget_get_size_request (this_proj -> curves[b][c] -> plot, & this_proj -> curves[b][c] -> wsize[0], & this_proj -> curves[b][c] -> wsize[1]);
      */
      gtk_window_get_size (GTK_WINDOW(this_proj -> curves[b][c] -> window),
                           & this_proj -> curves[b][c] -> wsize[0],
                           & y1);
      this_proj -> curves[b][c] -> wsize[1] = y1 - get_curve_shift (this_proj, b, c);
#else
      this_proj -> curves[b][c] -> wsize[0] = gtk_widget_get_width (this_proj -> curves[b][c] -> plot);
      this_proj -> curves[b][c] -> wsize[1] = gtk_widget_get_height (this_proj -> curves[b][c] -> plot);
#endif
      prep_plot (this_proj, b, c);
#ifdef GTK4
      if (event_x >= x_min && event_x <= x_max && event_y <= y_min+(double)curve_shift && event_y >= y_max)
#else
      if (event_x >= x_min && event_x <= x_max && event_y <= y_min && event_y >= y_max)
#endif
      {
        cstate -> mouseState.start_x = event_x;
        cstate -> mouseState.start_y = event_y;
        cstate -> mouseState.time = event_time;
        cstate -> mouseState.MouseIsDown = TRUE;
      }
    }
    else if (event_button == 3)
    {
#ifdef GTK4
      pop_menu_at_pointer (curve_popup_menu(data), event_x, event_y);
#else
      pop_menu_at_pointer (curve_popup_menu(data), event);
#endif
    }
  }
  else if (event_type == GDK_BUTTON_RELEASE)
  {
    cstate -> mouseState.MouseIsDown = FALSE;
    if (event_button == 1)
    {
      etime = event_time - cstate -> mouseState.time;
      if (event_x >= x_min && event_x <= x_max && event_y <= y_min && event_y >= y_max)
      {
        if (event_x != cstate ->  mouseState.start_x && event_y != cstate -> mouseState.start_y)
        {
          if (etime > 500 && etime < 50000)
          {
            x1 = cstate ->  mouseState.start_x - x_min;
            y1 = cstate ->  mouseState.start_y - y_max;
            x2 = event_x - x_min;
            y2 = event_y - y_max;
#ifdef GTK4
            y1 -= curve_shift;
            y2 -= curve_shift;
#endif
            tmp =  this_proj -> curves[b][c] -> axmax[0] - this_proj -> curves[b][c] -> axmin[0];
            if (x2 > x1)
            {
            // zoom-in on X
              this_proj -> curves[b][c] -> axmax[0] = this_proj -> curves[b][c] -> axmin[0] + x2 * tmp / XDRAW;
              this_proj -> curves[b][c] -> axmin[0] = this_proj -> curves[b][c] -> axmin[0] + x1 * tmp / XDRAW;
            }
            else
            {
            // zoom-out on X
              this_proj -> curves[b][c] -> axmin[0] = this_proj -> curves[b][c] -> axmin[0] - (x1 - x2) * tmp / XDRAW;
              this_proj -> curves[b][c] -> axmax[0] = this_proj -> curves[b][c] -> axmax[0] + (x1 - x2) * tmp / XDRAW;
            }
            tmp =  this_proj -> curves[b][c] -> axmax[1] - this_proj -> curves[b][c] -> axmin[1];
            if (y1 > y2)
            {
            // zoom-in on Y
              this_proj -> curves[b][c] -> axmin[1] = this_proj -> curves[b][c] -> axmax[1] + y1 * tmp / YDRAW;
              this_proj -> curves[b][c] -> axmax[1] = this_proj -> curves[b][c] -> axmax[1] + y2 * tmp / YDRAW;
            }
            else
            {
              // zoom-out on Y
              this_proj -> curves[b][c] -> axmin[1] = this_proj -> curves[b][c] -> axmin[1] + (y2 - y1) * tmp / YDRAW;
              this_proj -> curves[b][c] -> axmax[1] = this_proj -> curves[b][c] -> axmax[1] - (y2 - y1) * tmp / YDRAW;
            }
          }
        }
      }
      tint id;
      id.a = a;
      id.b = b;
      id.c = c;
      update_curve ((gpointer)& id);
    }
  }
}

#ifdef GTK4
/*!
  \fn G_MODULE_EXPORT void on_curve_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)

  \brief mouse button pressed signal on the curve window

  \param gesture the GtkGesture sending the signal
  \param n_press number of times it was pressed
  \param x x position
  \param y y position
  \param data the associated data pointer
*/
G_MODULE_EXPORT void on_curve_button_pressed (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  curve_button_event (x, y, gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), GDK_BUTTON_PRESS, gtk_event_controller_get_current_event_time((GtkEventController *)gesture), data);
}

/*!
  \fn G_MODULE_EXPORT void on_curve_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data)

  \brief mouse button released signal on the curve window

  \param gesture the GtkGesture sending the signal
  \param n_press number of times it was pressed
  \param x x position
  \param y y position
  \param data the associated data pointer
*/
G_MODULE_EXPORT void on_curve_button_released (GtkGesture * gesture, int n_press, double x, double y, gpointer data)
{
  curve_button_event (x, y, gtk_gesture_single_get_current_button ((GtkGestureSingle * )gesture), GDK_BUTTON_RELEASE, gtk_event_controller_get_current_event_time((GtkEventController *)gesture), data);
}
#else
/*!
  \fn G_MODULE_EXPORT gboolean on_curve_button_event (GtkWidget * widget, GdkEvent * event, gpointer data)

  \brief mouse button event on the curve window

  \param widget the GtkWidget sending the signal
  \param event the GdkEvent triggering the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean on_curve_button_event (GtkWidget * widget, GdkEvent * event, gpointer data)
{
  GdkEventButton * bevent = (GdkEventButton *)event;
  curve_button_event (event, bevent -> x, bevent -> y, bevent -> button, bevent -> type, bevent -> time, data);
  return TRUE;
}
#endif

/*!
  \fn void curve_key_pressed (guint keyval, GdkModifierType state, gpointer data)

  \brief the keyboard shortcut actions for the curve window

  \param keyval the key pressed
  \param state the keyboard modifier
  \param data the associated data pointer
*/
void curve_key_pressed (guint keyval, GdkModifierType state, gpointer data)
{
  if (state & GDK_CONTROL_MASK)
  {
    switch (keyval)
    {
      case GDK_KEY_a:
        autoscale (data);
        break;
      case GDK_KEY_c:
        hide_curve (data);
        break;
      case GDK_KEY_e:
        edit_curve (data);
        break;
      case GDK_KEY_i:
        save_image (data);
        break;
      case GDK_KEY_s:
        write_curve (data);
        break;
    }
  }
}

#ifdef GTK3
/*!
  \fn G_MODULE_EXPORT gboolean on_curve_key_pressed (GtkWidget * widg, GdkEventKey * event, gpointer data)

  \brief keyboard key press event for the curve window GTK3

  \param widg the GtkWidget sending the signal
  \param event the GdkEventKey triggering the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean on_curve_key_pressed (GtkWidget * widg, GdkEventKey * event, gpointer data)
{
  if (event -> type == GDK_KEY_PRESS)
  {
    //GdkModifierType accel_mask = gtk_accelerator_get_default_mod_mask ();
    // if ((event -> state & accel_mask) == GDK_CONTROL_MASK)
    curve_key_pressed (event -> keyval, event -> state, data);
  }
  return FALSE;
}
#else
/*!
  \fn G_MODULE_EXPORT gboolean on_curve_key_pressed (GtkEventControllerKey * self, guint keyval, guint keycode, GdkModifierType state, gpointer data)

  \brief keyboard key press event for the curve window GTK4

  \param self the GtkEventControllerKey sending the signal
  \param keyval number of times it was pressed
  \param keycode the key pressed
  \param state the keyboard modifier
  \param data the associated data pointer
*/
G_MODULE_EXPORT gboolean on_curve_key_pressed (GtkEventControllerKey * self, guint keyval, guint keycode, GdkModifierType state, gpointer data)
{
  curve_key_pressed (keyval, state, data);
  return TRUE;
}
#endif

/*!
  \fn int get_curve_shift (project * this_proj, int b, int c)

  \brief get cruve window size shift

  \param this_proj the target project
  \param b the calculation id
  \param c the curve id
*/
int get_curve_shift (project * this_proj, int b, int c)
{
  return get_widget_height (this_proj -> curves[b][c] -> window) - get_widget_height (this_proj -> curves[b][c] -> plot);
}

/*!
  \fn G_MODULE_EXPORT void on_curve_realize (GtkWidget * widg, gpointer data)

  \brief curve window realize callback

  \param widg the GtkWidget sending the signal
  \param data the associated data pointer
*/
G_MODULE_EXPORT void on_curve_realize (GtkWidget * widg, gpointer data)
{
  tint * id = (tint *)data;
  project * this_proj = get_project_by_id(id -> a);
  resize_this_window (this_proj -> curves[id -> b][id -> c] -> window,
                      this_proj -> curves[id -> b][id -> c] -> wsize[0],
                      this_proj -> curves[id -> b][id -> c] -> wsize[1]+get_curve_shift (this_proj, id -> b, id -> c));
}

/*!
  \fn GtkWidget * create_curve (tint * data)

  \brief create the curve data plot window

  \param data the associated data pointer
*/
GtkWidget * create_curve (tint * data)
{
  GtkWidget * Curve, * vbox;

  activec = data -> c;
  project * this_proj = get_project_by_id(data -> a);
  gchar * str = g_strdup_printf ("%s - %s", prepare_for_title (this_proj -> name), this_proj -> curves[data -> b][data -> c] -> name);
  Curve = create_win (str, MainWindow, FALSE, TRUE);
  activer = data -> b;
  g_free (str);
  vbox = create_vbox (BSEP);
  add_container_child (CONTAINER_WIN, Curve, vbox);
  this_proj -> curves[data -> b][data -> c] -> curve_vbox = create_vbox (BSEP);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, this_proj -> curves[data -> b][data -> c] -> curve_vbox, FALSE, FALSE, 0);

  curve_action_id ++;
  this_proj -> curves[data -> b][data -> c] -> action_id = curve_action_id;
  this_proj -> curves[data -> b][data -> c] -> action_group = g_simple_action_group_new ();
  GSimpleAction * curve_action[5];
  curve_action[0] = g_simple_action_new ("save.data", NULL);
  curve_action[1] = g_simple_action_new ("close.curve", NULL);
  curve_action[2] = g_simple_action_new ("edit.curve", NULL);
  curve_action[3] = g_simple_action_new ("save.image", NULL);
  curve_action[4] = g_simple_action_new ("shortcuts.curve", NULL);
  int i;
  for (i=0; i<5; i++)
  {
    g_action_map_add_action (G_ACTION_MAP(this_proj -> curves[data -> b][data -> c] -> action_group), G_ACTION(curve_action[i]));
    g_signal_connect (curve_action[i], "activate", G_CALLBACK(curve_menu_bar_action), data);
  }
  str = g_strdup_printf ("c-%d", this_proj -> curves[data -> b][data -> c] -> action_id);
  gtk_widget_insert_action_group (Curve, str, G_ACTION_GROUP(this_proj -> curves[data -> b][data -> c] -> action_group));
  g_free (str);
  curve_window_add_menu_bar (data);

  this_proj -> curves[data -> b][data -> c] -> datatree = NULL;
  this_proj -> curves[data -> b][data -> c] -> state.id = data;
  this_proj -> curves[data -> b][data -> c] -> plot = gtk_drawing_area_new ();
  gtk_widget_set_size_request (this_proj -> curves[data -> b][data -> c] -> plot, 100, 100);
  gtk_widget_set_hexpand (this_proj -> curves[data -> b][data -> c] -> plot, TRUE);
  gtk_widget_set_vexpand (this_proj -> curves[data -> b][data -> c] -> plot, TRUE);
#ifdef GTK3
  gtk_widget_add_events (GTK_WIDGET (this_proj -> curves[data -> b][data -> c] -> plot),
                         GDK_EXPOSURE_MASK | GDK_SMOOTH_SCROLL_MASK |
                         GDK_POINTER_MOTION_MASK | GDK_POINTER_MOTION_HINT_MASK |
                         GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
  g_signal_connect (G_OBJECT(this_proj -> curves[data -> b][data -> c] -> plot), "motion_notify_event", G_CALLBACK(on_motion_notify_event), & this_proj -> curves[data -> b][data -> c] -> state);
  g_signal_connect (G_OBJECT(this_proj -> curves[data -> b][data -> c] -> plot), "button_press_event", G_CALLBACK(on_curve_button_event), & this_proj -> curves[data -> b][data -> c] -> state);
  g_signal_connect (G_OBJECT(this_proj -> curves[data -> b][data -> c] -> plot), "button_release_event", G_CALLBACK(on_curve_button_event),& this_proj -> curves[data -> b][data -> c] -> state);
  g_signal_connect (G_OBJECT(this_proj -> curves[data -> b][data -> c] -> plot), "popup-menu", G_CALLBACK(view_curve_popup), & this_proj -> curves[data -> b][data -> c] -> state);
#else
  add_widget_gesture_and_key_action (Curve, "curve-button-pressed", G_CALLBACK(on_curve_button_pressed), & this_proj -> curves[data -> b][data -> c] -> state,
                                            "curve-button-released", G_CALLBACK(on_curve_button_released), & this_proj -> curves[data -> b][data -> c] -> state,
                                            "curve-key-pressed", G_CALLBACK(on_curve_key_pressed), data,
                                            "curve-pointer-motion", G_CALLBACK(on_curve_pointer_motion), & this_proj -> curves[data -> b][data -> c] -> state,
                                            NULL, NULL, NULL);
#endif

  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, this_proj -> curves[data -> b][data -> c] -> plot, FALSE, TRUE, 0);

#ifdef GTK3
  g_signal_connect (G_OBJECT(this_proj -> curves[data -> b][data -> c] -> plot), "draw", G_CALLBACK(show_curve), data);
  g_signal_connect (G_OBJECT(Curve), "key-press-event", G_CALLBACK(on_curve_key_pressed), data);
#else
  gtk_drawing_area_set_draw_func (GTK_DRAWING_AREA(this_proj -> curves[data -> b][data -> c] -> plot), (GtkDrawingAreaDrawFunc)show_curve, data, NULL);
#endif
  g_signal_connect (G_OBJECT(Curve), "realize", G_CALLBACK(on_curve_realize), data);
  add_gtk_close_event (Curve, G_CALLBACK(to_hide_curve), data);
  return Curve;
}
