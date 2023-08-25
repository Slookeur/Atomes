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
* This file: 'w_sequencer.c'
*
*  Contains:
*

 - The subroutine to create the MD sequencer window

*
*  List of subroutines:

  static gboolean animate (gpointer data);
  static gboolean seq_wait_for_stop (gpointer data);

  void set_player_title (glwin * view);
  void update_selection (glwin * view, int o_step);
  void update_step_button (glwin * view);
  void sequence (glwin * view, int o_step, int n_step);

  G_MODULE_EXPORT void seq_go_previous (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_go_next (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_go_first (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_go_last (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_go_to (GtkEntry * res, gpointer data);
  G_MODULE_EXPORT void seq_jump (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_play (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_stop (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_loop (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_faster (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void seq_slower (GtkButton * but, gpointer data);
  G_MODULE_EXPORT void window_sequencer (GtkWidget * widg, gpointer data);

*/

#include "global.h"
#include "interface.h"
#include "callbacks.h"
#include "glview.h"
#include "glwindow.h"

/*
*  void set_player_title (glwin * view)
*
*  Usage: set sequencer window title
*
*  glwin * view : the target glwin
*/
void set_player_title (glwin * view)
{
  int step = view -> anim -> last -> img -> step + 1;
  gchar * str = g_strdup_printf ("%s - player - step %d", prepare_for_title (get_project_by_id(view -> proj) -> name), step);
  gtk_window_set_title (GTK_WINDOW(view -> player -> win), str);
  g_free (str);
}

/*
*  void update_selection (glwin * view, int o_step)
*
*  Usage: match and udpate selected atom()s from o_step to the active step
*
*  glwin * view : the target glwin
*  int o_step   : the step to match selection with
*/
void update_selection (glwin * view, int o_step)
{
  int i, j, k;
  i = view -> anim -> last -> img -> step;
  struct project * this_proj = get_project_by_id(view -> proj);
  for (k=0; k<2; k++)
  {
    view -> anim -> last -> img -> selected[k] -> selected = 0;
    view -> anim -> last -> img -> selected[k] -> first = NULL;
    view -> anim -> last -> img -> selected[k] -> last = NULL;
    for (j=0; j<this_proj -> natomes; j++)
    {
      if (this_proj -> atoms[o_step][j].pick[k])
      {
        this_proj -> atoms[i][j].pick[k] = FALSE;
        process_selected_atom (this_proj, view, j, 0, 0, k);
      }
      if (this_proj -> atoms[o_step][j].label[k])
      {
        this_proj -> atoms[i][j].label[k] = TRUE;
      }
    }
    update_all_selections (view, k);
  }
}

/*
*  void update_step_button (glwin * view)
*
*  Usage: correct widget buttons sensitivity based on MD step
*
*  glwin * view : the target glwin
*/
void update_step_button (glwin * view)
{
  if (view -> anim -> last -> img -> step == get_project_by_id(view -> proj) -> steps - 1)
  {
    widget_set_sensitive (view -> player -> first, 1);
    widget_set_sensitive (view -> player -> prev, 1);
    widget_set_sensitive (view -> player -> last, 0);
    widget_set_sensitive (view -> player -> next, 0);
  }
  else if (view -> anim -> last -> img -> step == 0)
  {
    widget_set_sensitive (view -> player -> first, 0);
    widget_set_sensitive (view -> player -> prev, 0);
    widget_set_sensitive (view -> player -> last, 1);
    widget_set_sensitive (view -> player -> next, 1);
  }
  else
  {
    widget_set_sensitive (view -> player -> first, 1);
    widget_set_sensitive (view -> player -> prev, 1);
    widget_set_sensitive (view -> player -> last, 1);
    widget_set_sensitive (view -> player -> next, 1);
  }
}

/*
*  void sequence (glwin * view, int o_step, int n_step)
*
*  Usage: sequence to next step
*
*  glwin * view : the target glwin
*  int o_step   : actual step
*  int n_step   : next step
*/
void sequence (glwin * view, int o_step, int n_step)
{
  int i;
  for (i=0; i<2; i++) save_all_selections (view, i);
  view -> anim -> last -> img -> step = n_step;
  update_selection (view, o_step);
  for (i=0; i<NGLOBAL_SHADERS; i++)
  {
    if (in_md_shaders (get_project_by_id(view -> proj), i)) view -> n_shaders[i][n_step] = -1;
  }
  recreate_all_shaders (view);
  set_player_title (view);
  update_step_button (view);
  update (view);
}

/*
*  G_MODULE_EXPORT void seq_go_previous (GtkButton * but, gpointer data)
*
*  Usage: go to previous frame
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_go_previous (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  if (view -> anim -> last -> img -> step > 0)
  {
    sequence (view, view -> anim -> last -> img -> step, view -> anim -> last -> img -> step - 1);
  }
}

/*
*  G_MODULE_EXPORT void seq_go_next (GtkButton * but, gpointer data)
*
*  Usage: go to next frame
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_go_next (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  struct project * this_proj = get_project_by_id(view -> proj);
  if (view -> anim -> last -> img -> step < this_proj -> steps-1)
  {
    sequence (view, view -> anim -> last -> img -> step, view -> anim -> last -> img -> step + 1);
  }
}

/*
*  G_MODULE_EXPORT void seq_go_first (GtkButton * but, gpointer data)
*
*  Usage: go to first frame
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_go_first (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  sequence (view, view -> anim -> last -> img -> step, 0);
}

/*
*  G_MODULE_EXPORT void seq_go_last (GtkButton * but, gpointer data)
*
*  Usage: got to last frame
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_go_last (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  sequence (view, view -> anim -> last -> img -> step, get_project_by_id(view -> proj) -> steps - 1);
}

/*
*  G_MODULE_EXPORT void seq_go_to (GtkEntry * res, gpointer data)
*
*  Usage: jump to frame
*
*  GtkEntry * res : the GtkEntry sending the signal
*  gpointer data  : the associated data pointer
*/
G_MODULE_EXPORT void seq_go_to (GtkEntry * res, gpointer data)
{
  glwin * view = (glwin *) data;
  const gchar * m = entry_get_text (res);
  int s = (int)atof(m);
  struct project * this_proj = get_project_by_id(view -> proj);
  if (s > 0 && s <= this_proj -> steps)
  {
    sequence (view, view -> anim -> last -> img -> step, s-1);
  }
  update_entry_int (res, this_proj -> modelgl -> anim -> last -> img -> step+1);
}

/*
*  G_MODULE_EXPORT void seq_jump (GtkButton * but, gpointer data)
*
*  Usage: jump to frame dialog
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_jump (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  GtkWidget * win = dialogmodal ("Enter a step number", GTK_WINDOW(view -> player -> win));
  GtkWidget * vbox = dialog_get_content_area (win);
  GtkWidget * hbox = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hbox, TRUE, TRUE, 0);
  gchar * str = g_strdup_printf ("Step number [%d-%d]: ", 1, get_project_by_id(view -> proj) -> steps);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, gtk_label_new (str), TRUE, TRUE, 0);
  g_free (str);
  GtkWidget * step = create_entry (G_CALLBACK(seq_go_to), 100, 15, TRUE, (gpointer)view);
  update_entry_int (GTK_ENTRY(step), view -> anim -> last -> img -> step+1);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hbox, step, FALSE, FALSE, 0);
  run_this_gtk_dialog (win, G_CALLBACK(run_destroy_dialog), NULL);
}

/*
*  static gboolean animate (gpointer data)
*
*  Usage: animate
*
*  gpointer data : the associated data pointer
*/
static gboolean animate (gpointer data)
{
  glwin * view = (glwin *) data;
  if (view -> play)
  {
    if (view -> anim -> last -> img -> step < get_project_by_id(view -> proj) -> steps-1)
    {
      sequence (view, view -> anim -> last -> img -> step, view -> anim -> last -> img -> step+1);
    }
    else if (view -> loop)
    {
      sequence (view, view -> anim -> last -> img -> step, 0);
    }
    else
    {
      view -> play = FALSE;
      view -> stop = TRUE;
    }
  }
  else
  {
    view -> play = FALSE;
    view -> stop = TRUE;
  }
  return view -> play;
}

/*
*  G_MODULE_EXPORT void seq_play (GtkButton * but, gpointer data)
*
*  Usage: play
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_play (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  if (! view -> play)
  {
    view -> play = TRUE;
    view -> stop = FALSE;
    g_timeout_add (view -> speed, (GSourceFunc) animate, view);
  }
}

/*
*  G_MODULE_EXPORT void seq_stop (GtkButton * but, gpointer data)
*
*  Usage: stop
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_stop (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  view -> play = FALSE;
}

/*
*  G_MODULE_EXPORT void seq_loop (GtkButton * but, gpointer data)
*
*  Usage: loop the animation
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_loop (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  if (view -> loop)
  {
    view -> loop = FALSE;
    button_set_image (but, NULL, IMG_STOCK, MEDIA_LOOP);
  }
  else
  {
    view -> loop = TRUE;
    button_set_image (but, NULL, IMG_STOCK, MEDIA_ULOOP);
  }
  show_the_widgets (GTK_WIDGET(but));
}

/*
*  static gboolean seq_wait_for_stop (gpointer data)
*
*  Usage: pause / restart if on pause
*
*  gpointer data : the associated data pointer
*/
static gboolean seq_wait_for_stop (gpointer data)
{
  glwin * view = (glwin *) data;

  if (view -> stop)
  {
    seq_play (NULL, data);
    return FALSE;
  }
  else
  {
    return TRUE;
  }
}

/*
*  G_MODULE_EXPORT void seq_faster (GtkButton * but, gpointer data)
*
*  Usage: go faster
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_faster (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  int oldspeed;
  if (view -> play && view -> speed > 4)
  {
    oldspeed = view -> speed;
    if (view -> speed == 5)
    {
      view -> speed -= 1;
    }
    else
    {
      view -> speed -= 5;
    }
    view -> play = FALSE;
    g_timeout_add (oldspeed, (GSourceFunc) seq_wait_for_stop, view);
  }
}

/*
*  G_MODULE_EXPORT void seq_slower (GtkButton * but, gpointer data)
*
*  Usage: go slower
*
*  GtkButton * but : the GtkButton sending the signal
*  gpointer data   : the associated data pointer
*/
G_MODULE_EXPORT void seq_slower (GtkButton * but, gpointer data)
{
  glwin * view = (glwin *) data;
  int oldspeed;
  if (view -> play && view -> speed < 10000)
  {
    oldspeed = view -> speed;
    if (view -> speed == 1)
    {
      view -> speed = 5;
    }
    else
    {
      view -> speed += 5;
    }
    view -> play = FALSE;
    g_timeout_add (oldspeed, (GSourceFunc) seq_wait_for_stop, view);
  }
}

/*
*  G_MODULE_EXPORT void window_sequencer (GtkWidget * widg, gpointer data)
*
*  Usage: create the sequencer window
*
*  GtkWidget * widg : the GtkWidget sending the signal
*  gpointer data    : the associated data pointer
*/
G_MODULE_EXPORT void window_sequencer (GtkWidget * widg, gpointer data)
{
  //int p;
  glwin * view = (glwin *) data;
  if (view -> player == NULL)
  {
    //p = view -> proj;
    view -> player = g_malloc0 (sizeof*view -> player);
    view -> player -> win = create_win (" ", view -> win, FALSE, FALSE);
    set_player_title (view);
    GtkWidget * vbox = create_vbox (BSEP);
    add_container_child (CONTAINER_WIN, view -> player -> win, vbox);
    // First line
    GtkWidget * hboxa = create_hbox (0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hboxa, TRUE, TRUE, 0);
    // First
    view -> player -> first = create_button ("First", IMG_STOCK, MEDIA_FIRST, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_go_first), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, view -> player -> first, TRUE, TRUE, 0);
    // Previous
    view -> player -> prev = create_button ("Previous", IMG_STOCK, MEDIA_PREV, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_go_previous), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, view -> player -> prev, TRUE, TRUE, 0);
    // Next
    view -> player -> next = create_button ("Next", IMG_STOCK, MEDIA_NEXT, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_go_next), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, view -> player -> next, TRUE, TRUE, 0);
    // Last
    view -> player -> last = create_button ("Last", IMG_STOCK, MEDIA_LAST, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_go_last), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, view -> player -> last, TRUE, TRUE, 0);
    // JumpTo
    view -> player -> jump = create_button ("Go to", IMG_STOCK, MEDIA_GOTO, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_jump), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, view -> player -> jump, TRUE, TRUE, 0);

    // Second line
    GtkWidget * hboxb = create_hbox (0);
    add_box_child_start (GTK_ORIENTATION_VERTICAL, vbox, hboxb, TRUE, TRUE, 0);
    // Play
    view -> player -> play = create_button ("Play", IMG_STOCK, MEDIA_PLAY, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_play), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxb, view -> player -> play, TRUE, TRUE, 0);
    // Stop
    view -> player -> stop = create_button ("Stop", IMG_STOCK, MEDIA_STOP, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_stop), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxb, view -> player -> stop, TRUE, TRUE, 0);
    // Loop
    view -> player -> loop = create_button ("Loop", IMG_STOCK, MEDIA_LOOP, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_loop), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxb, view -> player -> loop, TRUE, TRUE, 0);
    // Faster
    view -> player -> fast = create_button ("Faster", IMG_STOCK, MEDIA_FAST, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_faster), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxb, view -> player -> fast, TRUE, TRUE, 0);
    // Slower
    view -> player -> slow =  create_button ("Slower", IMG_STOCK, MEDIA_SLOW, -1, -1, GTK_RELIEF_NONE, G_CALLBACK(seq_slower), view);
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxb, view -> player -> slow, TRUE, TRUE, 0);
    add_gtk_close_event (view -> player -> win, G_CALLBACK(hide_this_window), NULL);
    show_the_widgets (view -> player -> win);
    update_step_button (view);
  }
  else
  {
    gtk_widget_show (view -> player -> win);
  }
}
