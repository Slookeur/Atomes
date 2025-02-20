/* This file is part of the 'atomes' software

'atomes' is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

'atomes' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with 'atomes'.
If not, see <https://www.gnu.org/licenses/>

Copyright (C) 2022-2025 by CNRS and University of Strasbourg */

/*!
* @file image.c
* @short Functions to render an image from the OpenGL window
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'image.c'
*
* Contains:
*

 - The functions to render an image from the OpenGL window

*
* List of functions:

  void render_image (glwin * view, video_options * iopts);

  G_MODULE_EXPORT void run_render_image (GtkNativeDialog * info, gint response_id, gpointer data);
  G_MODULE_EXPORT void run_render_image (GtkDialog * info, gint response_id, gpointer data);

*/

#include "global.h"
#include "interface.h"
#include "project.h"
#include "glwindow.h"
#include "glview.h"
#include "movie.h"

#include <libavformat/avformat.h>

#ifndef GTKGLAREA
extern GdkWindow * xwin;
extern GLXContext glcontext;
extern Pixmap pixmap;
extern GLXPixmap glpixmap;
#endif
extern GdkPixbuf * pixbuf;

char * image_name[IMAGE_FORMATS] = {"PNG",
                                    "JPG/JPEG",
                                    "TIFF",
                                    "Bitmap"};

char * image_list[IMAGE_FORMATS] = {"png",
                                    "jpeg",
                                    "tiff",
                                    "bmp"};

extern void fill_image (VideoStream * vs, int width, int height, glwin * view);
extern void init_frame_buffer (int x, int y);
extern void close_frame_buffer ();

#ifdef GTK4
/*!
  \fn G_MODULE_EXPORT void run_render_image (GtkNativeDialog * info, gint response_id, gpointer data)

  \brief render an image from the OpenGL window - running the dialog

  \param info the GtkNativeDialog sending the signal
  \param response_id the response id
  \param data the associated data pointer
*/
G_MODULE_EXPORT void run_render_image (GtkNativeDialog * info, gint response_id, gpointer data)
{
  GtkFileChooser * chooser = GTK_FILE_CHOOSER((GtkFileChooserNative *)info);
#else
/*!
  \fn G_MODULE_EXPORT void run_render_image (GtkDialog * info, gint response_id, gpointer data)

  \brief render an image from the OpenGL window - running the dialog

  \param info the GtkDialog sending the signal
  \param response_id the response id
  \param data the associated data pointer
*/
G_MODULE_EXPORT void run_render_image (GtkDialog * info, gint response_id, gpointer data)
{
  GtkFileChooser * chooser = GTK_FILE_CHOOSER((GtkWidget *)info);
#endif
  if (response_id == GTK_RESPONSE_ACCEPT)
  {
    video_options * iopts = (video_options *)data;
    gchar * videofile = file_chooser_get_file_name (chooser);
#ifdef GTK4
    destroy_this_native_dialog (info);
#else
    destroy_this_dialog (info);
#endif
    init_frame_buffer (iopts -> video_res[0], iopts -> video_res[1]);
    project * this_proj = get_project_by_id (iopts -> proj);
    glwin * view = this_proj -> modelgl;
    init_opengl ();
    int i, x, y, q;
    for (i=0; i<NGLOBAL_SHADERS; i++)
    {
      if (in_md_shaders (this_proj, i)) view -> n_shaders[i][step] = -1;
    }
    recreate_all_shaders (view);
    in_movie_encoding = TRUE;
    if (iopts -> oglquality != 0)
    {
      q = view -> anim -> last -> img -> quality;
      view -> anim -> last -> img -> quality = iopts -> oglquality;
    }
    for (i=0; i<2; i++) tmp_pixels[i] = view -> pixels[i];
    x = view -> pixels[0];
    y = view -> pixels[1] - 100;
    view -> pixels[0] = iopts -> video_res[0];
    view -> pixels[1] = iopts -> video_res[1];
    fill_image (NULL, iopts -> video_res[0], iopts -> video_res[1], view);
    GError * error = NULL;
    gboolean res = gdk_pixbuf_savev (pixbuf, videofile, image_list[iopts -> codec], NULL, NULL, & error);
    if (! res)
    {
      show_warning ("An error occurred when exporting an image\nyou might want to try again\nsorry for the trouble", view -> win);
    }
    close_frame_buffer ();
    in_movie_encoding = FALSE;
    if (iopts -> oglquality != 0) view -> anim -> last -> img -> quality = q;
    for (i=0; i<NGLOBAL_SHADERS; i++)
    {
      if (in_md_shaders (this_proj, i)) view -> n_shaders[i][step] = -1;
    }
    recreate_all_shaders (view);
    reshape (view, x, y, TRUE);
    update (view);
  }
  else
  {
#ifdef GTK4
    destroy_this_native_dialog (info);
#else
    destroy_this_dialog (info);
#endif
  }
}

/*!
  \fn void render_image (glwin * view, video_options * iopts)

  \brief render an image from the OpenGL window - prepare the dialog

  \param view the target glwin
  \param iopts the rendering options
*/
void render_image (glwin * view, video_options * iopts)
{
  GtkFileFilter * filter;
  gchar * str;
#ifdef GTK4
  GtkFileChooserNative * info;
#else
  GtkWidget * info;
#endif
  info = create_file_chooser ("Render Image",
                              GTK_WINDOW(view -> win),
                              GTK_FILE_CHOOSER_ACTION_SAVE,
                              "Save");
  GtkFileChooser * chooser = GTK_FILE_CHOOSER(info);
#ifdef GTK3
  gtk_file_chooser_set_do_overwrite_confirmation (chooser, TRUE);
#endif
  file_chooser_set_current_folder (chooser);
  str = g_strdup_printf ("%s.%s", prepare_for_title(get_project_by_id(view -> proj) -> name), image_list[iopts -> codec]);
  gtk_file_chooser_set_current_name (chooser, str);
  g_free (str);
  filter = gtk_file_filter_new ();
  str = g_strdup_printf ("%s file (*.%s)", image_name[iopts -> codec], image_list[iopts -> codec]);
  gtk_file_filter_set_name (GTK_FILE_FILTER(filter), str);
  g_free (str);
  str = g_strdup_printf ("*.%s", image_list[iopts -> codec]);
  gtk_file_filter_add_pattern (GTK_FILE_FILTER(filter), str);
  g_free (str);
  gtk_file_chooser_add_filter (chooser, filter);
#ifdef GTK4
  run_this_gtk_native_dialog ((GtkNativeDialog *)info, G_CALLBACK(run_render_image), iopts);
#else
  run_this_gtk_dialog (info, G_CALLBACK(run_render_image), iopts);
#endif
}
