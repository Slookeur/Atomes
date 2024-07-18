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
* @file read_curve.c
* @short Functions to read curve data in the atomes project file format
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'read_curve.c'
*
* Contains:
*

 - The functions to read curve data in the atomes project file format

*
* List of functions:

  int read_project_curve (FILE * fp, int wid, int pid);

  gboolean read_data_layout (FILE * fp, DataLayout * layout);

*/

#include "global.h"
#include "project.h"

/*!
  \fn gboolean read_data_layout (FILE * fp, DataLayout * layout)

  \brief read data layout from file

  \param fp the file pointer
  \param layout the data layout to store the data
*/
gboolean read_data_layout (FILE * fp, DataLayout * layout)
{
  if (fread (& layout -> datacolor, sizeof(ColRGBA), 1, fp) != 1) return FALSE;
  if (fread (& layout -> thickness, sizeof(double), 1, fp) != 1) return FALSE;
  if (fread (& layout -> dash, sizeof(int), 1, fp) != 1) return FALSE;
  if (fread (& layout -> glyph, sizeof(int), 1, fp) != 1) return FALSE;
  if (fread (& layout -> gsize, sizeof(double), 1, fp) != 1) return FALSE;
  if (fread (& layout -> gfreq, sizeof(int), 1, fp) != 1) return FALSE;
  if (fread (& layout -> hwidth, sizeof(double), 1, fp) != 1) return FALSE;
  if (fread (& layout -> hopac, sizeof(double), 1, fp) != 1) return FALSE;
  if (fread (& layout -> hpos, sizeof(int), 1, fp) != 1) return FALSE;
  if (fread (& layout -> aspect, sizeof(int), 1, fp) != 1) return FALSE;
  return TRUE;
}

/*!
  \fn int read_project_curve (FILE * fp, int wid, int pid)

  \brief read a project curve from file

  \param fp the file pointer
  \param wid the total number of projects in the workspace
  \param pid the active project id
*/
int read_project_curve (FILE * fp, int wid, int pid)
{
  int i, j;
  int pic, rid, cid;
  if (wid > 0)
  {
    if (fread (& pic, sizeof(int), 1, fp) != 1) return ERROR_RW;
  }
  else
  {
    pic = pid;
  }
  project * this_proj = get_project_by_id (pic);
  if (fread (& rid, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fread (& cid, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fread (& this_proj -> curves[rid][cid] -> displayed, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
  if (fread (& this_proj -> curves[rid][cid] -> ndata, sizeof(int), 1, fp) != 1) return ERROR_RW;
  this_proj -> curves[rid][cid] -> data[0] = allocdouble (this_proj -> curves[rid][cid] -> ndata);
  if (fread (this_proj -> curves[rid][cid] -> data[0], sizeof(double), this_proj -> curves[rid][cid] -> ndata, fp) != this_proj -> curves[rid][cid] -> ndata) return ERROR_RW;
  this_proj -> curves[rid][cid] -> data[1] = allocdouble (this_proj -> curves[rid][cid] -> ndata);
  if (fread (this_proj -> curves[rid][cid] -> data[1], sizeof(double), this_proj -> curves[rid][cid] -> ndata, fp) != this_proj -> curves[rid][cid] -> ndata) return ERROR_RW;
  if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (i)
  {
    this_proj -> curves[rid][cid] -> err = allocdouble (this_proj -> curves[rid][cid] -> ndata);
    if (fread (this_proj -> curves[rid][cid] -> err, sizeof(double), this_proj -> curves[rid][cid] -> ndata, fp) != this_proj -> curves[rid][cid] -> ndata) return ERROR_RW;
  }

  if (this_proj -> curves[rid][cid] -> displayed)
  {
    if (fread (this_proj -> curves[rid][cid] -> wsize, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> cmin, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> cmax, sizeof(double), 2, fp) != 2) return ERROR_RW;
    // Title
    if (fread (& this_proj -> curves[rid][cid] -> show_title, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> default_title, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (! this_proj -> curves[rid][cid] -> default_title)
    {
      this_proj -> curves[rid][cid] -> title = read_this_string (fp);
      if (this_proj -> curves[rid][cid] -> title == NULL) return ERROR_RW;
    }
    if (fread (this_proj -> curves[rid][cid] -> title_pos, sizeof(double), 2, fp) != 2) return ERROR_RW;
    this_proj -> curves[rid][cid] -> title_font = read_this_string (fp);
    if (this_proj -> curves[rid][cid] -> title_font == NULL) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] ->  title_color, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    // Axis
    if (fread (this_proj -> curves[rid][cid] -> axmin, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> axmax, sizeof(double), 2, fp) != 2) return ERROR_RW;
    for (j=0; j<2; j++)
    {
      this_proj -> curves[rid][cid] -> axis_title[j] = read_this_string (fp);
      if (this_proj -> curves[rid][cid] -> axis_title[j] == NULL) return ERROR_RW;
      this_proj -> curves[rid][cid] -> axis_title_font[j] = read_this_string (fp);
      if (this_proj -> curves[rid][cid] -> axis_title_font[j] == NULL) return ERROR_RW;
    }
    if (fread (this_proj -> curves[rid][cid] -> axis_title_x, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> axis_title_y, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> scale, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> axis_defaut_title, sizeof(gboolean), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> autoscale, sizeof(gboolean), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> majt, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> mint, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> ticks_io, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> ticks_pos, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> majt_size, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> mint_size, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> labels_pos, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> labels_digit, sizeof(int), 2, fp) != 2) return ERROR_RW;
    for (j=0; j<2; j++)
    {
      this_proj -> curves[rid][cid] -> labels_font[j] = read_this_string (fp);
      if (this_proj -> curves[rid][cid] -> labels_font[j] == NULL) return ERROR_RW;
    }
    if (fread (this_proj -> curves[rid][cid] -> labels_angle, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> labels_shift_x, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> labels_shift_y, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> show_grid, sizeof(gboolean), 2, fp) != 2) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> show_axis, sizeof(gboolean), 2, fp) != 2) return ERROR_RW;

    // Legend
    if (fread (& this_proj -> curves[rid][cid] -> show_legend, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    this_proj -> curves[rid][cid] -> legend_font = read_this_string (fp);
    if (this_proj -> curves[rid][cid] -> legend_font == NULL) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> legend_pos, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> legend_color, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> show_legend_box, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> legend_box_dash, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> legend_box_thickness, sizeof(double), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> legend_box_color, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    // Frame
    if (fread (& this_proj -> curves[rid][cid] -> show_frame, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> frame_type, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> frame_dash, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> frame_thickness, sizeof(double), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> frame_color, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    if (fread (this_proj -> curves[rid][cid] -> frame_pos, sizeof(this_proj -> curves[rid][cid] -> frame_pos), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> backcolor, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    // Data
    this_proj -> curves[rid][cid] -> layout = g_malloc0 (sizeof*this_proj -> curves[rid][cid] -> layout);
    if (! read_data_layout (fp, this_proj -> curves[rid][cid] -> layout)) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> draw_id, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fread (& this_proj -> curves[rid][cid] -> bshift, sizeof(int), 1, fp) != 1) return ERROR_RW;

    this_proj -> curves[rid][cid] -> extrac = g_malloc0 (sizeof*this_proj -> curves[rid][cid] -> extrac);
    if (fread (& this_proj -> curves[rid][cid] -> extrac -> extras, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (this_proj -> curves[rid][cid] -> extrac -> extras > 0)
    {
      this_proj -> curves[rid][cid] -> extrac -> first = g_malloc0 (sizeof*this_proj -> curves[rid][cid] -> extrac -> first);
      this_proj -> curves[rid][cid] -> extrac -> last = g_malloc0 (sizeof*this_proj -> curves[rid][cid] -> extrac -> last);
      CurveExtra * ctmp = this_proj -> curves[rid][cid] -> extrac -> first;
      for (i=0; i<this_proj -> curves[rid][cid] -> extrac -> extras; i++)
      {
        if (fread (& ctmp -> id.a, sizeof(int), 1, fp) != 1) return ERROR_RW;
        if (fread (& ctmp -> id.b, sizeof(int), 1, fp) != 1) return ERROR_RW;
        if (fread (& ctmp -> id.c, sizeof(int), 1, fp) != 1) return ERROR_RW;
        ctmp -> layout = g_malloc0 (sizeof*ctmp -> layout);
        if (! read_data_layout (fp, ctmp -> layout)) return ERROR_RW;
        if (i < this_proj -> curves[rid][cid] -> extrac -> extras - 1)
        {
          ctmp -> next = g_malloc0 (sizeof*ctmp -> next);
          ctmp -> next -> prev = ctmp;
          ctmp = ctmp -> next;
        }
        else if (i == this_proj -> curves[rid][cid] -> extrac -> extras - 1)
        {
          this_proj -> curves[rid][cid] -> extrac -> last = ctmp;
        }
      }
    }
    if (fread (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (i == 1)
    {
      this_proj -> curves[rid][cid] -> cfile = read_this_string (fp);
      if (this_proj -> curves[rid][cid] -> cfile == NULL) return ERROR_RW;
    }
  }
#ifdef DEBUG
  // debugiocurve (this_proj, win, rid, cid, "READ");
#endif
  return OK;
}
