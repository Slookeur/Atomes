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
* This file: 'save_curve.c'
*
*  Contains:
*

 - The subroutines to write curves information in the atomes project file format

*
*  List of subroutines:

  int save_project_curve (FILE * fp, int wid, struct project * this_proj, int rid, int cid);

  gboolean write_data_layout (FILE * fp, DataLayout * layout);

*/

#include "global.h"
#include "project.h"

/*
*  gboolean write_data_layout (FILE * fp, DataLayout * layout)
*
*  Usage: save curve data layout to file
*
*  FILE * fp           : the file pointer
*  DataLayout * layout : the data layout to save
*/
gboolean write_data_layout (FILE * fp, DataLayout * layout)
{
  if (fwrite (& layout -> datacolor, sizeof(ColRGBA), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> thickness, sizeof(double), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> dash, sizeof(int), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> glyph, sizeof(int), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> gsize, sizeof(double), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> gfreq, sizeof(int), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> hwidth, sizeof(double), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> hopac, sizeof(double), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> hpos, sizeof(int), 1, fp) != 1) return FALSE;
  if (fwrite (& layout -> aspect, sizeof(int), 1, fp) != 1) return FALSE;
  return TRUE;
}

/*
*  int save_project_curve (FILE * fp, int wid, struct project * this_proj, int rid, int cid)
*
*  Usage: save project curve to file
*
*  FILE * fp                  : the file pointer
*  int wid                    : the total number of project file in the workspace
*  struct project * this_proj : the target project
*  int rid                    : the calculation to save
*  int cid                    : the curve id to save
*/
int save_project_curve (FILE * fp, int wid, struct project * this_proj, int rid, int cid)
{
  int i, j;
  if (wid > 0)
  {
    if (fwrite (& this_proj -> id, sizeof(int), 1, fp) != 1) return ERROR_RW;
  }
  if (fwrite (& rid, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& cid, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& this_proj -> curves[rid][cid] -> displayed, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
  if (fwrite (& this_proj -> curves[rid][cid] -> ndata, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (fwrite (this_proj -> curves[rid][cid] -> data[0],
              sizeof(double), this_proj -> curves[rid][cid] -> ndata, fp) != this_proj -> curves[rid][cid] -> ndata) return ERROR_RW;
  if (fwrite (this_proj -> curves[rid][cid] -> data[1],
              sizeof(double), this_proj -> curves[rid][cid] -> ndata, fp) != this_proj -> curves[rid][cid] -> ndata) return ERROR_RW;
  i = 0;
  if (this_proj -> curves[rid][cid] -> err != NULL) i = 1;
  if (fwrite (& i, sizeof(int), 1, fp) != 1) return ERROR_RW;
  if (this_proj -> curves[rid][cid] -> err != NULL)
  {
    if (fwrite (this_proj -> curves[rid][cid] -> err,
                sizeof(double), this_proj -> curves[rid][cid] -> ndata, fp) != this_proj -> curves[rid][cid] -> ndata) return ERROR_RW;
  }

  if (this_proj -> curves[rid][cid] -> displayed)
  {
    if (fwrite (this_proj -> curves[rid][cid] -> wsize, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> cmin, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> cmax, sizeof(double), 2, fp) != 2) return ERROR_RW;
    // Title
    if (fwrite (& this_proj -> curves[rid][cid] -> show_title, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> default_title, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (! this_proj -> curves[rid][cid] -> default_title)
    {
      if (save_this_string (fp, this_proj -> curves[rid][cid] -> title) != OK) return ERROR_RW;
    }
    if (fwrite (this_proj -> curves[rid][cid] -> title_pos, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (save_this_string (fp, this_proj -> curves[rid][cid] -> title_font) != OK) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] ->  title_color, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    // Axis
    if (fwrite (this_proj -> curves[rid][cid] -> axmin, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> axmax, sizeof(double), 2, fp) != 2) return ERROR_RW;
    for (j=0; j<2; j++)
    {
      if (save_this_string (fp, this_proj -> curves[rid][cid] -> axis_title[j]) != OK) return ERROR_RW;
      if (save_this_string (fp, this_proj -> curves[rid][cid] -> axis_title_font[j]) != OK) return ERROR_RW;
    }
    if (fwrite (this_proj -> curves[rid][cid] -> axis_title_x, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> axis_title_y, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> scale, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> axis_defaut_title, sizeof(gboolean), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> autoscale, sizeof(gboolean), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> majt, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> mint, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> ticks_io, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> ticks_pos, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> majt_size, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> mint_size, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> labels_pos, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> labels_digit, sizeof(int), 2, fp) != 2) return ERROR_RW;
    for (j=0; j<2; j++)
    {
      if (save_this_string (fp, this_proj -> curves[rid][cid] -> labels_font[j]) != OK) return ERROR_RW;
    }
    if (fwrite (this_proj -> curves[rid][cid] -> labels_angle, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> labels_shift_x, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> labels_shift_y, sizeof(int), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> show_grid, sizeof(gboolean), 2, fp) != 2) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> show_axis, sizeof(gboolean), 2, fp) != 2) return ERROR_RW;
    // Legend
    if (fwrite (& this_proj -> curves[rid][cid] -> show_legend, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (save_this_string (fp, this_proj -> curves[rid][cid] -> legend_font) != OK) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> legend_pos, sizeof(double), 2, fp) != 2) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> legend_color, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> show_legend_box, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> legend_box_dash, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> legend_box_thickness, sizeof(double), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> legend_box_color, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    // Frame
    if (fwrite (& this_proj -> curves[rid][cid] -> show_frame, sizeof(gboolean), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> frame_type, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> frame_dash, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> frame_thickness, sizeof(double), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> frame_color, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    if (fwrite (this_proj -> curves[rid][cid] -> frame_pos, sizeof(this_proj -> curves[rid][cid] -> frame_pos), 1, fp) != 1) return ERROR_RW;
    // Data
    if (fwrite (& this_proj -> curves[rid][cid] -> backcolor, sizeof(ColRGBA), 1, fp) != 1) return ERROR_RW;
    if (! write_data_layout (fp, this_proj -> curves[rid][cid] -> layout)) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> draw_id, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> bshift, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (fwrite (& this_proj -> curves[rid][cid] -> extrac -> extras, sizeof(int), 1, fp) != 1) return ERROR_RW;
    if (this_proj -> curves[rid][cid] -> extrac -> extras > 0)
    {
      struct cextra * ctmp = this_proj -> curves[rid][cid] -> extrac -> first;
      for (i=0; i<this_proj -> curves[rid][cid] -> extrac -> extras; i++)
      {
        if (fwrite (& ctmp -> id.a, sizeof(int), 1, fp) != 1) return ERROR_RW;
        if (fwrite (& ctmp -> id.b, sizeof(int), 1, fp) != 1) return ERROR_RW;
        if (fwrite (& ctmp -> id.c, sizeof(int), 1, fp) != 1) return ERROR_RW;
        if (! write_data_layout (fp, ctmp -> layout)) return ERROR_RW;
        if (ctmp -> next != NULL) ctmp = ctmp -> next;
      }
    }
    if (save_this_string (fp, this_proj -> curves[rid][cid] -> cfile) != OK) return ERROR_RW;
  }
#ifdef DEBUG
//  debugiocurve (this_proj, win, rid, cid, "WRITE");
#endif
  return OK;
}
