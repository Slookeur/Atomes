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
* @file legend.c
* @short Function to draw the legend
* @author Sébastien Le Roux <sebastien.leroux@ipcms.unistra.fr>
*/

/*
* This file: 'legend.c'
*
* Contains:
*

 - The function to draw the legend

*
* List of functions:

  void show_legend (cairo_t * cr, project * this_proj, int rid, int cid);

*/

#include <stdlib.h>
#include <cairo.h>
#include <pango/pangocairo.h>

#include "global.h"
#include "curve.h"

extern DataLayout * get_extra_layout (int i);

/*!
  \fn void show_legend (cairo_t * cr, project * this_proj, int rid, int cid)

  \brief draw legend

  \param cr the cairo drawing context to use for the draw
  \param this_proj the target project
  \param rid the calculation id
  \param cid the curve id
*/
void show_legend (cairo_t * cr, project * this_proj, int rid, int cid)
{
  double x, y, z;
  double ih, fh;
  double tw, th;
  int w, h;
  int j, k, l, m;
  gchar * str;
  curve_dash * dasht;

  x = this_proj -> curves[rid][cid] -> legend_pos[0] * resol[0];
  y = this_proj -> curves[rid][cid] -> legend_pos[1] * resol[1];

  pango_layout_set_font_description (layout, pango_font_description_from_string (this_proj -> curves[rid][cid] -> legend_font));
  CurveExtra * ctmp;
  ctmp = this_proj -> curves[rid][cid] -> extrac -> first;
  for ( j=this_proj -> curves[rid][cid] -> extrac -> extras ; j >= 0 ; j-- )
  {
    y = y + 4.0*z/3.0;
    if (this_proj -> curves[rid][cid] -> draw_id == j)
    {
      if (this_proj -> curves[rid][cid] -> layout -> dash > 0)
      {
        dasht = selectdash (this_proj -> curves[rid][cid] -> layout -> dash);
        cairo_set_dash(cr, dasht -> a, dasht -> b, 0.0);
        cairo_set_source_rgba (cr, this_proj -> curves[rid][cid] -> layout -> datacolor.red,
                                   this_proj -> curves[rid][cid] -> layout -> datacolor.green,
                                   this_proj -> curves[rid][cid] -> layout -> datacolor.blue,
                                   this_proj -> curves[rid][cid] -> layout -> datacolor.alpha);
        cairo_set_line_width (cr, this_proj -> curves[rid][cid] -> layout -> thickness);
        cairo_move_to (cr, x-5, y);
        cairo_line_to (cr, x+20, y);
        cairo_stroke(cr);
        g_free (dasht);
      }
      z = x-5;
      draw_glyph (cr, this_proj -> curves[rid][cid] -> layout -> glyph, z, y,
                      this_proj -> curves[rid][cid] -> layout -> datacolor,
                      this_proj -> curves[rid][cid] -> layout -> gsize);
      z = x+20;
      draw_glyph (cr, this_proj -> curves[rid][cid] -> layout -> glyph, z, y,
                      this_proj -> curves[rid][cid] -> layout -> datacolor,
                      this_proj -> curves[rid][cid] -> layout -> gsize);
      cairo_set_source_rgba (cr, this_proj -> curves[rid][cid] -> legend_color.red,
                                 this_proj -> curves[rid][cid] -> legend_color.green,
                                 this_proj -> curves[rid][cid] -> legend_color.blue,
                                 this_proj -> curves[rid][cid] -> legend_color.alpha);
      str = g_strdup_printf ("%s - %s", prepare_for_title(this_proj -> name), this_proj -> curves[rid][cid] -> name);
    }
    else
    {
      DataLayout * lay = ctmp -> layout;
      if (lay -> dash > 0)
      {
        dasht = selectdash (lay -> dash);
        cairo_set_dash(cr, dasht -> a, dasht -> b, 0.0);
        cairo_set_source_rgba (cr, lay -> datacolor.red,
                                   lay -> datacolor.green,
                                   lay -> datacolor.blue,
                                   lay -> datacolor.alpha);
        cairo_set_line_width (cr, lay -> thickness);
        cairo_move_to (cr, x-5, y);
        cairo_line_to (cr, x+20, y);
        cairo_stroke(cr);
        g_free (dasht);
      }
      z = x-5;
      draw_glyph (cr, lay -> glyph, z, y, lay -> datacolor, lay -> gsize);
      z = x+20;
      draw_glyph (cr, lay -> glyph, z, y, lay -> datacolor, lay -> gsize);
      cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 1.0);
      k = ctmp -> id.a;
      l = ctmp -> id.b;
      m = ctmp -> id.c;
      str = g_strdup_printf ("%s - %s", prepare_for_title(get_project_by_id(k) -> name),
                                        get_project_by_id(k) -> curves[l][m] -> name);
      if (ctmp -> next !=  NULL) ctmp = ctmp -> next;
    }
    pango_layout_set_text (layout, str, -1);
    pango_layout_get_size (layout, & w, & h);
    z = (double) h / PANGO_SCALE;
    if (j == 0) ih = z;
    fh = z;
    tw = max(tw, (double) w / PANGO_SCALE);
    cairo_move_to (cr, x + 35.0, y - z/2);
    pango_cairo_update_layout (cr, layout);
    pango_cairo_show_layout (cr, layout);
    cairo_stroke (cr);
    g_free (str);
  }
  if (this_proj -> curves[rid][cid] -> show_legend_box)
  {
    dasht = selectdash (this_proj -> curves[rid][cid] -> legend_box_dash);
    cairo_set_dash(cr, dasht -> a, dasht -> b, 0.0);
    cairo_set_source_rgba (cr, this_proj -> curves[rid][cid] -> legend_box_color.red,
                               this_proj -> curves[rid][cid] -> legend_box_color.green,
                               this_proj -> curves[rid][cid] -> legend_box_color.blue,
                               this_proj -> curves[rid][cid] -> legend_box_color.alpha);
    cairo_set_line_width (cr, this_proj -> curves[rid][cid] -> legend_box_thickness);
    x = this_proj -> curves[rid][cid] -> legend_pos[0] * resol[0] - 25;
    th = ih + fh + (y - this_proj -> curves[rid][cid] -> legend_pos[1] * resol[1]);
    y = this_proj -> curves[rid][cid] -> legend_pos[1] * resol[1] - ih;
    tw = tw + 80;
    cairo_rectangle (cr, x, y, tw, th);
    cairo_stroke(cr);
    g_free (dasht);
  }
}
