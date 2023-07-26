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
* This file: 'glyph.c'
*
*  Contains:
*

 - Curve glyph drawing subroutines

*
*  List of subroutines:

  void draw_poly (cairo_t * to, double x, double y, double size, double step, double initp);
  void draw_glyph (cairo_t * in, int theglyph, double x, double y, ColRGBA gcolor, double size);

*/

#include <cairo.h>
#include <math.h>

#include "global.h"
#include "curve.h"

/*
*  void draw_poly (cairo_t * to, double x, double y, double size, double step, double initp)
*
*  Usage:
*
*  cairo_t * to :
*  double x     :
*  double y     :
*  double size  :
*  double step  :
*  double initp :
*/
void draw_poly (cairo_t * to, double x, double y, double size, double step, double initp)
{
  double px, py;
  double alpha;
  double astep;
  double astart;

  astart = 2*pi / initp;
  px = x + size * cos (astart);
  py = y + size * sin (astart);
  astep = 2 * pi / step;
  cairo_move_to (to, px, py);
  for (alpha =0.0 ; alpha <= 2*pi ; alpha += astep)
  {
    px = x + size * cos (astart + alpha);
    py = y + size *sin (astart + alpha);
    cairo_line_to (to, px, py);
  }
}

/*
*  void draw_glyph (cairo_t * in, int theglyph, double x, double y, ColRGBA gcolor, double size)
*
*  Usage:
*
*  cairo_t * in   :
*  int theglyph   :
*  double x       :
*  double y       :
*  ColRGBA gcolor :
*  double size    :
*/
void draw_glyph (cairo_t * in, int theglyph, double x, double y, ColRGBA gcolor, double size)
{
  double step;
  double start;
  double width;
  double cdash[3];
  double offset;
  int dcount;
  int idglyph;
  double starttab[] = {1.0, 4.0, 2.0, 1.33333};

  dcount = cairo_get_dash_count (in);
  cairo_get_dash (in, cdash, & offset);
  cairo_set_dash (in, dashed1, len1, 0.0);
  width = cairo_get_line_width (in);
  cairo_set_line_width (in, 1.0);
  cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 1.0);
  if (theglyph == 1)
  {
    cairo_move_to (in, x + size/2.0, y - size/2.0);
    cairo_line_to (in, x - size/2.0, y + size/2.0);
    cairo_move_to (in, x + size/2.0, y + size/2.0);
    cairo_line_to (in, x - size/2.0, y - size/2.0);
    cairo_stroke (in);
  }
  if (theglyph > 1 && theglyph < 7)
  {
    cairo_move_to (in, x, y);
    cairo_rectangle (in, x - size/2.0, y - size/2.0, size, size);
    if (theglyph == 2)
    {
      cairo_fill(in);
    }
    else if (theglyph == 3)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 0.25);
      cairo_move_to (in, x, y);
      cairo_rectangle (in, x - size/2.0, y - size/2.0, size, size);
      cairo_fill (in);
    }
    else if (theglyph == 4)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, 1.0, 1.0, 1.0, 1.0);
      cairo_move_to (in, x, y);
      cairo_rectangle (in, x - size/2.0, y - size/2.0, size, size);
      cairo_fill (in);
      cairo_stroke (in);
      cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 0.25);
      cairo_move_to (in, x, y);
      cairo_rectangle (in, x - size/2.0, y - size/2.0, size, size);
      cairo_fill (in);
    }
    else if (theglyph == 5)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, 1.0, 1.0, 1.0, 1.0);
      cairo_move_to (in, x, y);
      cairo_rectangle (in, x - size/2.0, y - size/2.0, size, size);
      cairo_fill (in);
    }
    cairo_stroke (in);
  }
  else if (theglyph >= 7 && theglyph < 12)
  {
    step = 4.0;
    start = 1.0;
    draw_poly (in, x, y, size, step, start);
    if (theglyph == 7)
    {
      cairo_fill(in);
    }
    else if (theglyph == 8)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 0.25);
      draw_poly (in, x, y, size, step, start);
      cairo_fill (in);
    }
    else if (theglyph == 9)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, 1.0, 1.0, 1.0, 1.0);
      draw_poly (in, x, y, size, step, start);
      cairo_fill (in);
      cairo_stroke (in);
      cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 0.25);
      draw_poly (in, x, y, size, step, start);
      cairo_fill (in);
    }
    else if (theglyph == 10)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, 1.0, 1.0, 1.0, 1.0);
      draw_poly (in, x, y, size, step, start);
      cairo_fill (in);
    }
    cairo_stroke (in);
  }
  else if (theglyph >= 12 && theglyph < 17)
  {
    cairo_arc (in, x, y, size, 0.0, 2*pi);
    if (theglyph == 12)
    {
      cairo_fill(in);
    }
    else if (theglyph == 13)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 0.25);
      cairo_arc (in, x, y, size, 0.0, 2*pi);
      cairo_fill (in);
    }
    else if (theglyph == 14)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, 1.0, 1.0, 1.0, 1.0);
      cairo_arc (in, x, y, size, 0.0, 2*pi);
      cairo_fill (in);
      cairo_stroke (in);
      cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 0.25);
      cairo_arc (in, x, y, size, 0.0, 2*pi);
      cairo_fill (in);
    }
    else if (theglyph == 15)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, 1.0, 1.0, 1.0, 1.0);
      cairo_arc (in, x, y, size, 0.0, 2*pi);
      cairo_fill (in);
    }
    cairo_stroke (in);
  }
  else if (theglyph > 16)
  {
    step = 3.0;
    idglyph = (theglyph - 17) / 5;
    start = starttab[idglyph];
    draw_poly (in, x, y, size, step, start);
    if (theglyph == 18 + idglyph*5)
    {
      cairo_fill(in);
    }
    else if (theglyph == 19 + idglyph*5)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 0.25);
      draw_poly (in, x, y, size, step, start);
      cairo_fill (in);
    }
    else if (theglyph == 20 + idglyph*5)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, 1.0, 1.0, 1.0, 1.0);
      draw_poly (in, x, y, size, step, start);
      cairo_fill (in);
      cairo_stroke (in);
      cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 0.25);
      draw_poly (in, x, y, size, step, start);
      cairo_fill (in);
    }
    else if (theglyph == 21 + idglyph*5)
    {
      cairo_stroke (in);
      cairo_set_source_rgba (in, 1.0, 1.0, 1.0, 1.0);
      draw_poly (in, x, y, size, step, start);
      cairo_fill (in);
    }
    cairo_stroke (in);
  }
  cairo_set_dash (in, cdash, dcount, offset);
  cairo_set_source_rgba (in, gcolor.red, gcolor.green, gcolor.blue, 1.0);
  cairo_set_line_width (in, width);
}

