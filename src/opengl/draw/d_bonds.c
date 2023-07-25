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
* This file: 'd_bonds.c'
*
*  Contains: 
*
*
*
*
*  List of subroutines: 

  int cylinder_vertices (int qual);
  int cylinder_indices (int qual);
  int cap_vertices (int qual);
  int cap_indices (int qual);
  int find_bond_vertices (gboolean to_pick, int sty, int sa, int sb, int bi, int cap);
  int create_bond_lists (gboolean to_pick);

  float get_bond_radius (int sty, int ac, int at, int bt, int sel);

  void setup_line_vertice (float * vertices, vec3_t pos, ColRGBA col, float alpha);
  void setup_cylinder_vertice (float * vertices, vec3_t pos_a, vec3_t pos_b, ColRGBA col, float rad, float alpha, float delta);
  void setup_cap_vertice (float * vertices, vec3_t pos_a, vec3_t pos_b, ColRGBA col, float rad, float alpha);
  void setup_this_bond (int sty, gboolean to_pick, gboolean picked, int cap, int bi, int pi, struct atom * at, struct atom * bt, float al, float * vertices);
  void prepare_bond (int sty, gboolean to_pick, gboolean picked, int cap, int bi, int pi, int bid, struct atom * at, struct atom * bt, float * vertices);
  void setup_all_cylinder_vertices (int style, gboolean to_pick, int cap, int bi, float * vertices);
  void setup_line_vertices (int style, int cap, int bi, int sa, int sb, float * vertices);

  object_3d * draw_cylinder (int quality, float ra, float rb);
  object_3d * draw_cylinder_cap (int quality, float rad, gboolean picked);

*/

#include "global.h"
#include "glview.h"
#include "dlp_field.h"

extern ColRGBA get_atom_color (int i, int j, double al, int picked, gboolean to_picked);
extern vec3_t model_position;
extern int nbs;

struct bond_rotation {
  double x, y, z;
  double angle;
};

/*
*  int cylinder_vertices (int qual)
*
*  Usage: 
*
*  int qual : 
*/
int cylinder_vertices (int qual)
{
  return 2*qual;
}

/*
*  int cylinder_indices (int qual)
*
*  Usage: 
*
*  int qual : 
*/
int cylinder_indices (int qual)
{
  return 2*(qual + 1);
}

/*
*  object_3d * draw_cylinder (int quality, float ra, float rb)
*
*  Usage: 
*
*  int quality : 
*  float ra    : 
*  float rb    : 
*/
object_3d * draw_cylinder (int quality, float ra, float rb)
{
  int i, j;
  object_3d * new_cylinder = g_malloc0 (sizeof*new_cylinder);
  new_cylinder -> quality = quality;
  new_cylinder -> num_vertices = cylinder_vertices (quality);
  new_cylinder -> vert_buffer_size = 3;
  new_cylinder -> vertices = allocfloat (3*new_cylinder -> num_vertices);
  new_cylinder -> num_indices = cylinder_indices (quality);
  new_cylinder -> ind_buffer_size = 1;
  new_cylinder -> indices = allocint (cylinder_indices (quality));

  float step = 2.0 * pi / (quality-1);
  float x, y;
  j = 0;
  // Vertex
  for(i = 0; i < quality; i++)
  {
    x = cos (i*step);
    y = sin (i*step);
    new_cylinder -> vertices[j] = ra*x;
    new_cylinder -> vertices[j+1] = ra*y;
    new_cylinder -> vertices[j+2] = 0.5;
    j += 3;
    new_cylinder -> vertices[j] = rb*x;
    new_cylinder -> vertices[j+1] = rb*y;
    new_cylinder -> vertices[j+2] = -0.5;
    j += 3;
  }

  for (i = 0; i < 2*quality; i++)
  {
    new_cylinder -> indices[i] = i % (2*quality-2);
  }

  return new_cylinder;
}

/*
*  int cap_vertices (int qual)
*
*  Usage: 
*
*  int qual : 
*/
int cap_vertices (int qual)
{
  return qual + 1;
}

/*
*  int cap_indices (int qual)
*
*  Usage: 
*
*  int qual : 
*/
int cap_indices (int qual)
{
  return qual + 2;
}

/*
*  object_3d * draw_cylinder_cap (int quality, float rad, gboolean picked)
*
*  Usage: 
*
*  int quality     : 
*  float rad       : 
*  gboolean picked : 
*/
object_3d * draw_cylinder_cap (int quality, float rad, gboolean picked)
{
  int i, j;

  object_3d * new_cap = g_malloc0 (sizeof*new_cap);
  new_cap -> quality = quality;
  new_cap -> num_vertices = cap_vertices(quality);
  new_cap -> vert_buffer_size = 3;
  new_cap -> vertices = allocfloat (3*new_cap -> num_vertices);
  new_cap -> num_indices = cap_indices(quality);
  new_cap -> ind_buffer_size = 1;
  new_cap -> indices = allocint (new_cap -> num_indices);

  float delta = (picked) ? 0.05 : 0.0;
  float step = 2.0 * pi / (quality - 1);
  float x, y;
  j = 0;
  // Center
  new_cap -> vertices[j] = 0.0;
  new_cap -> vertices[j+1] = 0.0;
  new_cap -> vertices[j+2] = 0.0 - delta;
  j += 3;
  for(i = 0; i < quality; i++)
  {
    x = cos (i*step);
    y = sin (i*step);
    new_cap -> vertices[j] = rad*x;
    new_cap -> vertices[j+1] = rad*y;
    new_cap -> vertices[j+2] = 0.0 - delta;
    j += 3;
  }

  for (i=0; i <= quality; i++)
  {
    new_cap -> indices[i] = i % (quality + 1);
  }
  return new_cap;
}

vec4_t rotate_bond (vec3_t a, vec3_t b)
{
  vec3_t c, f, raxis;
  c = v3_sub (vec3(b.x, b.y, b.z), vec3(a.x, a.y, a.z));
  f = vec3 (0.0, 0.0, 1.0);
  double rangle;
  raxis = v3_cross (f, c);
  //rangle = 180.0 / pi * acos(v3_dot(f, c) / v3_length(c));
  rangle = acos(v3_dot(f, c) / v3_length(c));
  return axis_to_quat (raxis, rangle);
}

/*
*  float get_bond_radius (int sty, int ac, int at, int bt, int sel)
*
*  Usage: 
*
*  int sty : 
*  int ac  : 
*  int at  : 
*  int bt  : 
*  int sel : 
*/
float get_bond_radius (int sty, int ac, int at, int bt, int sel)
{
  if (sty == BALL_AND_STICK)
  {
    return plot -> bondrad[at][bt] + 0.05*sel;
  }
  else if (sty == CYLINDERS)
  {
    return plot -> radall[ac] + 0.05*sel;
  }
  else
  {
    return plot -> linerad[at][bt] + 4.0*sel;
  }
}

/*
*  void setup_line_vertice (float * vertices, vec3_t pos, ColRGBA col, float alpha)
*
*  Usage: 
*
*  float * vertices : 
*  vec3_t pos       : 
*  ColRGBA col      : 
*  float alpha      : 
*/
void setup_line_vertice (float * vertices, vec3_t pos, ColRGBA col, float alpha)
{
  int s = nbs*LINE_BUFF_SIZE;
  vertices[s]   = pos.x;
  vertices[s+1] = pos.y;
  vertices[s+2] = pos.z;
  vertices[s+3] = col.red;
  vertices[s+4] = col.green;
  vertices[s+5] = col.blue;
  vertices[s+6] = col.alpha * alpha;
  nbs ++;
}

/*
*  void setup_cylinder_vertice (float * vertices, vec3_t pos_a, vec3_t pos_b, ColRGBA col, float rad, float alpha, float delta)
*
*  Usage: 
*
*  float * vertices : 
*  vec3_t pos_a     : 
*  vec3_t pos_b     : 
*  ColRGBA col      : 
*  float rad        : 
*  float alpha      : 
*  float delta      : 
*/
void setup_cylinder_vertice (float * vertices, vec3_t pos_a, vec3_t pos_b, ColRGBA col, float rad, float alpha, float delta)
{
  int s = nbs*CYLI_BUFF_SIZE;
  vertices[s] = (pos_a.x + pos_b.x)/2.0;
  vertices[s+1] = (pos_a.y + pos_b.y)/2.0;
  vertices[s+2] = (pos_a.z + pos_b.z)/2.0;
  vertices[s+3] = v3_length(v3_sub(pos_a, pos_b)) + delta;
  vertices[s+4] = rad;
  vec4_t quat = rotate_bond (pos_b, pos_a);
  vertices[s+5]  = quat.w;
  vertices[s+6] = quat.x;
  vertices[s+7] = quat.y;
  vertices[s+8] = quat.z;
  vertices[s+9] = col.red;
  vertices[s+10] = col.green;
  vertices[s+11] = col.blue;
  vertices[s+12] = col.alpha * alpha;
  nbs ++;
}

/*
*  void setup_cap_vertice (float * vertices, vec3_t pos_a, vec3_t pos_b, ColRGBA col, float rad, float alpha)
*
*  Usage: 
*
*  float * vertices : 
*  vec3_t pos_a     : 
*  vec3_t pos_b     : 
*  ColRGBA col      : 
*  float rad        : 
*  float alpha      : 
*/
void setup_cap_vertice (float * vertices, vec3_t pos_a, vec3_t pos_b, ColRGBA col, float rad, float alpha)
{
  int s = nbs*CAPS_BUFF_SIZE;
  vertices[s] = pos_b.x;
  vertices[s+1] = pos_b.y;
  vertices[s+2] = pos_b.z;
  vertices[s+3] = rad;
  vec4_t quat = rotate_bond (pos_b,  pos_a);
  vertices[s+4]  = quat.w;
  vertices[s+5] = quat.x;
  vertices[s+6] = quat.y;
  vertices[s+7] = quat.z;
  vertices[s+8] = col.red;
  vertices[s+9] = col.green;
  vertices[s+10] = col.blue;
  vertices[s+11] = col.alpha * alpha;
  nbs ++;
}

int vs_bid;

/*
*  void setup_this_bond (int sty, gboolean to_pick, gboolean picked, int cap, int bi, int pi, struct atom * at, struct atom * bt, float al, float * vertices)
*
*  Usage: 
*
*  int sty          : 
*  gboolean to_pick : 
*  gboolean picked  : 
*  int cap          : 
*  int bi           : 
*  int pi           : 
*  struct atom * at : 
*  struct atom * bt : 
*  float al         : 
*  float * vertices : 
*/
void setup_this_bond (int sty, gboolean to_pick, gboolean picked, int cap, int bi, int pi, struct atom * at, struct atom * bt, float al, float * vertices)
{
  float alpha = 1.0;
  float delta = 0.0;
  float shift[3];
  int p, q, r;
  vec3_t pos_a, pos_b;
  ColRGBA col = get_atom_color (at -> sp, at -> id, 1.0, (picked) ? pi + 1 : 0, to_pick);
  float rad = get_bond_radius ((to_pick) ? BALL_AND_STICK : (sty == NONE) ? plot -> style : sty, bi, at -> sp, bt -> sp, (picked) ? 1.0 : 0.0);
  if (! cap && picked)
  {
    gboolean show_a, show_b;
    int sta, stb;
    if (in_movie_encoding && plot -> at_data != NULL)
    {
      show_a = plot -> at_data[bt -> id].show[bi];
      show_b = plot -> at_data[bt -> id].pick[pi];
      sta = plot -> at_data[at -> id].style;
      stb = plot -> at_data[bt -> id].style;
    }
    else
    {
      show_a = bt -> show[bi];
      show_b = bt -> pick[pi];
      sta = at -> style;
      stb = bt -> style;
    }
    delta = ((show_a && show_b) && (sta == stb)) ? 0.0 : 0.1;
  }
  for (p=0; p<plot -> extra_cell[0]+1;p++)
  {
    for (q=0; q<plot -> extra_cell[1]+1; q++)
    {
      for (r=0; r<plot -> extra_cell[2]+1; r++)
      {
        shift[0]=p*box_gl -> vect[0][0]+q*box_gl -> vect[1][0]+r*box_gl -> vect[2][0];
        shift[1]=p*box_gl -> vect[0][1]+q*box_gl -> vect[1][1]+r*box_gl -> vect[2][1];
        shift[2]=p*box_gl -> vect[0][2]+q*box_gl -> vect[1][2]+r*box_gl -> vect[2][2];
        at_shift (at, shift);
        at_shift (bt, shift);
        pos_a = vec3(at -> x, at -> y, at -> z);
        pos_b = vec3((at -> x + bt -> x)/2.0, (at -> y + bt -> y)/2.0, (at -> z + bt -> z)/2.0);
        if (to_pick || ((sty == NONE && (plot -> style == BALL_AND_STICK || plot -> style == CYLINDERS)) || sty == BALL_AND_STICK || sty == CYLINDERS))
        {
          if (cap)
          {
            setup_cap_vertice (vertices, pos_a, pos_b, col, rad, alpha * al);
          }
          else
          {
            setup_cylinder_vertice (vertices, pos_a, pos_b, col, rad, (to_pick) ? 1.0 : alpha*al, delta);
          }
        }
        else
        {
          setup_line_vertice (vertices, pos_a, col, alpha*al);
          setup_line_vertice (vertices, pos_b, col, alpha*al);
        }
        at_unshift (at, shift);
        at_unshift (bt, shift);
        alpha = 0.5;
      }
    }
  }
}

/*
*  void prepare_bond (int sty, gboolean to_pick, gboolean picked, int cap, int bi, int pi, int bid, struct atom * at, struct atom * bt, float * vertices)
*
*  Usage: 
*
*  int sty          : 
*  gboolean to_pick : 
*  gboolean picked  : 
*  int cap          : 
*  int bi           : 
*  int pi           : 
*  int bid          : 
*  struct atom * at : 
*  struct atom * bt : 
*  float * vertices : 
*/
void prepare_bond (int sty, gboolean to_pick, gboolean picked, int cap, int bi, int pi, int bid, struct atom * at, struct atom * bt, float * vertices)
{
  if (bi == 0)
  {
    setup_this_bond (sty, to_pick, picked, cap, bi, pi, at, bt, 1.0, vertices);
  }
  else
  {
    struct atom * tmp_a, * tmp_b;
    float x, y, z;
    int sign;
    sign = 1;
    if (wingl -> bondid[step][1][bid][0] == at -> id) sign = -1;
    x = wingl -> clones[step][bid].x;
    y = wingl -> clones[step][bid].y;
    z = wingl -> clones[step][bid].z;
    tmp_a = duplicate_atom (at);
    tmp_b = duplicate_atom (at);
    tmp_a -> pick[pi] = bt -> pick[pi];
    tmp_a -> style = bt -> style;
    tmp_a -> sp = bt -> sp + proj_sp;
    tmp_b -> sp += proj_sp;
    tmp_a -> x += sign * x;
    tmp_a -> y += sign * y;
    tmp_a -> z += sign * z;
    setup_this_bond (sty, to_pick, picked, cap, bi, pi, tmp_b, tmp_a, 0.5, vertices);
    g_free (tmp_a);
    g_free (tmp_b);

    tmp_a = duplicate_atom (bt);
    tmp_b = duplicate_atom (bt);
    tmp_a -> pick[pi] = at -> pick[pi];
    tmp_a -> style = at -> style;
    tmp_a -> sp = at -> sp + proj_sp;
    tmp_b -> sp += proj_sp;
    tmp_a -> id = at -> id;
    tmp_a -> x -= sign * x;
    tmp_a -> y -= sign * y;
    tmp_a -> z -= sign * z;
    setup_this_bond (sty, to_pick, picked, cap, bi, pi, tmp_a, tmp_b, 0.5, vertices);
    g_free (tmp_a);
    g_free (tmp_b);
  }
}

/*
*  int find_bond_vertices (gboolean to_pick, int sty, int sa, int sb, int bi, int cap)
*
*  Usage: 
*
*  gboolean to_pick : 
*  int sty          : 
*  int sa           : 
*  int sb           : 
*  int bi           : 
*  int cap          : 
*/
int find_bond_vertices (gboolean to_pick, int sty, int sa, int sb, int bi, int cap)
{
  int i, j, k, l, m, n;
  gboolean show_a, show_b;
  l = 0;
  for (i=0; i < wingl -> bonds[step][bi]; i++)
  {
    j = wingl -> bondid[step][bi][i][0];
    k = wingl -> bondid[step][bi][i][1];
    if (in_movie_encoding && plot -> at_data != NULL)
    {
      show_a = plot -> at_data[j].show[bi];
      m = plot -> at_data[j].style;
      show_b = plot -> at_data[k].show[bi];
      n = plot -> at_data[k].style;
    }
    else
    {
      show_a = proj_gl -> atoms[step][j].show[bi];
      m = proj_gl -> atoms[step][j].style;
      show_b = proj_gl -> atoms[step][k].show[bi];
      n = proj_gl -> atoms[step][k].style;
    }
    if (proj_gl -> atoms[step][j].sp == sa && proj_gl -> atoms[step][k].sp == sb)
    {
      if (show_a && (m == sty || to_pick))
      {
        if (cap)
        {
          if (! show_b || n != sty) l += 1 + bi;
        }
        else
        {
          l += 1 + bi;
        }
      }
    }
    if (proj_gl -> atoms[step][j].sp == sb && proj_gl -> atoms[step][k].sp == sa)
    {
      if (show_b && (n == sty || to_pick))
      {
        if (cap)
        {
          if (! show_a || m != sty) l += 1 + bi;
        }
        else
        {
          l += 1 + bi;
        }
      }
    }
  }
  return 2*l;
}

/*
*  void setup_all_cylinder_vertices (int style, gboolean to_pick, int cap, int bi, float * vertices)
*
*  Usage: 
*
*  int style        : 
*  gboolean to_pick : 
*  int cap          : 
*  int bi           : 
*  float * vertices : 
*/
void setup_all_cylinder_vertices (int style, gboolean to_pick, int cap, int bi, float * vertices)
{
  int i, j, k, l, m;
  gboolean show_a, show_b;
  for (i=0; i < wingl -> bonds[step][bi]; i++)
  {
    j = wingl -> bondid[step][bi][i][0];
    k = wingl -> bondid[step][bi][i][1];
    if (in_movie_encoding && plot -> at_data != NULL)
    {
      show_a = plot -> at_data[j].show[bi];
      l = plot -> at_data[j].style;
      show_b = plot -> at_data[k].show[bi];
      m = plot -> at_data[k].style;;
    }
    else
    {
      show_a = proj_gl -> atoms[step][j].show[bi];
      l = proj_gl -> atoms[step][j].style;
      show_b = proj_gl -> atoms[step][k].show[bi];
      m = proj_gl -> atoms[step][k].style;
    }
    if (show_a && (l == style || to_pick))
    {
      if (cap)
      {
        if (! show_b || m != style) prepare_bond (style, to_pick, FALSE, cap, bi, 0, i, & proj_gl -> atoms[step][j], & proj_gl -> atoms[step][k], vertices);
      }
      else
      {
        prepare_bond (style, to_pick, FALSE, cap, bi, 0, i, & proj_gl -> atoms[step][j], & proj_gl -> atoms[step][k], vertices);
      }
    }
    if (show_b && (m == style || to_pick))
    {
      if (cap)
      {
        if (! show_a || l != style) prepare_bond (style, to_pick, FALSE, cap, bi, 0, i, & proj_gl -> atoms[step][k], & proj_gl -> atoms[step][j], vertices);
      }
      else
      {
        prepare_bond (style, to_pick, FALSE, cap, bi, 0, i, & proj_gl -> atoms[step][k], & proj_gl -> atoms[step][j], vertices);
      }
    }
  }
}

/*
*  void setup_line_vertices (int style, int cap, int bi, int sa, int sb, float * vertices)
*
*  Usage: 
*
*  int style        : 
*  int cap          : 
*  int bi           : 
*  int sa           : 
*  int sb           : 
*  float * vertices : 
*/
void setup_line_vertices (int style, int cap, int bi, int sa, int sb, float * vertices)
{
  int i, j, k, l, m;
  gboolean show_a, show_b;
  for (i=0; i < wingl -> bonds[step][bi]; i++)
  {
    j = wingl -> bondid[step][bi][i][0];
    k = wingl -> bondid[step][bi][i][1];
    if (in_movie_encoding && plot -> at_data != NULL)
    {
      show_a = plot -> at_data[j].show[bi];
      l = plot -> at_data[j].style;
      show_b = plot -> at_data[k].show[bi];
      m = plot -> at_data[k].style;
    }
    else
    {
      show_a = proj_gl -> atoms[step][j].show[bi];
      l = proj_gl -> atoms[step][j].style;
      show_b = proj_gl -> atoms[step][k].show[bi];
      m = proj_gl -> atoms[step][k].style;
    }
    if (proj_gl -> atoms[step][j].sp == sa && proj_gl -> atoms[step][k].sp == sb)
    {
      if (show_a && l == style)
      {
        prepare_bond (style, FALSE, FALSE, cap, bi, 0, i, & proj_gl -> atoms[step][j], & proj_gl -> atoms[step][k], vertices);
      }
    }
    if (proj_gl -> atoms[step][j].sp == sb && proj_gl -> atoms[step][k].sp == sa)
    {
      if (show_b && m == style)
      {
        prepare_bond (style, FALSE, FALSE, cap, bi, 0, i, & proj_gl -> atoms[step][k], & proj_gl -> atoms[step][j], vertices);
      }
    }
  }
}

/*
*  int create_bond_lists (gboolean to_pick)
*
*  Usage: 
*
*  gboolean to_pick : 
*/
int create_bond_lists (gboolean to_pick)
{
  int nshaders = 0;
  int **** nbonds;
  int **** ncaps;
  int nbds[7];
  int ncap[7];
  object_3d * cyl, * cap;
  int f, g, h, i, j, k, l, m;

  if (! to_pick)
  {
    cleaning_shaders (wingl, BONDS);
    wingl -> create_shaders[BONDS] = FALSE;
  }

  g = (plot -> draw_clones) ? 2 : 1;

  nbonds = allocqint (NUM_STYLES, g, proj_sp, proj_sp);
  if (! to_pick) ncaps = allocqint (NUM_STYLES, g, proj_sp, proj_sp);

  for (f=0; f<NUM_STYLES; f++)
  {
    nbds[f] = ncap[f] = 0;
    k = l = m = 0;
    if (to_pick || (! f && (plot -> style != SPHERES && plot -> style != PUNT)) || (f && f-1 != SPHERES && f-1 != PUNT))
    {
      for (h=0; h<g; h++)
      {
        for (i=0; i<proj_sp; i++)
        {
           for (j=0; j<proj_sp; j++)
           {
             nbonds[f][h][i][j] = find_bond_vertices (to_pick, f-1, i, j, h, 0);
             k += nbonds[f][h][i][j];
             if (nbonds[f][h][i][j] > 0) l ++;
             if (! to_pick && ((! f && (plot -> style == BALL_AND_STICK || plot -> style == CYLINDERS)) || (f && (f-1 == BALL_AND_STICK || f-1 == CYLINDERS))))
             {
               ncaps[f][h][i][j] = find_bond_vertices (to_pick, f-1, i, j, h, 1);
               m += ncaps[f][h][i][j];
             }
           }
        }
      }
      nbds[f] = k;
      ncap[f] = m;
      if (to_pick || (((! f && (plot -> style == BALL_AND_STICK || plot -> style == CYLINDERS)) || (f && (f-1 == BALL_AND_STICK || f-1 == CYLINDERS)))))
      {
        if (k > 0)
        {
          nshaders ++;
          if (m > 0 && ! to_pick) nshaders ++;
        }
      }
      else
      {
        nshaders += l;
      }
    }
    if (to_pick) break;
  }
#ifdef DEBUG
  g_debug ("Bond LIST:: to_pick= %s, shaders= %d", (to_pick) ? "true" : "false", nshaders);
#endif
  if (nshaders == 0) return nshaders;
  if (! to_pick) wingl -> ogl_glsl[BONDS][step] = g_malloc0 (nshaders*sizeof*wingl -> ogl_glsl[BONDS][step]);
  l = 0;
  for (f=0; f<NUM_STYLES; f++)
  {
    if (nbds[f])
    {
      if (to_pick || (! f && (plot -> style == BALL_AND_STICK || plot -> style == CYLINDERS)) || (f && (f-1 == BALL_AND_STICK || f-1 == CYLINDERS)))
      {
        cyl = draw_cylinder (plot -> quality, 1.0, 1.0);
        cyl -> num_instances =  (nbds[f]/2) * (plot -> extra_cell[0]+1)*(plot -> extra_cell[1]+1)*(plot -> extra_cell[2]+1);
        cyl -> inst_buffer_size = CYLI_BUFF_SIZE;
        cyl -> instances = allocfloat (CYLI_BUFF_SIZE*cyl -> num_instances);
        nbs = 0;
        for (h=0; h<g; h++)
        {
          setup_all_cylinder_vertices (f-1, to_pick, 0, h, cyl -> instances);
          if (to_pick && h==0) wingl -> bonds_to_be_picked = wingl -> to_be_picked;
        }
        if (! to_pick)
        {
          wingl -> ogl_glsl[BONDS][step][l] = init_shader_program (BONDS, GLSL_CYLINDERS, cylinder_vertex, NULL, full_color, GL_TRIANGLE_STRIP, 6, 1, TRUE, cyl);
          g_free (cyl);
          l ++;
          if (ncap[f] > 0)
          {
            cap = draw_cylinder_cap (plot -> quality, 1.0, FALSE);
            cap -> num_instances =  (ncap[f]/2) * (plot -> extra_cell[0]+1)*(plot -> extra_cell[1]+1)*(plot -> extra_cell[2]+1);
            cap -> inst_buffer_size = CAPS_BUFF_SIZE;
            cap -> instances = allocfloat (CAPS_BUFF_SIZE*cap -> num_instances);
            nbs = 0;
            for (h=0; h<g; h++)
            {
              setup_all_cylinder_vertices (f-1, FALSE, 1, h, cap -> instances);
            }
            wingl -> ogl_glsl[BONDS][step][l] = init_shader_program (BONDS, GLSL_CAPS, cap_vertex, NULL, full_color, GL_TRIANGLE_FAN, 5, 1, TRUE, cap);
            g_free (cap);
            l ++;
          }
        }
        else
        {
          wingl -> ogl_glsl[PICKS][0][1] = init_shader_program (PICKS, GLSL_CYLINDERS, cylinder_vertex, NULL, full_color, GL_TRIANGLE_STRIP, 6, 1, FALSE, cyl);
          g_free (cyl);
        }
      }
      else if ((! f && (plot -> style != SPHERES && plot -> style != PUNT)) || (f && f-1 != SPHERES && f-1 != PUNT))
      {
        for (h=0; h<g; h++)
        {
          for (i=0; i<proj_sp; i++)
          {
            for (j=0; j<proj_sp; j++)
            {
              if (nbonds[f][h][i][j])
              {
                cyl = g_malloc0 (sizeof*cyl);
                cyl -> vert_buffer_size = LINE_BUFF_SIZE;
                cyl -> num_vertices = nbonds[f][h][i][j] * (plot -> extra_cell[0]+1)*(plot -> extra_cell[1]+1)*(plot -> extra_cell[2]+1);
                cyl -> vertices = allocfloat (cyl -> vert_buffer_size*cyl -> num_vertices);
                nbs = 0;
                setup_line_vertices (f-1, 0, h, i, j, cyl -> vertices);
                wingl -> ogl_glsl[BONDS][step][l] = init_shader_program (BONDS, GLSL_LINES, line_vertex, NULL, line_color, GL_LINES, 2, 1, FALSE, cyl);
                wingl -> ogl_glsl[BONDS][step][l] -> line_width = get_bond_radius (WIREFRAME, h, i+proj_sp*h, j+proj_sp*h, FALSE);
                g_free (cyl);
                l++;
              }
            }
          }
        }
      }
    }
    if (to_pick) break;
  }
  g_free (nbonds);
  if (! to_pick) g_free (ncaps);
  return nshaders;
}
