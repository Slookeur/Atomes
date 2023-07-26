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
* This header file: 'ogl_shading.h'
*
*  Contains: 

*/

#ifndef OGL_SHADING_H_
#define OGL_SHADING_H_

#define GLSL_POINTS     0
#define GLSL_SPHERES    1
#define GLSL_LINES      2
#define GLSL_CYLINDERS  3
#define GLSL_CAPS       4
#define GLSL_POLYEDRA   5
#define GLSL_STRING     6
#define GLSL_LIGHT      7

#define POLY_BUFF_SIZE 10  // p(x,y,z), n(x,y,z), color (r,g,b,a)
#define LINE_BUFF_SIZE  7  // p(x,y,z), color (r,g,b,a)
#define CYLI_BUFF_SIZE 13  // p(x,y,z), length, rad, quat(w,x,y,z), color (r,g,b,a)
#define CAPS_BUFF_SIZE 12  // p(x,y,z), rad, quat(w,x,y,z), color (r,g,b,a)
#define ATOM_BUFF_SIZE  8  // p(x,y,z), rad, color (r,g,b,a)
#define CHAR_BUFF_SIZE  4  // p(x,y), t(x,y)

// Points
extern const GLchar * point_vertex;
extern const GLchar * point_color;

// Basic lines
extern const GLchar * line_vertex;
extern const GLchar * line_color;
extern const GLchar * line_stipple;
extern const GLchar * line_stipple_color;
extern const GLchar * angle_vertex;
extern const GLchar * angle_stipple;
extern const GLchar * angle_color;

extern const GLchar * angstrom_vertex;
extern const GLchar * degree_vertex;

// Triangles: Multiple lights + Materials + Transparency shaders
extern const GLchar * full_vertex;
extern const GLchar * full_color;

// Sphere
extern const GLchar * sphere_vertex;

// Cylinder and caps
extern const GLchar * cylinder_vertex;
extern const GLchar * cone_vertex;
extern const GLchar * cap_vertex;

extern const GLchar * axis_sphere_vertex;
extern const GLchar * axis_cylinder_geom;
extern const GLchar * axis_line_vertex;

// Cylinder in geometry shader
extern const GLchar * gs_cylinder_vertex;
extern const GLchar * gs_cylinder_geom;

extern const GLchar * polyedron_vertex;
extern const GLchar * polyedron_geom;
extern const GLchar * polyedron_color;

extern const GLchar * axis_cylinder_geom;

extern const GLchar * pick_color;

extern const GLchar * string_vertex;
extern const GLchar * string_color;
extern const GLchar * string_color_2d;


typedef struct {
  int quality;
  int num_vertices;
  int vert_buffer_size;
  float * vertices;
  int num_indices;
  int ind_buffer_size;
  int * indices;
  int num_instances;
  int inst_buffer_size;
  float * instances;
  GLuint texture;       // Texture ID if any
  float shift[4];       // Texture shift if any (0-2: x,y,z), visible (3: always/normal)
} object_3d;

typedef struct {
  int id;
  int object;
  GLuint vertex_shader;
  GLuint geometry_shader;
  GLuint fragment_shader;
  GLenum vert_type;
  int draw_type;
  gboolean draw_instanced;
  GLuint vao;
  GLuint * vbo;
  GLuint * array_pointer;
  GLuint * uniform_loc;
  GLuint * light_uniform;
  object_3d * obj;
  float line_width;
  ColRGBA * col;
} glsl_program;

#endif
