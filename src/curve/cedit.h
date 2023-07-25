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
* This header file: 'cedit.h'
*
*  Contains: 

*  Called by: 

  curve/curve.c
  curve/m_curve.c
  curve/tab-1.c
  curve/tab-2.c
  curve/tab-3.c
  curve/tab-4.c
  curve/w_curve.c

*/

#ifndef CEDIT_H_
#define CEDIT_H_

#include "global.h"

extern GtkWidget * thesetbox;
extern GtkWidget * setcolorbox;
extern char * lapos[2];

void prepbox (tint * cd);
G_MODULE_EXPORT void edit_curve (GtkWidget * curve, gpointer cdata);
#endif
