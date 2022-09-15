/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include "global.h"
#include "glwindow.h"

void send_rings_opengl_ (int * id, int * st, int * ta, int * ri, int nring[* ta+1])
{
  int i;
  active_glwin -> show_rpoly[* id][* st][* ta][* ri] = FALSE;
  for (i=0; i< * ta+1; i++)
  {
    active_glwin -> all_rings[* id][* st][* ta][* ri][i] = nring[i] - 1;
  }
}

void send_atom_rings_id_opengl_ (int * st, int * at, int * id, int * ta, int * num, int ring[* num])
{
  if (ring != NULL)
  {
    int i;
    active_project -> atoms[* st][* at].rings[* id][* ta] = allocint(* num + 1);
    active_project -> atoms[* st][* at].rings[* id][* ta][0] = * num;
    for (i=0; i < * num; i++)
    {
      active_project -> atoms[* st][* at].rings[* id][* ta][i+1] = ring[i] - 1;
    }
  }
}

void allocate_all_rings_ (int * id, int * st, int * ta, int * nring)
{
  active_glwin -> all_rings[* id][* st][* ta - 1] = allocdint (* nring, * ta);
  active_glwin -> show_rpoly[* id][* st][* ta - 1] = allocint (* nring);
  active_glwin -> num_rings[* id][* st][* ta - 1] = * nring;
}
