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
#include "interface.h"
#include "gui.h"
#include "bind.h"
#include "color_box.h"
#include "glwindow.h"

void send_chains_opengl_ (int * st, int * ta, int * ri, int nchain[* ta])
{
  int i;
  for (i=0; i< * ta; i++)
  {
    active_glwin -> all_chains[* st][* ta - 1][* ri][i] = nchain[i] - 1;
  }
}

void send_atom_chains_id_opengl_ (int * st, int * at, int * ta, int * num, int nchain[* num])
{
  if (nchain != NULL)
  {
    int i;
    active_project -> atoms[* st][* at].chain[* ta - 1] = allocint(* num + 1);
    active_project -> atoms[* st][* at].chain[* ta - 1][0] = * num;
    for (i=0; i < * num; i++)
    {
      active_project -> atoms[* st][* at].chain[* ta - 1][i+1] = nchain[i] - 1;
    }
  }
}

void allocate_all_chains_ (int * st, int * ta, int * nring)
{
  active_glwin -> all_chains[* st][* ta - 1] = allocdint (* nring, * ta);
  active_glwin -> num_chains[* st][* ta - 1] = * nring;
}
