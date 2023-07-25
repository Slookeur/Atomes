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
* This header file: 'workspace.h'
*
*  Contains: 

*  Called by: 

  gui/calc_menu.c
  gui/callbacks.c
  gui/edit_menu.c
  gui/gui.c
  gui/main.c
  gui/tools.c
  gui/work_menu.c
  lic/valid.c
  project/close_p.c
  project/update_p.c
  opengl/edit/cbuild_action.c
  opengl/edit/cbuild_edit.c
  opengl/win/popup.c

*/

#ifndef WORKSPACE_H_
#define WORKSPACE_H_

void correct_this_window_title (GtkWidget * win, gchar * str);
G_MODULE_EXPORT void activate_project (GtkWidget * widg, gpointer data);
G_MODULE_EXPORT void change_project_name (GtkWidget * wid, gpointer edata);
void add_project_to_workspace ();
void remove_project_from_workspace ();
extern char * work_menu_items[NITEMS-2];
#endif
