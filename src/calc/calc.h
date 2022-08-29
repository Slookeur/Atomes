/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#ifndef CALC_H_
#define CALC_H_

#define MAXDATAQM 7

extern GtkWidget * qm_assistant;
extern struct project * qm_proj;
extern glwin * qm_view;
extern coord_info * qm_coord;
extern GtkTextBuffer * qmbuffer[MAXDATAQM+2];

extern gboolean force_mol;

extern int idopt;
extern int icalc;
extern int ident;
extern int icomb;

extern gboolean selection_confirmed;
extern G_MODULE_EXPORT void confirm_selection (GtkDialog * dialog, gint response_id, gpointer data);
extern void field_question (gchar * question, GCallback handler, gpointer data);

#endif
