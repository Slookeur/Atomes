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
#include "callbacks.h"
#include "interface.h"
#include "workspace.h"
#include "project.h"
#include "glview.h"
#include "glwindow.h"
#include "submenus.h"
#include "color_box.h"
#include "bind.h"
#include "atom_edit.h"

#define CONTEXTACT 12

extern atom_search * allocate_atom_search (int proj, int action, int searchid, int tsize);
extern void check_all_trees (struct project * this_proj);
extern int action_atoms_from_project (struct project * this_proj, atom_search * asearch, gboolean visible);
extern G_MODULE_EXPORT void opengl_advanced (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void render_gl_image (GtkWidget * widg, gpointer data);
extern G_MODULE_EXPORT void coord_properties (GtkWidget * widg, gpointer data);
extern void apply_project (gboolean showtools);
extern GtkWidget * field_atom_menu (int p, int s, int a, int f);
extern void reset_coordinates (struct project * this_proj, int status);
extern vec3_t get_bary (struct project * this_proj, int status);
extern void prepare_to_instert (gchar * key, struct project * this_proj, atom_search * asearch, gboolean visible);
extern struct insertion mol[];
extern struct selatom * new_selatom (int id, int sp);
extern void update_labels (glwin * view);
extern int inserted_from_lib;
extern void duplicate_material_and_lightning (image * new_img, image * old_img);
#ifdef GTK4
extern GSimpleActionGroup * view_pop_actions;
extern G_MODULE_EXPORT void window_color_coord (GSimpleAction * action, GVariant * parameter, gpointer data);
#endif
atom_search * remove_search = NULL;
atom_search * insert_search = NULL;

int selected_atom;
int selected_btom;
int selected_aspec;
int selected_bspec;
int is_selected;
int is_labelled;
int is_filled;
struct atom_selection * bond_selection = NULL;

tint atoid[CONTEXTACT][4];
dint btoid;

int get_to_be_selected (glwin * view)
{
  if (view -> atom_win && ! column_label)
  {
    return (view -> atom_win -> visible || view -> selection_mode == NSELECTION-1) ? 1 : 0;
  }
  else
  {
    return 0;
  }
}

#ifdef GTK4
G_MODULE_EXPORT void set_full_screen (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void set_full_screen (GtkWidget * widg, gpointer data)
#endif
{
  glwin * view = (glwin *)data;
  if (! view -> fullscreen)
  {
    gtk_window_fullscreen (GTK_WINDOW(view -> win));
    gtk_widget_hide (view -> menu_bar);
    view -> fullscreen = TRUE;
  }
  else
  {
    gtk_window_unfullscreen (GTK_WINDOW(view -> win));
    gtk_widget_show (view -> menu_bar);
    view -> fullscreen = FALSE;
  }
}

int get_style (gchar * str)
{
  int i;
  is_filled = NONE;
#ifdef GTK4
  i = StringLength(str);
  return (int) atof ((const gchar *)g_strdup_printf ("%c", str[i-1]));
#else
  for (i=0; i<OGL_STYLES; i++)
  {
    if (g_strcmp0 (text_styles[i], str) == 0) return i;
  }
#endif
  return NONE;
  /*for (i=0; i<FILLED_STYLES; i++)
  {
    if (g_strcmp0 (text_filled[i], str) == 0)
    {
      is_filled = i;
    }
  }
  return SPACEFILL;*/
}

atom_search * free_this_search_data (atom_search * this_search)
{
  struct project * this_proj = get_project_by_id(this_search -> proj);
  g_free (this_search);
  clean_other_window_after_edit (this_proj);
  if (this_proj -> modelgl -> atom_win)
  {
    if (! this_proj -> modelgl -> atom_win -> visible)
    {
      if (! this_proj -> modelgl -> cell_win || ! this_proj -> modelgl -> cell_win -> slab_passivate)
      {
        g_free (this_proj -> modelgl -> atom_win);
        this_proj -> modelgl -> atom_win = NULL;
      }
    }
  }
  return NULL;
}

void to_remove_this_object (int type, gpointer data)
{
  int i, j;
  struct selatom * selat;
  tint * sel;
  prepare_atom_edition (& opengl_project -> modelgl -> colorp[0][0], FALSE);
  remove_search = allocate_atom_search (opengl_project -> id, REMOVE, REMOVE, opengl_project -> natomes);
  switch (type)
  {
    case 0:
      i = GPOINTER_TO_INT(data);
      remove_search -> todo[i] = 1;
      break;
    case 1:
      sel = (tint * )data;
      i = opengl_project -> modelgl -> anim -> last -> img -> step;
      for (j=0; j<opengl_project -> natomes; j++)
      {
        if (sel -> a < 2)
        {
          if (opengl_project -> atoms[i][j].sp == selected_aspec && opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
          {
            remove_search -> todo[j] = 1;
          }
        }
        else
        {
          if (opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
          {
            remove_search -> todo[j] = 1;
          }
        }
      }
      break;
    case 2:
      sel = (tint * )data;
      i = opengl_project -> modelgl -> anim -> last -> img -> step;
      gboolean doit;
      for (j=0; j<opengl_project -> natomes; j++)
      {
        doit = FALSE;
        if (opengl_project -> atoms[i][j].sp == selected_aspec || selected_aspec == -1)
        {
          if (! sel -> a)
          {
            if (opengl_project -> atoms[i][j].pick[0] == is_selected || is_selected == -1) doit = TRUE;
          }
          else if (sel -> a == 2)
          {
            doit = TRUE;
          }
          else if (opengl_project -> atoms[i][j].label[0] == is_labelled)
          {
            if (opengl_project -> atoms[i][j].pick[0] == is_selected || is_selected == -1) doit = TRUE;
          }
          if (doit)
          {
            remove_search -> todo[j] = 1;
          }
        }
      }
      break;
    case 3:
      selat = bond_selection -> first;
      for (i=0; i<bond_selection -> selected ; i++)
      {
        j = selat -> id;
        remove_search -> todo[j] = 1;
        selat = selat -> next;
      }
      break;
  }
}

gchar * get_object_from_action (GSimpleAction * action)
{
  int i, j, k;
  const gchar * act = g_action_get_name(G_ACTION(action));
  gchar * act_end;
  gchar * str;
  k = strlen (act);
  if (strstr(act, "mol-ins"))
  {
    act_end = g_strdup_printf ("%c%c", act[k-2], act[k-1]);
    for (i=0; mol[i].type || mol[i].object; i++)
    {
      if (mol[i].object)
      {
        str = (i < 10) ? g_strdup_printf (".%d", i) : g_strdup_printf ("%d", i);
        if (g_strcmp0(str, act_end) == 0)
        {
          g_free (str);
          g_free (act_end);
          return g_strdup_printf ("%s", mol[i].object);
          break;
        }
        g_free (str);
      }
    }
    g_free (act_end);
  }
  if (strstr(act, "set-ifp"))
  {
    act_end = g_strdup_printf ("%c%c%c%c", act[k-4], act[k-3], act[k-2], act[k-1]);
    for (i=0; i<nprojects; i++)
    {
      if (get_project_by_id(i) -> steps == 1 && get_project_by_id(i) -> natomes)
      {
        for (j=0; j<3; j++)
        {
          str = (i < 10) ? g_strdup_printf (".%d.%d", i, j) : g_strdup_printf ("%d.%d", i, j);
          if (g_strcmp0(str, act_end) == 0)
          {
            g_free (str);
            g_free (act_end);
            return g_strdup_printf ("%s in %s", action_atoms[j], g_strdup_printf ("%s (%d)", prepare_for_title(get_project_by_id(i) -> name), i+1));
          }
          g_free (str);
        }
      }
    }
    g_free (act_end);
  }
  return NULL;
}

#ifdef GTK4
void to_replace_this_object (int type, GSimpleAction * action, gpointer data)
#else
void to_replace_this_object (int type, GtkWidget * widg, gpointer data)
#endif
{
  int h, i, j;
  struct selatom * selat;
  tint * sel;
  gboolean replace = TRUE;
  if (opengl_project -> modelgl -> cell_win)
  {
    if (opengl_project -> modelgl -> cell_win -> cut_this_slab) replace = FALSE;
  }
  if (replace)
  {
    prepare_atom_edition (& opengl_project -> modelgl -> colorp[0][0], FALSE);
    insert_search = allocate_atom_search (opengl_project -> id, REPLACE, REPLACE, opengl_project -> natomes);
  }
  else
  {
    insert_search = opengl_project -> modelgl -> search_widg[8];
  }
  gchar * lab;
#ifdef GTK4
  if (action != NULL)
  {
    lab = get_object_from_action (action);
  }
#else
  if (widg != NULL)
  {
    lab = g_strdup_printf ("%s", gtk_menu_item_get_label (GTK_MENU_ITEM(widg)));
  }
#endif
  else
  {
    lab = g_strdup_printf ("Copied Data");
  }
  h = get_selected_object_id (FALSE, opengl_project -> id,  lab, insert_search);
  g_free (lab);
  if (h != 0)
  {
    switch (type)
    {
      case 0:
        i = GPOINTER_TO_INT(data);
        to_insert_in_project (h, i, opengl_project, insert_search, FALSE);
        insert_search -> todo[i] = 1;
        break;
      case 1:
        sel = (tint * )data;
        i = opengl_project -> modelgl -> anim -> last -> img -> step;
        for (j=0; j<opengl_project -> natomes; j++)
        {
          if (sel -> a < 2)
          {
            if (opengl_project -> atoms[i][j].sp == selected_aspec && opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
            {
              to_insert_in_project (h, j, opengl_project, insert_search, FALSE);
              insert_search -> todo[j] = 1;
            }
          }
          else if (opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
          {
            to_insert_in_project (h, j, opengl_project, insert_search, FALSE);
            insert_search -> todo[j] = 1;
          }
        }
        break;
      case 2:
        sel = (tint * )data;
        gboolean doit;
        i = opengl_project -> modelgl -> anim -> last -> img -> step;
        for (j=0; j<opengl_project -> natomes; j++)
        {
          doit = FALSE;
          if (opengl_project -> atoms[i][j].sp == selected_aspec || selected_aspec == -1)
          {
            if (! sel -> a)
            {
              if (opengl_project -> atoms[i][j].pick[0] == is_selected || is_selected == -1) doit = TRUE;
            }
            else if (sel -> a == 2)
            {
              doit = TRUE;
            }
            else if (opengl_project -> atoms[i][j].label[0] == is_labelled)
            {
              if (opengl_project -> atoms[i][j].pick[0] == is_selected || is_selected == -1) doit = TRUE;
            }
            if (doit)
            {
              to_insert_in_project (h, j, opengl_project, insert_search, FALSE);
              insert_search -> todo[j] = 1;
            }
          }
        }
        break;
      case 3:
        selat = bond_selection -> first;
        for (i=0; i<bond_selection -> selected ; i++)
        {
          j = selat -> id;
          to_insert_in_project (h, j, opengl_project, insert_search, FALSE);
          insert_search -> todo[j] = 1;
          selat = selat -> next;
        }
        break;
    }
  }
}

void copy_bond_selection ()
{
  if (copied_object)
  {
    g_free (copied_object);
    copied_object = NULL;
  }
  pasted_todo = allocint (opengl_project -> natomes);
  struct selatom * selat = bond_selection -> first;
  while (selat)
  {
    pasted_todo[selat -> id] = 1;
    selat = selat -> next;
  }
  copied_object = create_object_from_selection (opengl_project);
}

void remove_object ()
{
  gchar * str;
  gboolean vis = (opengl_project -> modelgl -> atom_win) ? opengl_project -> modelgl -> atom_win -> visible : FALSE;
  int i = action_atoms_from_project (opengl_project, remove_search, vis);
  remove_search = free_this_search_data (remove_search);
  if (! opengl_project -> modelgl -> cell_win || ! opengl_project -> modelgl -> cell_win -> slab_passivate)
  {
    if (! i)
    {
      str = g_strdup_printf ("No atoms to be removed !");
    }
    else
    {
      str = g_strdup_printf ("%d atom(s) removed !", i);
    }
    show_info (str, 0, opengl_project -> modelgl -> win);
    g_free (str);
  }
  else
  {
    opengl_project -> modelgl -> search_widg[8] -> int_b = i;
  }
}

#ifdef GTK4
G_MODULE_EXPORT void remove_this_atom (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void remove_this_atom (GtkWidget * widg, gpointer data)
#endif
{
  to_remove_this_object (0, data);
  remove_object ();
}

void insert_object (int action, gpointer data)
{
  int i, j;
  gchar * str;
  gboolean vis = (opengl_project -> modelgl -> atom_win) ? opengl_project -> modelgl -> atom_win -> visible : FALSE;
  if (insert_search -> in_selection)
  {
    if (action == 1 && remove_search != NULL)
    {
      i = action_atoms_from_project (opengl_project, remove_search, vis);
    }
    j = action_atoms_from_project (opengl_project, insert_search, vis);
    if (action == 1)
    {
      str = g_strdup_printf ("%d atom(s) removed !\n%d atom(s) inserted !", i, j);
    }
    else
    {
      str = g_strdup_printf ("%d atom(s) inserted !", (inserted_from_lib) ? inserted_from_lib : j);
      inserted_from_lib = 0;
    }
    clean_other_window_after_edit (opengl_project);
    show_info (str, 0, opengl_project -> modelgl -> win);
    g_free (str);
  }
  insert_search = free_this_search_data (insert_search);
}

gboolean insert_this_object;

#ifdef GTK4
G_MODULE_EXPORT void add_object (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void add_object (GtkWidget * widg, gpointer data)
#endif
{
  prepare_atom_edition (data, FALSE);
  insert_search = allocate_atom_search (opengl_project -> id, INSERT, INSERT, 0);
  if (insert_this_object)
  {
    gchar * lab = NULL;
#ifdef GTK4
    if (action != NULL)
    {
      lab = get_object_from_action (action);
    }
#else
    if (widg != NULL)
    {
      lab = g_strdup_printf ("%s", gtk_menu_item_get_label (GTK_MENU_ITEM(widg)));
    }
#endif
    else
    {
      lab = g_strdup_printf ("Copied Data");
    }
    allocate_todo (insert_search, 1);
    insert_search -> todo[0] = 1;
    prepare_to_instert (lab, opengl_project, insert_search, FALSE);
    g_free (lab);
  }
  else if (copied_object)
  {
    tint ul = ulam_coord (opengl_project -> modelgl);
    opengl_project -> modelgl -> atom_win -> to_be_inserted[1] = duplicate_insert_object (copied_object);
    struct insert_object * object = opengl_project -> modelgl -> atom_win -> to_be_inserted[1];
    int i;
    for (i=0; i<object -> atoms; i++)
    {
      object -> at_list[i].x += opengl_project -> modelgl -> insert_coords.x + object -> dim*ul.a;
      object -> at_list[i].y += opengl_project -> modelgl -> insert_coords.y + object -> dim*ul.b;
      object -> at_list[i].z += opengl_project -> modelgl -> insert_coords.z + object -> dim*ul.c;
    }
    allocate_todo (insert_search, 1);
    insert_search -> todo[0] = 1;
    insert_search -> in_selection ++;
  }
  insert_object (3, data);
}

#ifdef GTK4
G_MODULE_EXPORT void to_add_object (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void to_add_object (GtkWidget * widg, gpointer data)
#endif
{
  insert_this_object = TRUE;
#ifdef GTK4
  add_object (action, parameter, data);
#else
  add_object (widg, data);
#endif
  insert_this_object = FALSE;
}

#ifdef GTK4
G_MODULE_EXPORT void replace_this_atom (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  to_replace_this_object (0, action, data);
#else
G_MODULE_EXPORT void replace_this_atom (GtkWidget * widg, gpointer data)
{
  to_replace_this_object (0, widg, data);
#endif
  to_remove_this_object (0, data);
  insert_object (1, & opengl_project -> modelgl -> colorp[0][0]);
}

#ifdef GTK4
G_MODULE_EXPORT void copy_this_atom (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void copy_this_atom (GtkWidget * widg, gpointer data)
#endif
{
  int i;
  if (pasted_todo)
  {
    g_free (pasted_todo);
    pasted_todo = NULL;
  }
  if (copied_object)
  {
    g_free (copied_object);
    copied_object = NULL;
  }
  pasted_todo = allocint (opengl_project -> natomes);
  i = GPOINTER_TO_INT(data);
  pasted_todo[i] = 1;
  copied_object = create_object_from_selection (opengl_project);
}

void check_hidden_visible (struct project * this_proj)
{
  int i, j, k, l, m;
  int ** num;
  int * numc[4];
  int * numv[4];
  int * numg[2];

  k = 2;
  num = allocdint (2, this_proj -> nspec);
  for (i=0; i<2; i++) if (this_proj -> modelgl -> adv_bonding[i]) k ++;
  for (i=0; i<k; i++)
  {
    numc[i] = allocint (this_proj -> coord -> totcoord[i]);
    numv[i] = allocint (this_proj -> coord -> totcoord[i]);
    if (i < 2)
    {
      numg[i] = allocint (this_proj -> nspec);
      for (j=1; j<this_proj -> nspec; j++)
      {
        numg[i][j] = this_proj -> coord -> ntg[i][j-1] + numg[i][j-1];
      }
    }
  }
  for (i=0; i<this_proj -> steps; i++)
  {
    for (j=0; j<this_proj -> natomes; j++)
    {
      for (l=0; l<2; l++) if (this_proj -> atoms[i][j].show[l]) num[l][this_proj -> atoms[i][j].sp] ++;
      for (l=0; l<k; l++)
      {
        m = this_proj -> atoms[i][j].coord[l];
        if (l < 2 && this_proj -> atoms[i][j].sp > 0) m += numg[l][this_proj -> atoms[i][j].sp];
        numc[l][m] ++;
        if (this_proj -> atoms[i][j].show[0]) numv[l][m] ++;
      }
    }
  }
#ifdef GTK4
  gboolean update_bar = FALSE;
#endif
  for (i=0; i<2; i++)
  {
    for (j=0; j<this_proj -> nspec; j++)
    {
      if (num[i][j] > 0)
      {
        if (num[i][j] == this_proj -> chemistry -> nsps[j]*this_proj -> steps)
        {
#ifdef GTK4
          if (! this_proj -> modelgl -> anim -> last -> img -> show_atom[i][j]) update_bar = TRUE;
#else
          // GTK3 Menu Action To Check
          check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_spec[i][j], TRUE);
#endif
          this_proj -> modelgl -> anim -> last -> img -> show_atom[i][j] = TRUE;
        }
      }
      else if (this_proj -> modelgl -> anim -> last -> img -> show_atom[i][j])
      {
#ifdef GTK4
         update_bar = TRUE;
#else
      // GTK3 Menu Action To Check
        check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_spec[i][j], FALSE);
#endif
        this_proj -> modelgl -> anim -> last -> img -> show_atom[i][j] = FALSE;
      }
    }
  }
  l = 0;
  for (i=0; i<k; i++)
  {
    if (is_coord_in_menu(i, this_proj))
    {
      for (j=0; j<this_proj -> coord -> totcoord[i]; j++)
      {
        if (numv[i][j] > 0)
        {
          if (numc[i][j] == numv[i][j])
          {
#ifdef GTK4
            if (! this_proj -> modelgl -> anim -> last -> img -> show_coord[i][j]) update_bar = TRUE;
#else
            // GTK3 Menu Action To Check
            for (l=0; l<2; l++) check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_geom[l][i][j], TRUE);
#endif
          }
          this_proj -> modelgl -> anim -> last -> img -> show_coord[i][j] = TRUE;
        }
        else if (this_proj -> modelgl -> anim -> last -> img -> show_coord[i][j])
        {
#ifdef GTK4
          update_bar = TRUE;
#else
          // GTK3 Menu Action To Check
          for (l=0; l<2; l++) check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_geom[l][i][j], FALSE);
#endif
          this_proj -> modelgl -> anim -> last -> img -> show_coord[i][j] = FALSE;
        }
      }
    }
  }
  for (i=0; i<2; i++) g_free (num[i]);
  g_free (num);
  for (i=0; i<k; i++)
  {
    g_free (numc[i]);
    g_free (numv[i]);
    if (i < 2) g_free (numg[i]);
  }
#ifdef GTK4
  if (update_bar) update_menu_bar (this_proj -> modelgl);
#endif
}

#ifdef GTK4
G_MODULE_EXPORT void show_hide_this_atom (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void show_hide_this_atom (GtkWidget * widg, gpointer data)
#endif
{
  int i;
  int id = GPOINTER_TO_INT (data);
  for (i=0; i<opengl_project -> steps; i++)
  {
    opengl_project -> atoms[i][id].show[0] = ! opengl_project -> atoms[i][id].show[0];
  }
  check_hidden_visible (opengl_project);
  init_default_shaders (opengl_project -> modelgl);
}

int check_label_numbers (struct project * this_proj, int types)
{
  int h, i, j, k, l;
  int start, end;
  int * naid = allocint (this_proj -> nspec);
  int * nlid = allocint (this_proj -> nspec);
  start = (types == 0 || types == 2) ? 0 : 1;
  end = (types == 1 || types == 2) ? 2 : 1;
  l = 0;
#ifdef GTK4
  gboolean update_bar = FALSE;
#endif
  for (h=start; h<end; h++)
  {
    for (i=0; i<this_proj -> nspec; i++) naid[i] = nlid[i] = 0;
    for (i=0; i<this_proj -> steps; i++)
    {
      for (j=0; j<this_proj -> natomes; j++)
      {
        k = this_proj -> atoms[i][j].sp;
        naid[k] ++;
        if (this_proj -> atoms[i][j].label[h]) nlid[k] ++;
      }
    }
    if (h==types || (h == 0 && types == 2)) l = 0;
    for (i=0; i<this_proj -> nspec; i++)
    {
      // g_debug ("h= %d, i= %d, nlid[%d]= %d, naid[%d]= %d", h, i+1, i+1, nlid[i], i+1, naid[i]);
      if (nlid[i] == naid[i])
      {
#ifdef GTK4
        if (! this_proj -> modelgl -> anim -> last -> img -> show_label[h][i]) update_bar = TRUE;
#else
        if (! check_menu_item_get_active ((gpointer)this_proj -> modelgl -> ogl_lab[h][i]))
        {
          check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_lab[h][i], 1);
        }
#endif
        this_proj -> modelgl -> anim -> last -> img -> show_label[h][i] = TRUE;

      }
      else
      {
        if (nlid[i] == 0)
        {
#ifdef GTK4
          if (this_proj -> modelgl -> anim -> last -> img -> show_label[h][i]) update_bar = TRUE;
#else
          if (check_menu_item_get_active ((gpointer)this_proj -> modelgl -> ogl_lab[h][i]))
          {
            check_menu_item_set_active ((gpointer)this_proj -> modelgl -> ogl_lab[h][i], 0);
          }
#endif
        }
        this_proj -> modelgl -> anim -> last -> img -> show_label[h][i] = FALSE;
      }
      if (h==types || (h == 0 && types == 2)) l += nlid[i];
    }
  }
  g_free (naid);
  g_free (nlid);
  check_all_trees (this_proj);
#ifdef GTK4
  if (update_bar) update_menu_bar (this_proj -> modelgl);
#endif
  return l;
}

#ifdef GTK4
G_MODULE_EXPORT void label_unlabel_this_atom (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void label_unlabel_this_atom (GtkWidget * widg, gpointer data)
#endif
{
  int i, j, k, l, m;
  int id = GPOINTER_TO_INT (data);
  i = opengl_project -> modelgl -> anim -> last -> img -> step;
  j = opengl_project -> atoms[i][id].label[0];
  for (k=0; k<opengl_project -> steps; k++)
  {
    opengl_project -> atoms[k][id].label[0] = ! j;
    opengl_project -> atoms[k][id].label[1] = ! j;
    if (opengl_project -> modelgl -> selection_mode == 1)
    {
      for (l=0; l<opengl_project -> atoms[k][id].numv; l++)
      {
        m = opengl_project -> atoms[k][id].vois[l];
        opengl_project -> atoms[k][m].label[0] = ! j;
        opengl_project -> atoms[k][m].label[1] = ! j;
      }
    }
  }
  opengl_project -> modelgl -> create_shaders[LABEL] = TRUE;
  if (check_label)
  {
    opengl_project -> modelgl -> labelled = check_label_numbers (opengl_project, 2);
    update (opengl_project -> modelgl);
  }
}

#ifdef GTK4
G_MODULE_EXPORT void select_unselect_this_atom (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void select_unselect_this_atom (GtkWidget * widg, gpointer data)
#endif
{
  int h, i, j, k, l, m;
  int id = GPOINTER_TO_INT (data);
  h = get_to_be_selected (opengl_project -> modelgl);
  j = opengl_project -> modelgl -> anim -> last -> img -> step;
  save_all_selections (opengl_project -> modelgl, h);
  k = opengl_project -> atoms[j][id].pick[h];
  for (i=0; i<opengl_project -> steps; i++)
  {
    if (i == j)
    {
      if (! opengl_project -> modelgl -> selection_mode || opengl_project -> atoms[i][id].pick[h] != selected_status)
      {
        process_selected_atom (opengl_project, opengl_project -> modelgl, id, 0, 0, h);
      }
      if (opengl_project -> modelgl -> selection_mode == 1)
      {
        for (l=0; l<opengl_project -> atoms[j][id].numv; l++)
        {
          m = opengl_project -> atoms[i][id].vois[l];
          if (opengl_project -> atoms[i][m].pick[h] != opengl_project -> atoms[i][id].pick[h])
          {
            process_selected_atom (opengl_project, opengl_project -> modelgl, m, 0, 0, h);
          }
        }
      }
    }
    else
    {
      if (! opengl_project -> modelgl -> selection_mode || opengl_project -> atoms[i][id].pick[h] != selected_status)
      {
        opengl_project -> atoms[i][id].pick[h] = ! k;
      }
      if (opengl_project -> modelgl -> selection_mode == 1)
      {
        for (l=0; l<opengl_project -> atoms[j][id].numv; l++)
        {
          m = opengl_project -> atoms[j][id].vois[l];
          opengl_project -> atoms[i][m].pick[h] = opengl_project -> atoms[i][id].pick[h];
        }
      }
    }
  }
  update_all_selections (opengl_project -> modelgl, h);
  int shaders[1] = {SELEC};
  re_create_md_shaders (1, shaders, opengl_project);
  opengl_project -> modelgl -> create_shaders[LABEL] = TRUE;
  if (opengl_project -> modelgl -> mode == EDITION)
  {
    if (opengl_project -> modelgl -> rebuild[0][1]) opengl_project -> modelgl -> rebuild[0][0] = TRUE;
    if (opengl_project -> modelgl -> rebuild[1][1]) opengl_project -> modelgl -> rebuild[1][0] = TRUE;
  }
  update (opengl_project -> modelgl);
}

gboolean wait_for_style = FALSE;

#ifdef GTK4
G_MODULE_EXPORT void style_this_atom (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void style_this_atom (GtkWidget * widg, gpointer data)
#endif
{
  int i, j;
  int id = GPOINTER_TO_INT (data);
#ifdef GTK4
  j = get_style ((gchar *)g_action_get_name(G_ACTION(action)));
#else
  j = get_style ((char *)gtk_menu_item_get_label (GTK_MENU_ITEM(widg)));
#endif
  for (i=0; i<opengl_project -> steps; i++)
  {
    opengl_project -> atoms[i][id].style =  j;
  }
  if (! wait_for_style)
  {
    init_default_shaders (opengl_project -> modelgl);
    update (opengl_project -> modelgl);
  }
}

#ifdef GTK4
G_MODULE_EXPORT void remove_the_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void remove_the_atoms (GtkWidget * widg, gpointer data)
#endif
{
  to_remove_this_object (2, data);
  remove_object ();
}

#ifdef GTK4
G_MODULE_EXPORT void replace_the_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  to_replace_this_object (2, action, data);
#else
G_MODULE_EXPORT void replace_the_atoms (GtkWidget * widg, gpointer data)
{
  to_replace_this_object (2, widg, data);
#endif
  to_remove_this_object (2, data);
  insert_object (1, & opengl_project -> modelgl -> colorp[0][0]);
  remove_search = free_this_search_data (remove_search);
}

#ifdef GTK4
G_MODULE_EXPORT void copy_the_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void copy_the_atoms (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint *)data;
  int h, i, j;
  if (copied_object)
  {
    g_free (copied_object);
    copied_object = NULL;
  }
  pasted_todo = allocint (opengl_project -> natomes);
  h = 0;
  i = opengl_project -> modelgl -> anim -> last -> img -> step;
  for (j=0; j<opengl_project -> natomes; j++)
  {
    if (opengl_project -> atoms[i][j].sp == selected_aspec || selected_aspec == -1)
    {
      if (! sel -> a || opengl_project -> atoms[i][j].label[0] == is_labelled)
      {
        if (opengl_project -> atoms[i][j].pick[0] == is_selected || is_selected == -1)
        {
          pasted_todo[j] = 1;
          h = 1;
        }
      }
    }
  }
  if (h)
  {
    copied_object = create_object_from_selection (opengl_project);
  }
  else
  {
    g_free (pasted_todo);
    pasted_todo = NULL;
  }
}

#ifdef GTK4
G_MODULE_EXPORT void show_hide_others (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void show_hide_others (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int i, j, k;

  for (i=0; i<opengl_project -> steps; i++)
  {
    for (j=0; j<opengl_project -> natomes; j++)
    {
      for (k=0; k<2; k++)
      {
        if (opengl_project -> atoms[i][j].show[k] ==  sel -> c)
        {
          opengl_project -> atoms[i][j].show[k] = ! sel -> c;
        }
      }
    }
  }
  check_hidden_visible (opengl_project);
  init_default_shaders (opengl_project -> modelgl);
}

#ifdef GTK4
G_MODULE_EXPORT void show_hide_the_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void show_hide_the_atoms (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int h, i, j;
  h = get_to_be_selected (opengl_project -> modelgl);
  for (i=0; i<opengl_project -> steps; i++)
  {
    for (j=0; j<opengl_project -> natomes; j++)
    {
      if (opengl_project -> atoms[i][j].sp == selected_aspec || selected_aspec == -1)
      {
        if (sel -> a == 2 || (opengl_project -> atoms[i][j].pick[h] == is_selected || is_selected == -1))
        {
          if (sel -> a || opengl_project -> atoms[i][j].label[0] == is_labelled)
          {
            opengl_project -> atoms[i][j].show[0] = sel -> c;
            opengl_project -> atoms[i][j].show[1] = sel -> c;
          }
        }
      }
    }
  }
  check_hidden_visible (opengl_project);
  init_default_shaders (opengl_project -> modelgl);
}

#ifdef GTK4
G_MODULE_EXPORT void label_unlabel_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void label_unlabel_atoms (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int h, i, j;
  h = get_to_be_selected (opengl_project -> modelgl);
  for (i=0; i<opengl_project -> steps; i++)
  {
    for (j=0; j<opengl_project -> natomes; j++)
    {
      if (opengl_project -> atoms[i][j].sp == selected_aspec || selected_aspec == -1)
      {
        if (sel -> a == 2 || (opengl_project -> atoms[i][j].pick[h] == is_selected || is_selected == -1))
        {
          opengl_project -> atoms[i][j].label[0] = sel -> c;
          opengl_project -> atoms[i][j].label[1] = sel -> c;
        }
      }
    }
  }
  opengl_project -> modelgl -> create_shaders[LABEL] = TRUE;
  if (check_label)
  {
    opengl_project -> modelgl -> labelled = check_label_numbers (opengl_project, 2);
    update (opengl_project -> modelgl);
  }
}

#ifdef GTK4
G_MODULE_EXPORT void select_unselect_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void select_unselect_atoms (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint *)data;
  int h, i, j;
  h = get_to_be_selected (opengl_project -> modelgl);
  save_all_selections (opengl_project -> modelgl, h);
  for (i=0; i<opengl_project -> steps; i++)
  {
    for (j=0; j<opengl_project -> natomes; j++)
    {
      if (opengl_project -> atoms[i][j].sp == selected_aspec || selected_aspec == -1)
      {
        if (! sel -> b || opengl_project -> atoms[i][j].label[0] == is_labelled)
        {
          if (i == opengl_project -> modelgl -> anim -> last -> img -> step)
          {
            if (opengl_project -> atoms[i][j].pick[h] != sel -> c)
            {
              process_selected_atom (opengl_project, opengl_project -> modelgl, j, 0, 0, h);
            }
          }
          else
          {
            if (opengl_project -> atoms[i][j].pick[h] != sel -> c)
            {
              opengl_project -> atoms[i][j].pick[h] = sel -> c;
            }
          }
        }
      }
    }
  }
  update_all_selections (opengl_project -> modelgl, h);
  int shaders[1] = {SELEC};
  re_create_md_shaders (1, shaders, opengl_project);
  opengl_project -> modelgl -> create_shaders[MEASU] = TRUE;
  opengl_project -> modelgl -> create_shaders[LABEL] = TRUE;
  if (opengl_project -> modelgl -> mode == EDITION)
  {
    if (opengl_project -> modelgl -> rebuild[0][1]) opengl_project -> modelgl -> rebuild[0][0] = TRUE;
    if (opengl_project -> modelgl -> rebuild[1][1]) opengl_project -> modelgl -> rebuild[1][0] = TRUE;
  }
  update (opengl_project -> modelgl);
}

#ifdef GTK4
G_MODULE_EXPORT void style_the_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void style_the_atoms (GtkWidget * widg, gpointer data)
#endif
{
  int h, i, j, k;
  tint * sel = (tint *)data;
  h = get_to_be_selected (opengl_project -> modelgl);
#ifdef GTK4
  k = get_style ((gchar *)g_action_get_name(G_ACTION(action)));
#else
  k = get_style ((char *)gtk_menu_item_get_label (GTK_MENU_ITEM(widg)));
#endif
  for (i=0; i<opengl_project -> steps; i++)
  {
    for (j=0; j<opengl_project -> natomes; j++)
    {
      if (opengl_project -> atoms[i][j].sp == selected_aspec || selected_aspec == -1)
      {
        if (sel -> a == 2 || (opengl_project -> atoms[i][j].pick[h] == is_selected || is_selected == -1))
        {
          if (sel -> a || opengl_project -> atoms[i][j].label[0] == is_labelled)
          {
            opengl_project -> atoms[i][j].style = k;
          }
        }
      }
    }
  }
  init_default_shaders (opengl_project -> modelgl);
  update (opengl_project -> modelgl);
}

#ifdef GTK4
G_MODULE_EXPORT void remove_the_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void remove_the_coord (GtkWidget * widg, gpointer data)
#endif
{
  to_remove_this_object (1, data);
  remove_object ();
}

#ifdef GTK4
G_MODULE_EXPORT void replace_the_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
{
  to_replace_this_object (1, action, data);
#else
G_MODULE_EXPORT void replace_the_coord (GtkWidget * widg, gpointer data)
{
  to_replace_this_object (1, widg, data);
#endif
  to_remove_this_object (1, data);
  insert_object (1, data);
}

#ifdef GTK4
G_MODULE_EXPORT void copy_the_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void copy_the_coord (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint *)data;
  int h, i, j;
  if (copied_object)
  {
    g_free (copied_object);
    copied_object = NULL;
  }
  pasted_todo = allocint (opengl_project -> natomes);
  h = 0;
  i = opengl_project -> modelgl -> anim -> last -> img -> step;
  for (j=0; j<opengl_project -> natomes; j++)
  {
    if (sel -> a < 2)
    {
      if (opengl_project -> atoms[i][j].sp == selected_aspec && opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
      {
        pasted_todo[j] = 1;
        h = 1;
      }
    }
    else
    {
      if (opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
      {
        pasted_todo[j] = 1;
        h = 1;
      }
    }
  }
  if (h)
  {
    copied_object = create_object_from_selection (opengl_project);
  }
  else
  {
    g_free (pasted_todo);
    pasted_todo = NULL;
  }
}

#ifdef GTK4
G_MODULE_EXPORT void show_hide_the_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void show_hide_the_coord (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int i, j, k;
  for (i=0; i<opengl_project -> steps; i++)
  {
    k = 0;
    for (j=0; j<opengl_project -> natomes; j++)
    {
      if (sel -> a < 2)
      {
        if (opengl_project -> atoms[i][j].sp == selected_aspec && opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
        {
          opengl_project -> atoms[i][j].show[0] = sel -> c;
          opengl_project -> atoms[i][j].show[1] = sel -> c;
        }
      }
      else
      {
        if (opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
        {
          opengl_project -> atoms[i][j].show[0] = sel -> c;
          opengl_project -> atoms[i][j].show[1] = sel -> c;
          k ++;
        }
      }
    }
  }
  check_hidden_visible (opengl_project);
  init_default_shaders (opengl_project -> modelgl);
}

#ifdef GTK4
G_MODULE_EXPORT void label_unlabel_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void label_unlabel_coord (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int i, j, k, l, m;
  if (sel -> a > 3) k = opengl_project -> coord -> geolist[sel -> a][0][sel -> b] - 1;
  for (i=0; i<opengl_project -> steps; i++)
  {
    for (j=0; j<opengl_project -> natomes; j++)
    {
      if (sel -> a < 2)
      {
        if (opengl_project -> atoms[i][j].sp == selected_aspec && opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
        {
          opengl_project -> atoms[i][j].label[0] = sel -> c;
          opengl_project -> atoms[i][j].label[1] = sel -> c;
          if (opengl_project -> modelgl -> selection_mode == 1)
          {
            for (l=0; l<opengl_project -> atoms[i][j].numv; l++)
            {
              m = opengl_project -> atoms[i][j].vois[l];
              opengl_project -> atoms[i][m].label[0] = sel -> c;
              opengl_project -> atoms[i][m].label[1] = sel -> c;
            }
          }
        }
      }
      else if (sel -> a < 4)
      {
        if (opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
        {
          opengl_project -> atoms[i][j].label[0] = sel -> c;
          opengl_project -> atoms[i][j].label[1] = sel -> c;
        }
      }
      else
      {
        if (opengl_project -> atoms[i][j].rings[sel -> a-4][k])
        {
          if (opengl_project -> atoms[i][j].rings[sel -> a-4][k][0])
          {
            opengl_project -> atoms[i][j].label[0] = sel -> c;
            opengl_project -> atoms[i][j].label[1] = sel -> c;
          }
        }
      }
    }
  }
  opengl_project -> modelgl -> create_shaders[LABEL] = TRUE;
  if (check_label)
  {
    opengl_project -> modelgl -> labelled = check_label_numbers (opengl_project, 2);
    update (opengl_project -> modelgl);
  }
}

#ifdef GTK4
G_MODULE_EXPORT void select_unselect_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void select_unselect_coord (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int h, i, j, k;
  h = get_to_be_selected (opengl_project -> modelgl);
  save_all_selections (opengl_project -> modelgl, h);
  if (sel -> a > 3) k = opengl_project -> coord -> geolist[sel -> a][0][sel -> b] - 1;
  for (i=0; i<opengl_project -> steps; i++)
  {
    for (j=0; j<opengl_project -> natomes; j++)
    {
      if (sel -> a < 2)
      {
        if (opengl_project -> atoms[i][j].sp == selected_aspec && opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
        {
          if (i == opengl_project -> modelgl -> anim -> last -> img -> step)
          {
            if (opengl_project -> atoms[i][j].pick[h] != sel -> c) process_selected_atom (opengl_project, opengl_project -> modelgl, j, 0, 0, h);
          }
          else
          {
            opengl_project -> atoms[i][j].pick[h] = sel -> c;
          }
        }
      }
      else if (sel -> a < 4)
      {
        if (opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
        {
          if (i == opengl_project -> modelgl -> anim -> last -> img -> step)
          {
            if (opengl_project -> atoms[i][j].pick[h] != sel -> c) process_selected_atom (opengl_project, opengl_project -> modelgl, j, 0, 0, h);
          }
          else
          {
            opengl_project -> atoms[i][j].pick[h] = sel -> c;
          }
        }
      }
      else
      {
        if (opengl_project -> atoms[i][j].rings[sel -> a-4][k])
        {
          if (opengl_project -> atoms[i][j].rings[sel -> a-4][k][0])
          {
            if (i == opengl_project -> modelgl -> anim -> last -> img -> step)
            {
              if (opengl_project -> atoms[i][j].pick[h] != sel -> c) process_selected_atom (opengl_project, opengl_project -> modelgl, j, 0, 0, h);
            }
            else
            {
              opengl_project -> atoms[i][j].pick[h] = sel -> c;
            }
          }
        }
      }
    }
  }
  update_all_selections (opengl_project -> modelgl, h);
  int shader[1] = {SELEC};
  re_create_md_shaders (1, shader, opengl_project);
  opengl_project -> modelgl -> create_shaders[LABEL] = TRUE;
  opengl_project -> modelgl -> create_shaders[MEASU] = TRUE;
  if (opengl_project -> modelgl -> mode == EDITION)
  {
    if (opengl_project -> modelgl -> rebuild[0][1]) opengl_project -> modelgl -> rebuild[0][0] = TRUE;
    if (opengl_project -> modelgl -> rebuild[1][1]) opengl_project -> modelgl -> rebuild[1][0] = TRUE;
  }
  update (opengl_project -> modelgl);
}

#ifdef GTK4
G_MODULE_EXPORT void style_the_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void style_the_coord (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int i, j, k, l;
  if (sel -> a > 3) k = opengl_project -> coord -> geolist[sel -> a][0][sel -> b] - 1;
#ifdef GTK4
  l = get_style ((gchar *)g_action_get_name(G_ACTION(action)));
#else
  l = get_style ((char *)gtk_menu_item_get_label (GTK_MENU_ITEM(widg)));
#endif
  for (i=0; i<opengl_project -> steps; i++)
  {
    for (j=0; j<opengl_project -> natomes; j++)
    {
      if (sel -> a < 2)
      {
        if (opengl_project -> atoms[i][j].sp == selected_aspec && opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
        {
          opengl_project -> atoms[i][j].style = l;
        }
      }
      else if (sel -> a < 4)
      {
        if (opengl_project -> atoms[i][j].coord[sel -> a] == sel -> b)
        {
          opengl_project -> atoms[i][j].style = l;
        }
      }
      else
      {
        if (opengl_project -> atoms[i][j].rings[sel -> a-4][k])
        {
          if (opengl_project -> atoms[i][j].rings[sel -> a-4][k][0])
          {
            opengl_project -> atoms[i][j].style = l;
          }
        }
      }
    }
  }
  init_default_shaders (opengl_project -> modelgl);
  update (opengl_project -> modelgl);
}

void create_new_project_using_data (struct atom_selection * selection)
{
  int i, j, k, l;
  int nspec = 0;
  int * pos_sp, * specs;
  struct selatom * tmp;

  pos_sp = allocint (opengl_project -> nspec);
  specs = allocint (opengl_project -> nspec);
  for (i=0; i<opengl_project -> nspec; i++)
  {
    specs[i] = 0;
    pos_sp[i] = -1;
  }

  tmp = selection -> first;
  j = -1;
  for (i=0; i<selection -> selected; i++)
  {
    specs[tmp -> sp] ++;
    if (pos_sp[tmp -> sp] < 0)
    {
      j ++;
      pos_sp[tmp -> sp] = j;
    }
    if (tmp -> next != NULL) tmp = tmp -> next;
  }

  for (i=0; i<opengl_project -> nspec; i++) if (specs[i] > 0) nspec ++;

  init_project (TRUE);
  active_project -> nspec = nspec;
  active_project -> natomes = selection -> selected;
  active_project -> steps = 1;
  alloc_proj_data (active_project, 1);
  active_chem = active_project -> chemistry;

  if (active_project -> natomes == opengl_project -> natomes)
  {
    active_cell -> ltype = opengl_project -> cell.ltype;
    active_cell -> pbc = opengl_project -> cell.pbc;
    k = (active_cell -> npt) ? opengl_project -> modelgl -> anim -> last -> img -> step : 0;
    for (i=0; i<3; i++)
    {
      for (j=0; j<3; j++)
      {
        if (i < 2) active_box -> param[i][j] = opengl_project -> cell.box[k].param[i][j];
        active_box -> vect[i][j] = opengl_project -> cell.box[k].vect[i][j];
      }
    }
  }

  for (i=0; i<opengl_project -> nspec; i++)
  {
    if (pos_sp[i] > -1)
    {
      k = pos_sp[i];
      active_chem -> label[k] = g_strdup_printf ("%s", opengl_project -> chemistry -> label[i]);
      active_chem -> element[k] = g_strdup_printf ("%s", opengl_project -> chemistry -> element[i]);
      active_chem -> nsps[k] = specs[i];
      for (j=0; j<CHEM_PARAMS; j++) active_chem -> chem_prop[j][k] = opengl_project -> chemistry -> chem_prop[j][i];
      for (j=0; j<opengl_project -> nspec; j++)
      {
        if (pos_sp[j] > -1)
        {
          l = pos_sp[j];
          active_chem -> cutoffs[k][l] = opengl_project -> chemistry -> cutoffs[i][j];
        }
      }
    }
  }
  active_chem -> grtotcutoff = opengl_project -> chemistry -> grtotcutoff;

  tmp = selection -> first;
  for (i=0; i<active_project -> steps; i++)
  {
    tmp = selection -> first;
    for (j=0; j<active_project -> natomes; j++)
    {
      k = tmp -> id;
      active_project -> atoms[i][j].id = j;
      active_project -> atoms[i][j].sp = pos_sp[tmp -> sp];
      active_project -> atoms[i][j].x = opengl_project -> atoms[i][k].x;
      active_project -> atoms[i][j].y = opengl_project -> atoms[i][k].y;
      active_project -> atoms[i][j].z = opengl_project -> atoms[i][k].z;
      active_project -> atoms[i][j].show[0] = TRUE;
      active_project -> atoms[i][j].show[1] = TRUE;
      active_project -> atoms[i][j].label[0] = FALSE;
      active_project -> atoms[i][j].label[1] = FALSE;
      active_project -> atoms[i][j].pick[0] = FALSE;
      active_project -> atoms[i][j].cloned = FALSE;
      active_project -> atoms[i][j].style = NONE;
      if (tmp -> next != NULL) tmp = tmp -> next;
    }
  }
  active_project -> runok[BD] = TRUE;
  active_project -> runok[RI] = TRUE;
  active_project -> runok[CH] = TRUE;
  active_project -> runok[SP] = TRUE;
  active_project_changed (activep);
  add_project_to_workspace ();
  i = opengl_project -> modelgl -> anim -> last -> img -> style;
  apply_project (FALSE);
  active_image -> style = i;
  update_all_menus (active_glwin, active_project -> natomes);
  g_free (pos_sp);
  g_free (specs);
  // Duplicate lightning and material
  duplicate_material_and_lightning (active_image, opengl_project -> modelgl -> anim -> last -> img);
  update (active_glwin);
}

#ifdef GTK4
G_MODULE_EXPORT void edit_in_new_project (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void edit_in_new_project (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int h, i, j;
  struct atom_selection * selected;
  struct selatom * tmp_a;
  selected = g_malloc0 (sizeof*selected);
  h = get_to_be_selected (opengl_project -> modelgl);
  if (! sel -> b)
  {
    if (sel -> c)
    {
      selected = opengl_project -> modelgl -> anim -> last -> img -> selected[h];
    }
    else
    {
      selected -> selected = opengl_project -> natomes - opengl_project -> modelgl -> anim -> last -> img -> selected[h] -> selected;
      j = opengl_project -> modelgl -> anim -> last -> img -> step;
      for (i=0; i<opengl_project -> natomes; i++)
      {
        if (! opengl_project -> atoms[j][i].pick[h])
        {
          if (selected -> first)
          {
            tmp_a -> next = new_selatom (i, opengl_project -> atoms[j][i].sp);
            tmp_a = tmp_a -> next;
          }
          else
          {
            selected -> first = new_selatom (i, opengl_project -> atoms[j][i].sp);
            tmp_a = selected -> first;
          }
        }
      }
    }
  }
  else
  {
    selected -> selected = (is_labelled) ? opengl_project -> modelgl -> labelled : opengl_project -> natomes - opengl_project -> modelgl -> labelled;
    j = opengl_project -> modelgl -> anim -> last -> img -> step;
    for (i=0; i<opengl_project -> natomes; i++)
    {
      if (opengl_project -> atoms[j][i].label[0] == is_labelled)
      {
        if (selected -> first)
        {
          tmp_a -> next = new_selatom (i, opengl_project -> atoms[j][i].sp);
          tmp_a = tmp_a -> next;
        }
        else
        {
          selected -> first = new_selatom (i, opengl_project -> atoms[j][i].sp);
          tmp_a = selected -> first;
        }
      }
    }
  }
  if (selected -> selected > 0) create_new_project_using_data (selected);
  selected = NULL;
  g_free (selected);
}

#ifdef GTK4
G_MODULE_EXPORT void edit_coord (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void edit_coord (GtkWidget * widg, gpointer data)
#endif
{
  tint * sel = (tint * )data;
  int i, j;
  struct atom_selection * selected;
  struct selatom * tmp_a;
  gboolean save_it;

  selected = g_malloc0 (sizeof*selected);
  j = opengl_project -> modelgl -> anim -> last -> img -> step;
  for (i=0; i<opengl_project -> natomes; i++)
  {
    save_it = FALSE;
    if (sel -> a < 2)
    {
      if (opengl_project -> atoms[j][i].sp == selected_aspec && opengl_project -> atoms[j][i].coord[sel -> a] == sel -> b)
      {
        save_it = TRUE;
      }
    }
    else
    {
      if (opengl_project -> atoms[j][i].coord[sel -> a] == sel -> b)
      {
        save_it = TRUE;
      }
    }
    if (save_it)
    {
      if (selected -> first)
      {
        tmp_a -> next = new_selatom (i, opengl_project -> atoms[j][i].sp);
        tmp_a = tmp_a -> next;
      }
      else
      {
        selected -> first = new_selatom (i, opengl_project -> atoms[j][i].sp);
        tmp_a = selected -> first;
      }
      selected -> selected ++;
    }
  }
  if (selected -> selected > 0) create_new_project_using_data (selected);
  g_free (selected);
}

#ifdef GTK4
G_MODULE_EXPORT void edit_atoms (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void edit_atoms (GtkWidget * widg, gpointer data)
#endif
{
  int i, j;
  struct atom_selection * selected;
  struct selatom * tmp_a;

  selected = g_malloc0 (sizeof*selected);
  j = opengl_project -> modelgl -> anim -> last -> img -> step;
  for (i=0; i<opengl_project -> natomes; i++)
  {
    if (opengl_project -> atoms[j][i].sp == selected_aspec || selected_aspec == -1)
    {
      if (selected -> first)
      {
        tmp_a -> next = new_selatom (i, opengl_project -> atoms[j][i].sp);
        tmp_a = tmp_a -> next;
      }
      else
      {
        selected -> first = new_selatom (i, opengl_project -> atoms[j][i].sp);
        tmp_a = selected -> first;
      }
      selected -> selected ++;
    }
  }
  if (selected -> selected > 0) create_new_project_using_data (selected);
  g_free (selected);
}

#ifdef GTK4
G_MODULE_EXPORT void select_action_for_all (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void select_action_for_all (GtkWidget * widg, gpointer data)
#endif
{
  int i = GPOINTER_TO_INT(data);
  selected_aspec = -1;
#ifdef DEBUG
  g_debug ("Action for All= %d", i);
#endif // DEBUG
#ifdef GTK4
  switch (i)
  {
    case 0:
      select_unselect_atoms (NULL, NULL, & opengl_project -> modelgl -> colorp[0][1]);
      break;
    case 1:
      select_unselect_atoms (NULL, NULL, & opengl_project -> modelgl -> colorp[0][0]);
      break;
    case 2:
      label_unlabel_atoms (NULL, NULL, & opengl_project -> modelgl -> colorp[0][1]);
      break;
    case 3:
      label_unlabel_atoms (NULL, NULL, & opengl_project -> modelgl -> colorp[0][0]);
      break;
    case 4:
      show_hide_the_atoms (NULL, NULL, & atoid[0][0]);
      break;
    case 5:
      show_hide_the_atoms (NULL, NULL, & atoid[1][1]);
      break;
    case 6:
      style_the_atoms (action, NULL, & atoid[0][1]);
      break;
    // No case 7 = color
    case 8:
      edit_in_new_project (NULL, NULL, & opengl_project -> modelgl -> colorp[0][is_selected]);
      break;
    case 9:
      remove_the_atoms (action, NULL, & cut_sel);
      break;
    case 10:
      replace_the_atoms (action, NULL, & cut_sel);
      break;
    case 11:
      copy_the_atoms (action, NULL, & cut_sel);
      break;
    case CONTEXTACT:
      select_unselect_atoms (NULL, NULL, & opengl_project -> modelgl -> colorp[1][1]);
      break;
    case CONTEXTACT+1:
      select_unselect_atoms (NULL, NULL, & opengl_project -> modelgl -> colorp[1][0]);
      break;
    case CONTEXTACT+2:
      is_selected = -1;
      label_unlabel_atoms (NULL, NULL, & atoid[0][0]);
      break;
    case CONTEXTACT+3:
      is_selected = -1;
      label_unlabel_atoms (NULL, NULL, & atoid[1][0]);
      break;
    case CONTEXTACT+4:
      is_selected = -1;
      show_hide_the_atoms (NULL, NULL, & atoid[0][0]);
      break;
    case CONTEXTACT+5:
      is_selected = -1;
      show_hide_the_atoms (NULL, NULL, & atoid[1][0]);
      break;
    case CONTEXTACT+6:
      is_selected = -1;
      style_the_atoms (action, NULL, & atoid[0][0]);
      break;
    case CONTEXTACT+8:
      is_selected = -1;
      edit_in_new_project (NULL, NULL, & opengl_project -> modelgl -> colorp[1][1]);
      break;
    case CONTEXTACT+9:
      is_selected = -1;
      remove_the_atoms (action, NULL, & cut_sel);
      break;
    case CONTEXTACT+10:
      is_selected = -1;
      replace_the_atoms (action, NULL, & cut_sel);
      break;
    case CONTEXTACT+11:
      is_selected = -1;
      copy_the_atoms (action, NULL, & cut_sel);
      break;
    case 2*CONTEXTACT+4:
      is_selected = -1;
      show_hide_the_atoms (NULL, NULL, & atoid[0][1]);
      break;
    case 2*CONTEXTACT+5:
      is_selected = -1;
      show_hide_the_atoms (NULL, NULL, & atoid[1][1]);
      break;
  }
#else
  switch (i)
  {
    case 0:
      select_unselect_atoms (widg, & opengl_project -> modelgl -> colorp[0][1]);
      break;
    case 1:
      select_unselect_atoms (widg, & opengl_project -> modelgl -> colorp[0][0]);
      break;
    case 2:
      label_unlabel_atoms (NULL, & opengl_project -> modelgl -> colorp[0][1]);
      break;
    case 3:
      label_unlabel_atoms (NULL, & opengl_project -> modelgl -> colorp[0][0]);
      break;
    case 4:
      show_hide_the_atoms (NULL, & atoid[0][0]);
      break;
    case 5:
      show_hide_the_atoms (NULL, & atoid[1][1]);
      break;
    case 6:
      style_the_atoms (widg, & atoid[0][1]);
      break;
    // No case 7 = color
    case 8:
      edit_in_new_project (NULL, & opengl_project -> modelgl -> colorp[0][is_selected]);
      break;
    case 9:
      remove_the_atoms (widg, & cut_sel);
      break;
    case 10:
      replace_the_atoms (widg, & cut_sel);
      break;
    case 11:
      copy_the_atoms (widg, & cut_sel);
      break;
    case CONTEXTACT:
      select_unselect_atoms (NULL, & opengl_project -> modelgl -> colorp[1][1]);
      break;
    case CONTEXTACT+1:
      select_unselect_atoms (NULL, & opengl_project -> modelgl -> colorp[1][0]);
      break;
    case CONTEXTACT+2:
      is_selected = -1;
      label_unlabel_atoms (NULL, & atoid[0][0]);
      break;
    case CONTEXTACT+3:
      is_selected = -1;
      label_unlabel_atoms (NULL, & atoid[1][0]);
      break;
    case CONTEXTACT+4:
      is_selected = -1;
      show_hide_the_atoms (NULL, & atoid[0][0]);
      break;
    case CONTEXTACT+5:
      is_selected = -1;
      show_hide_the_atoms (NULL, & atoid[1][0]);
      break;
    case CONTEXTACT+6:
      is_selected = -1;
      style_the_atoms (widg, & atoid[0][0]);
      break;
    case CONTEXTACT+8:
      is_selected = -1;
      edit_in_new_project (NULL, & opengl_project -> modelgl -> colorp[1][1]);
      break;
    case CONTEXTACT+9:
      is_selected = -1;
      remove_the_atoms (widg, & cut_sel);
      break;
    case CONTEXTACT+10:
      is_selected = -1;
      replace_the_atoms (widg, & cut_sel);
      break;
    case CONTEXTACT+11:
      is_selected = -1;
      copy_the_atoms (widg, & cut_sel);
      break;
    case 2*CONTEXTACT+4:
      is_selected = -1;
      show_hide_the_atoms (NULL, & atoid[0][1]);
      break;
    case 2*CONTEXTACT+5:
      is_selected = -1;
      show_hide_the_atoms (NULL, & atoid[1][1]);
      break;
  }
#endif
}

#ifdef GTK4
G_MODULE_EXPORT void select_action_for_this_bond (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void select_action_for_this_bond (GtkWidget * widg, gpointer data)
#endif
{
  int i, s;
  i = GPOINTER_TO_INT(data);
#ifdef GTK4
  switch (i)
  {
    case 0:
      select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(selected_atom));
      select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 1:
      select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(selected_atom));
      select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 2:
      label_unlabel_this_atom (NULL, NULL, GINT_TO_POINTER(selected_atom));
      label_unlabel_this_atom (NULL, NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 3:
      label_unlabel_this_atom (NULL, NULL, GINT_TO_POINTER(selected_atom));
      label_unlabel_this_atom (NULL, NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 4:
      show_hide_this_atom (NULL, NULL, GINT_TO_POINTER(selected_atom));
      show_hide_this_atom (NULL, NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 5:
      show_hide_this_atom (NULL, NULL, GINT_TO_POINTER(selected_atom));
      show_hide_this_atom (NULL, NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 6:
      style_this_atom (action, NULL, GINT_TO_POINTER(selected_atom));
      style_this_atom (action, NULL, GINT_TO_POINTER(selected_btom));
      break;
    default:
      //  8 = create project
      //  9 = remove
      // 10 = replace
      // 11 = copy
      bond_selection = g_malloc0(sizeof*bond_selection);
      s = opengl_project -> modelgl -> anim -> last -> img -> step;
      bond_selection -> first = new_selatom (selected_atom, opengl_project -> atoms[s][selected_atom].sp);
      bond_selection -> selected ++;
      bond_selection -> first -> next = new_selatom (selected_btom, opengl_project -> atoms[s][selected_btom].sp);
      bond_selection -> selected ++;
      switch (i)
      {
        case 8:
          create_new_project_using_data (bond_selection);
          break;
        case 9:
          to_remove_this_object (3, NULL);
          remove_object ();
          break;
        case 10:
          to_replace_this_object (3, action, data);
          to_remove_this_object (3, data);
          insert_object (1, & opengl_project -> modelgl -> colorp[0][0]);
          remove_search = free_this_search_data (remove_search);
          break;
        case 11:
          copy_bond_selection ();
          break;
      }
      break;
      g_free (bond_selection);
      bond_selection = NULL;
  }
#else
  switch (i)
  {
    case 0:
      select_unselect_this_atom (NULL, GINT_TO_POINTER(selected_atom));
      select_unselect_this_atom (NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 1:
      select_unselect_this_atom (NULL, GINT_TO_POINTER(selected_atom));
      select_unselect_this_atom (NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 2:
      label_unlabel_this_atom (NULL, GINT_TO_POINTER(selected_atom));
      label_unlabel_this_atom (NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 3:
      label_unlabel_this_atom (NULL, GINT_TO_POINTER(selected_atom));
      label_unlabel_this_atom (NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 4:
      show_hide_this_atom (NULL, GINT_TO_POINTER(selected_atom));
      show_hide_this_atom (NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 5:
      show_hide_this_atom (NULL, GINT_TO_POINTER(selected_atom));
      show_hide_this_atom (NULL, GINT_TO_POINTER(selected_btom));
      break;
    case 6:
      style_this_atom (widg, GINT_TO_POINTER(selected_atom));
      style_this_atom (widg, GINT_TO_POINTER(selected_btom));
      break;
    default:
      //  8 = create project
      //  9 = remove
      // 10 = replace
      // 11 = copy
      bond_selection = g_malloc0(sizeof*bond_selection);
      s = opengl_project -> modelgl -> anim -> last -> img -> step;
      bond_selection -> first = new_selatom (selected_atom, opengl_project -> atoms[s][selected_atom].sp);
      bond_selection -> selected ++;
      bond_selection -> first -> next = new_selatom (selected_btom, opengl_project -> atoms[s][selected_btom].sp);
      bond_selection -> selected ++;
      switch (i)
      {
        case 8:
          create_new_project_using_data (bond_selection);
          break;
        case 9:
          to_remove_this_object (3, NULL);
          remove_object ();
          break;
        case 10:
          to_replace_this_object (3, widg, data);
          to_remove_this_object (3, data);
          insert_object (1, & opengl_project -> modelgl -> colorp[0][0]);
          remove_search = free_this_search_data (remove_search);
          break;
        case 11:
          copy_bond_selection ();
          break;
      }
      break;
      g_free (bond_selection);
      bond_selection = NULL;
  }
#endif
}

#ifdef GTK4
G_MODULE_EXPORT void select_action_for_all_bonds (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void select_action_for_all_bonds (GtkWidget * widg, gpointer data)
#endif
{
  int g, h, i, j, k, l, m, n, o;
  int id = GPOINTER_TO_INT(data);
  int s = opengl_project -> modelgl -> anim -> last -> img -> step;
  g = get_to_be_selected (opengl_project -> modelgl);
  gboolean doit;
  gboolean eqtc, eqpc;
  bond_selection = NULL;
  struct selatom * tmp_a;
  if (id == 6 || id == 6+CONTEXTACT || id == 6+2*CONTEXTACT) wait_for_style = TRUE;
  for (h=0; h<2; h++)
  {
    for (i=0; i<opengl_project -> modelgl -> bonds[s][h]; i++)
    {
      doit = FALSE;
      k = opengl_project -> modelgl -> bondid[s][h][i][0];
      j = opengl_project -> modelgl -> bondid[s][h][i][1];
      l = opengl_project -> atoms[s][k].coord[0];
      m = opengl_project -> atoms[s][j].coord[0];
      n = opengl_project -> atoms[s][k].coord[1];
      o = opengl_project -> atoms[s][j].coord[1];
      if ((opengl_project -> atoms[s][j].sp == selected_aspec && opengl_project -> atoms[s][k].sp == selected_bspec) ||
          (opengl_project -> atoms[s][k].sp == selected_aspec && opengl_project -> atoms[s][j].sp == selected_bspec)) doit = TRUE;
      if (doit)
      {
        if (opengl_project -> atoms[s][j].sp == selected_aspec && opengl_project -> atoms[s][k].sp == selected_bspec)
        {
          eqtc = ((l == atoid[0][0].b && m == btoid.a) || (l == btoid.a && m == atoid[0][0].b)) ? TRUE : FALSE;
          eqpc = ((n == atoid[0][1].b && o == btoid.b) || (n == btoid.b && o == atoid[0][1].b)) ? TRUE : FALSE;
        }
        else
        {
          eqtc = ((l == atoid[0][0].b && m == btoid.a) || (l == btoid.a && m == atoid[0][0].b)) ? TRUE : FALSE;
          eqpc = ((n == atoid[0][1].b && o == btoid.b) || (n == btoid.b && o == atoid[0][1].b)) ? TRUE : FALSE;
        }
#ifdef GTK4
        if (id == 0 || (id == CONTEXTACT && eqtc) || (id == 2*CONTEXTACT && eqpc))
        {
          if (! opengl_project -> atoms[s][j].pick[g]) select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(j));
          if (! opengl_project -> atoms[s][k].pick[g]) select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(k));
        }
        else if (id == 1 || (id == CONTEXTACT+1 && eqtc) || (id == 2*CONTEXTACT+1 && eqpc))
        {
          if (opengl_project -> atoms[s][j].pick[g]) select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(j));
          if (opengl_project -> atoms[s][k].pick[g]) select_unselect_this_atom (NULL, NULL, GINT_TO_POINTER(k));
        }
        else if (id == 2 || (id == CONTEXTACT+2 && eqtc) || (id == 2*CONTEXTACT+2 && eqpc))
        {
          if (! opengl_project -> atoms[s][j].label[0]) label_unlabel_this_atom (NULL, NULL, GINT_TO_POINTER(j));
          if (! opengl_project -> atoms[s][k].label[0]) label_unlabel_this_atom (NULL, NULL, GINT_TO_POINTER(k));
        }
        else if (id == 3 || (id == CONTEXTACT+3 && eqtc) || (id == 2*CONTEXTACT+3 && eqpc))
        {
          if (opengl_project -> atoms[s][j].label[0]) label_unlabel_this_atom (NULL, NULL, GINT_TO_POINTER(j));
          if (opengl_project -> atoms[s][k].label[0]) label_unlabel_this_atom (NULL, NULL, GINT_TO_POINTER(k));
        }
        else if (id == 4 || (id == CONTEXTACT+4 && eqtc) || (id == 2*CONTEXTACT+4 && eqpc))
        {
          if (! opengl_project -> atoms[s][j].show[0]) show_hide_this_atom (NULL, NULL, GINT_TO_POINTER(j));
          if (! opengl_project -> atoms[s][k].show[0]) show_hide_this_atom (NULL, NULL, GINT_TO_POINTER(k));
        }
        else if (id == 5 || (id == CONTEXTACT+5 && eqtc) || (id == 2*CONTEXTACT+5 && eqpc))
        {
          if (opengl_project -> atoms[s][j].show[0]) show_hide_this_atom (NULL, NULL, GINT_TO_POINTER(j));
          if (opengl_project -> atoms[s][k].show[0]) show_hide_this_atom (NULL, NULL, GINT_TO_POINTER(k));
        }
        else if (id == 6 || (id == CONTEXTACT+6 && eqtc) || (id == 2*CONTEXTACT+6 && eqpc))
        {
          style_this_atom (action, NULL, GINT_TO_POINTER(j));
          style_this_atom (action, NULL, GINT_TO_POINTER(k));
        }
#else
        if (id == 0 || (id == CONTEXTACT && eqtc) || (id == 2*CONTEXTACT && eqpc))
        {
          if (! opengl_project -> atoms[s][j].pick[g]) select_unselect_this_atom (NULL, GINT_TO_POINTER(j));
          if (! opengl_project -> atoms[s][k].pick[g]) select_unselect_this_atom (NULL, GINT_TO_POINTER(k));
        }
        else if (id == 1 || (id == CONTEXTACT+1 && eqtc) || (id == 2*CONTEXTACT+1 && eqpc))
        {
          if (opengl_project -> atoms[s][j].pick[g]) select_unselect_this_atom (NULL, GINT_TO_POINTER(j));
          if (opengl_project -> atoms[s][k].pick[g]) select_unselect_this_atom (NULL, GINT_TO_POINTER(k));
        }
        else if (id == 2 || (id == CONTEXTACT+2 && eqtc) || (id == 2*CONTEXTACT+2 && eqpc))
        {
          if (! opengl_project -> atoms[s][j].label[0]) label_unlabel_this_atom (NULL, GINT_TO_POINTER(j));
          if (! opengl_project -> atoms[s][k].label[0]) label_unlabel_this_atom (NULL, GINT_TO_POINTER(k));
        }
        else if (id == 3 || (id == CONTEXTACT+3 && eqtc) || (id == 2*CONTEXTACT+3 && eqpc))
        {
          if (opengl_project -> atoms[s][j].label[0]) label_unlabel_this_atom (NULL, GINT_TO_POINTER(j));
          if (opengl_project -> atoms[s][k].label[0]) label_unlabel_this_atom (NULL, GINT_TO_POINTER(k));
        }
        else if (id == 4 || (id == CONTEXTACT+4 && eqtc) || (id == 2*CONTEXTACT+4 && eqpc))
        {
          if (! opengl_project -> atoms[s][j].show[0]) show_hide_this_atom (NULL, GINT_TO_POINTER(j));
          if (! opengl_project -> atoms[s][k].show[0]) show_hide_this_atom (NULL, GINT_TO_POINTER(k));
        }
        else if (id == 5 || (id == CONTEXTACT+5 && eqtc) || (id == 2*CONTEXTACT+5 && eqpc))
        {
          if (opengl_project -> atoms[s][j].show[0]) show_hide_this_atom (NULL, GINT_TO_POINTER(j));
          if (opengl_project -> atoms[s][k].show[0]) show_hide_this_atom (NULL, GINT_TO_POINTER(k));
        }
        else if (id == 6 || (id == CONTEXTACT+6 && eqtc) || (id == 2*CONTEXTACT+6 && eqpc))
        {
          style_this_atom (widg, GINT_TO_POINTER(j));
          style_this_atom (widg, GINT_TO_POINTER(k));
        }
#endif
        else if ((id == 8 || (id == CONTEXTACT+8 && eqtc) || (id == 2*CONTEXTACT+8 && eqpc))
              || (id == 9 || (id == CONTEXTACT+9 && eqtc) || (id == 2*CONTEXTACT+9 && eqpc))
              || (id == 10 || (id == CONTEXTACT+10 && eqtc) || (id == 2*CONTEXTACT+10 && eqpc))
              || (id == 11 || (id == CONTEXTACT+11 && eqtc) || (id == 2*CONTEXTACT+11 && eqpc)))
        {
          if (! bond_selection)
          {
            bond_selection = g_malloc0 (sizeof*bond_selection);
            bond_selection -> first = new_selatom (j, opengl_project -> atoms[s][j].sp);
            tmp_a = bond_selection -> first;
          }
          else
          {
            tmp_a -> next = new_selatom (j, opengl_project -> atoms[s][j].sp);
            tmp_a = tmp_a -> next;
          }
          bond_selection -> selected ++;
          tmp_a -> next = new_selatom (k, opengl_project -> atoms[s][k].sp);
          tmp_a = tmp_a -> next;
          bond_selection -> selected ++;
        }
      }
    }
  }
  if (id == 6 || id == CONTEXTACT+6 || id == 2*CONTEXTACT+6)
  {
    init_default_shaders (opengl_project -> modelgl);
    update (opengl_project -> modelgl);
    wait_for_style = FALSE;
  }
  else if (id == 8 || id == CONTEXTACT+8 || id == 2*CONTEXTACT+8)
  {
    if (bond_selection -> selected > 0) create_new_project_using_data (bond_selection);
    g_free (bond_selection);
    bond_selection = NULL;
  }
  else if (id == 9 || id == CONTEXTACT+9 || id == 2*CONTEXTACT+9)
  {
    if (bond_selection)
    {
      to_remove_this_object (3, data);
      remove_object ();
      g_free (bond_selection);
      bond_selection = NULL;
    }
  }
  else if (id == 10 || id == CONTEXTACT+10 || id == 2*CONTEXTACT+10)
  {
    if (bond_selection)
    {
#ifdef GTK4
      to_replace_this_object (3, action, data);
#else
      to_replace_this_object (3, widg, data);
#endif
      to_remove_this_object (3, data);
      insert_object (1, data);
      g_free (bond_selection);
      bond_selection = NULL;
    }
  }
  else if (id == 11 || id == CONTEXTACT+11 || id == 2*CONTEXTACT+11)
  {
    if (bond_selection)
    {
      copy_bond_selection ();
      g_free (bond_selection);
      bond_selection = NULL;
    }
  }
}

#ifdef GTK4
GMenu * add_edition_sub_menu (glwin * view, gchar * act, int aid, GCallback handler, gpointer data)
{
  GMenu * menu = g_menu_new ();
  GMenu * menus;
  gchar * word, * name, * str;
  gchar * eact = g_strdup_printf ("mol-ins-%s-%d", act, aid);
  int i, j;
  for (i=0; mol[i].type || mol[i].object; i++)
  {
    if (mol[i].type)
    {
      menus = g_menu_new ();
      g_menu_append_submenu (menu, mol[i].type, (GMenuModel*)menus);

    }
    if (mol[i].object)
    {
      append_opengl_item (view, menus, mol[i].object, eact, i, NULL, IMG_NONE, NULL, FALSE, handler, data, FALSE, FALSE, FALSE, TRUE);
    }
  }
  g_free (eact);

  gboolean doit = FALSE;
  for (i=0; i<nprojects; i++)
  {
    if (get_project_by_id(i) -> steps == 1 && get_project_by_id(i) -> natomes)
    {
      doit = TRUE;
      break;
    }
  }

  if (doit)
  {
    GMenu * menup = g_menu_new ();
    GMenu * menups;
    eact = g_strdup_printf ("ifp-%s-%d", act, aid);
    for (i=0; i<nprojects; i++)
    {
      if (get_project_by_id(i) -> steps == 1 && get_project_by_id(i) -> natomes)
      {
        name = g_strdup_printf ("%s (%d)", prepare_for_title(get_project_by_id(i) -> name), i+1);
        menups = g_menu_new ();
        for (j=0; j<3; j++)
        {
          word = g_strdup_printf ("%s in %s", action_atoms[j], name);
          str = g_strdup_printf ("%s-%d", eact, j);
          append_opengl_item (view, menups, word, str, i, NULL, IMG_NONE, NULL, FALSE, handler, data, FALSE, FALSE, FALSE, TRUE);
          g_free (word);
          g_free (str);
        }
        g_menu_append_submenu (menup, name, (GMenuModel*)menups);
        g_object_unref (menups);
        g_free (name);
      }
    }
    g_free (eact);
    g_menu_append_submenu (menu, "Import From Project", (GMenuModel*)menup);
    g_object_unref (menup);
  }
  eact = g_strdup_printf ("cp-%s-%d", act, aid);
  append_opengl_item (view, menu, "Copied Data", eact, 0, "<CTRL>V", IMG_NONE, NULL, FALSE, G_CALLBACK(add_object), data, FALSE, FALSE, FALSE, (copied_object) ? TRUE : FALSE);
  g_free (eact);
  return menu;
}

GMenu * add_style_sub_menu (glwin * view, gchar * act, int aid, GCallback handler, gpointer data)
{
  GMenu * menu = g_menu_new ();
  gchar * actc = g_strdup_printf ("%s-%d", act, aid);
  int i;
  for (i=0; i<OGL_STYLES; i++)
  {
    append_opengl_item (view, menu, text_styles[i], actc, i, NULL, IMG_NONE, NULL, FALSE, handler, data, FALSE, FALSE, FALSE, TRUE);
  }
  g_free (actc);
  return menu;
}
#else
void add_style_sub_menu (GtkWidget * item, GCallback handler, gpointer data)
{
  GtkWidget * menu = gtk_menu_new ();
  menu_item_set_submenu (item, menu);
  int i;
  for (i=0; i<OGL_STYLES; i++)
  {
    gtk3_menu_item (menu, text_styles[i], IMG_NONE, NULL, handler, data, FALSE, 0, 0, FALSE, FALSE, FALSE);
  }
}

/*void add_style_sub_menu (GtkWidget * item, GCallback handler, gpointer data)
{
  GtkWidget * menu = gtk_menu_new ();
  GtkWidget * pmenu;
  GtkWidget * sitem, * pitem;
  menu_item_set_submenu (item, menu);
  int i, j;
  for (i=0; i<OGL_STYLES; i++)
  {
    if (i != SPACEFILL)
    {
      sitem =  gtk3_menu_item (menu, text_stylesled[j], IMG_NONE, NULL, handler, data, FALSE, 0, 0, TRUE, TRUE, FALSE);
    }
    else
    {
      sitem = create_menu_item (FALSE, "Spacefilled");
      add_menu_child (menu, sitem);
      pmenu = gtk_menu_new ();
      menu_item_set_submenu (sitem, pmenu);
      for (j=0; j < FILLED_STYLES; j++)
      {
        pitem = gtk3_menu_item (menu, text_filled[j], IMG_NONE, NULL, handler, data, FALSE, 0, 0, TRUE, TRUE, FALSE);
      }
    }
  }
}*/

void add_edition_sub_menu (GtkWidget * item, GCallback handler, gpointer data)
{
  GtkWidget * menu = gtk_menu_new ();
  GtkWidget * smenu, * pmenu;
  GtkWidget * titem, * sitem;
  gchar * word, * name;
  menu_item_set_submenu (item, menu);
  int i, j;

  for (i=0; mol[i].type || mol[i].object; i++)
  {
    if (mol[i].type != NULL)
    {
      titem = create_menu_item (TRUE, mol[i].type);
      add_menu_child (menu, titem);
      smenu = gtk_menu_new ();
      menu_item_set_submenu (titem, smenu);
    }
    if (mol[i].object != NULL)
    {
      gtk3_menu_item (smenu, mol[i].object, IMG_NONE, NULL, handler, data, FALSE, 0, 0, FALSE, FALSE, FALSE);
    }
  }

  gboolean doit = FALSE;
  for (i=0; i<nprojects; i++)
  {
    if (get_project_by_id(i) -> steps == 1 && get_project_by_id(i) -> natomes)
    {
      doit = TRUE;
      break;
    }
  }

  if (doit)
  {
    titem = create_menu_item (TRUE, "Import From Project");
    add_menu_child (menu, titem);
    smenu = gtk_menu_new ();
    menu_item_set_submenu (titem, smenu);
    for (i=0; i<nprojects; i++)
    {
      if (get_project_by_id(i) -> steps == 1 && get_project_by_id(i) -> natomes)
      {
        name = g_strdup_printf ("%s (%d)", prepare_for_title(get_project_by_id(i) -> name), i+1);
        sitem = create_menu_item (TRUE, name);
        //child = gtk_bin_get_child (GTK_BIN (sitem));
        //gtk_label_set_use_markup (GTK_LABEL(child), 1);
        //g_signal_connect (G_OBJECT (sitem), "activate", handler, data);
        add_menu_child (smenu, sitem);
        pmenu = gtk_menu_new ();
        menu_item_set_submenu (sitem, pmenu);
        for (j=0; j<3; j++)
        {
          word = g_strdup_printf ("%s in %s", action_atoms[j], name);
          gtk3_menu_item (pmenu, word, IMG_NONE, NULL, handler, data, FALSE, 0, 0, FALSE, FALSE, FALSE);
          g_free (word);
        }
        g_free (name);
      }
    }
  }
  GtkWidget * pastem = gtk3_menu_item (menu, "Copied Data", IMG_NONE, NULL, G_CALLBACK(add_object), data, TRUE, GDK_KEY_v, GDK_CONTROL_MASK, FALSE, FALSE, FALSE);
  if (! copied_object) widget_set_sensitive (pastem, 0);
}
#endif

#ifdef GTK4
void create_selection_item (GMenu * menu, glwin * view, gchar * str, gchar * act, int aid, int id, int ig, int ic, int clone, GCallback handler, gpointer data)
{
   gchar * actc = g_strdup_printf ("%s-%d", act, aid);
#else
GtkWidget * create_selection_item (glwin * view, gchar * str, int id, int ig, int ic, int clone, GCallback handler, gpointer data)
{
  GtkWidget * sel_item = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
#endif
  int i, j;
  if (id == 6)
  {
#ifdef GTK4
    g_menu_append_submenu (menu, str, (GMenuModel *)add_style_sub_menu (view, act, aid, handler, data));
#else
    add_style_sub_menu (sel_item, handler, data);
#endif
  }
  else if (id == 7)
  {
    if (ic < 0)
    {
      i = get_project_by_id (view -> proj) -> nspec;
#ifdef GTK4
      g_menu_append_submenu (menu, str, (GMenuModel*)color_item(view, "Pick Color", actc, id, G_CALLBACK(to_run_atom_color_window), & view -> colorp[0][selected_aspec+clone*i]));
#else
      menu_item_set_submenu (sel_item, color_box(view, selected_aspec+clone*i, 0, 0));
#endif
    }
    else
    {
#ifdef GTK4
      i = ic;
      if (ig < 2)
      {
        for (j=0; j<selected_aspec; j++) i += opengl_project -> coord -> ntg[ig][j];
      }
      g_menu_append_submenu (menu, str, (GMenuModel*)color_item(view, "Pick Color", actc, id, G_CALLBACK(window_color_coord), & view -> gcid[ig][i][ig]));
#else
      int k;
      j = 2*opengl_project -> nspec;
      for (i=0; i < ig; i++)
      {
        j += opengl_project -> coord -> totcoord[i];
      }
      if (ig < 2)
      {
        for (i=0; i<selected_aspec; i++)
        {
          j += opengl_project -> coord -> ntg[ig][i];
        }
        k = selected_aspec;
      }
      else
      {
        k = ig;
      }
      menu_item_set_submenu (sel_item, color_box(view, j+ic, k, ic));
#endif
    }
  }
  else if (id == 10)
  {
#ifdef GTK4
    g_menu_append_submenu (menu, str, (GMenuModel *)add_edition_sub_menu (view, act, aid, handler, data));
#else
    add_edition_sub_menu (sel_item, handler, data);
#endif
  }
  else
  {
#ifdef GTK4
    append_opengl_item (view, menu, str, actc, 0, NULL, IMG_NONE, NULL, FALSE, handler, data, FALSE, FALSE, FALSE, TRUE);
#else
    g_signal_connect (G_OBJECT (sel_item), "activate", handler, data);
#endif
  }
#ifdef GTK4
  g_free (actc);
#else
  return sel_item;
#endif
}

gchar * mot[2][2]={{"All Non-Selected Atom(s)/Bond(s)", "All Selected Atom(s)/Bond(s)"},
                   {"All Non-Labelled Atom(s)/Bond(s)", "All Labelled Atom(s)/Bond(s)"}};

#ifdef GTK4
GMenu * selection_menu (int aid, glwin * view, int ai, int bi, int ac, int id,
                        GCallback handler_a, GCallback handler_b, GCallback handler_c)
#else
GtkWidget * selection_menu (glwin * view, int ai, int bi, int ac, int id,
                            GCallback handler_a, GCallback handler_b, GCallback handler_c)
#endif
{
  int p = view -> proj;
  int s = view -> anim -> last -> img -> step;
  int sa, sb;
  int i, j, k;
  gchar * str;

#ifdef GTK4
  gchar * strb;
  GMenu * menu = g_menu_new ();
#else
  GtkWidget * sel;
  GtkWidget * menu = gtk_menu_new ();
#endif
  struct project * this_proj = get_project_by_id(p);
  sa = this_proj -> atoms[s][ai].sp;
  if (bi > -1) sb = this_proj -> atoms[s][bi].sp;
  if (bi < 0 && (((id == 5 || id == 6) && this_proj -> atoms[s][ai].show[ac])
     || (id == 0 && ! is_selected) || (id == 1 && is_selected)
     || (id == 2 && ! is_labelled) || (id == 3 && is_labelled) || (id > 8 && id < 11)))
  {
    str = g_strdup_printf ("This Atom: %s<sub>%d</sub>", exact_name(this_proj -> chemistry -> label[sa]), ai+1);
#ifdef GTK4
    if (id == 6)
    {
      g_menu_append_submenu (menu, str, (GMenuModel *)add_style_sub_menu (view, "tas", aid, handler_a, GINT_TO_POINTER(ai)));
    }
    else if (id == 10)
    {
      g_menu_append_submenu (menu, str, (GMenuModel *)add_edition_sub_menu (view, "tae", aid, handler_a, GINT_TO_POINTER(ai)));
    }
    else
    {
      strb = g_strdup_printf ("act-%d-%d", aid, id);
      append_opengl_item (view, menu, str, strb, 0, NULL, IMG_NONE, NULL, FALSE, handler_a, GINT_TO_POINTER(ai), FALSE, FALSE, FALSE, TRUE);
      g_free (strb);
    }
#else
    sel = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
    add_menu_child (menu, sel);
    if (id == 6)
    {
      add_style_sub_menu (sel, handler_a, GINT_TO_POINTER(ai));
    }
    else if (id == 10)
    {
      add_edition_sub_menu (sel, handler_a, GINT_TO_POINTER(ai));
    }
    else
    {
      g_signal_connect (G_OBJECT (sel), "activate", handler_a, GINT_TO_POINTER(ai));
    }
#endif
  }
  else if (bi > -1 && id != 4 && id != 7)
  {
    str = g_strdup_printf ("This Bond: %s<sub>%d</sub> - %s<sub>%d</sub>",
                           exact_name(this_proj -> chemistry -> label[sa]), ai+1,
                           exact_name(this_proj -> chemistry -> label[sb]), bi+1);
#ifdef GTK4
    if (id == 6)
    {
      g_menu_append_submenu (menu, str, (GMenuModel *)add_style_sub_menu (view, "tb-s", aid, G_CALLBACK(select_action_for_this_bond), GINT_TO_POINTER(id)));
    }
    else if (id == 10)
    {
      g_menu_append_submenu (menu, str, (GMenuModel *)add_edition_sub_menu (view, "tb-e", aid, G_CALLBACK(select_action_for_this_bond), GINT_TO_POINTER(id)));
    }
    else
    {
      strb = g_strdup_printf ("act-%d-%d", aid, id);
      append_opengl_item (view, menu, str, strb, 0, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(select_action_for_this_bond), GINT_TO_POINTER(id), FALSE, FALSE, FALSE, TRUE);
      g_free (strb);
    }
#else
    sel = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
    if (id == 6)
    {
      add_style_sub_menu (sel, G_CALLBACK(select_action_for_this_bond), GINT_TO_POINTER(id));
    }
    else if (id == 10)
    {
      add_edition_sub_menu (sel, G_CALLBACK(select_action_for_this_bond), GINT_TO_POINTER(id));
    }
    else
    {
      g_signal_connect (G_OBJECT (sel), "activate", G_CALLBACK(select_action_for_this_bond), GINT_TO_POINTER(id));
    }
    add_menu_child (menu, sel);
#endif
    g_free (str);
  }

  if (id != 7)
  {
    k = 0;
    for (j=0; j<2; j++)
    {
      if (id<2 && j==0)
      {
        i = id;
      }
      else if ((id > 1 && id < 4) && j==1)
      {
        i = id - 2;
      }
      else
      {
        i = (j) ? is_labelled : is_selected;
      }
#ifdef GTK4
      if (id == 6)
      {
        g_menu_append_submenu (menu, mot[j][i], (GMenuModel *)add_style_sub_menu (view, "all-s", aid, G_CALLBACK(select_action_for_all), GINT_TO_POINTER(id+k)));
      }
      else  if (id == 10)
      {
        g_menu_append_submenu (menu, mot[j][i], (GMenuModel *)add_edition_sub_menu (view, "all-e", aid, G_CALLBACK(select_action_for_all), GINT_TO_POINTER(id+k)));
      }
      else
      {
        strb = g_strdup_printf ("act-all-%d-%d", aid, id+k);
        append_opengl_item (view, menu, mot[j][i], strb, 0, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(select_action_for_all), GINT_TO_POINTER(id+k), FALSE, FALSE, FALSE, TRUE);
        g_free (strb);
      }
#else
      sel = create_menu_item_from_widget (markup_label (mot[j][i], -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
      if (id == 6)
      {
        add_style_sub_menu (sel, G_CALLBACK(select_action_for_all), GINT_TO_POINTER(id+k));
      }
      else  if (id == 10)
      {
        add_edition_sub_menu (sel, G_CALLBACK(select_action_for_all), GINT_TO_POINTER(id+k));
      }
      else
      {
        g_signal_connect (G_OBJECT (sel), "activate", G_CALLBACK(select_action_for_all), GINT_TO_POINTER(id+k));
      }
      add_menu_child (menu, sel);
#endif
      k = CONTEXTACT;
    }
  }

  for (i=0; i<2; i++)
  {
    if (this_proj -> modelgl -> adv_bonding[i])
    {
      j = this_proj -> atoms[s][ai].coord[2+i];
      str = (! i) ? g_strdup_printf ("Fragment N°:\t<b>%d</b>", j+1) : g_strdup_printf ("Molecule N°:\t<b>%d</b>", j+1);
#ifdef GTK4
      create_selection_item (menu, view, str, (! i) ? "afm" : "amo", aid, id, 2+i, j, ac, handler_b, & atoid[id][2+i]);
#else
      add_menu_child (menu,  create_selection_item (view, str, id, 2+i, j, ac, handler_b, & atoid[id][2+i]));
#endif
      g_free (str);
    }
  }
  if (bi < 0)
  {
    if (this_proj -> atoms[s][ai].coord[0] > -1)
    {
      i = this_proj -> atoms[s][ai].coord[0];
      str = g_strdup_printf ("All <b>%d</b> Fold %s Atoms", this_proj -> coord -> geolist[0][sa][i], exact_name(this_proj -> chemistry -> label[sa]));
#ifdef GTK4
      create_selection_item (menu, view, str, "atc", aid, id, 0, i,  ac, handler_b, & atoid[id][0]);
#else
      add_menu_child (menu, create_selection_item(view, str, id, 0, i, ac, handler_b, & atoid[id][0]));
#endif
      g_free (str);
    }
    if (this_proj -> atoms[s][ai].coord[1] > -1)
    {
      i = this_proj -> atoms[s][ai].coord[1];
      str = g_strdup_printf ("All <b>%s</b> Coordinations", env_name (this_proj, i, sa, 1, NULL));
#ifdef GTK4
      create_selection_item (menu, view, str, "apc", aid, id, 1, i, ac, handler_b, & atoid[id][1]);
#else
      add_menu_child (menu, create_selection_item(view, str, id, 1, i, ac, handler_b, & atoid[id][1]));
#endif
      g_free (str);
    }
    if (! ac)
    {
      str = g_strdup_printf ("All <b>%s</b> Atoms", exact_name(this_proj -> chemistry -> label[sa]));
    }
    else
    {
      str = g_strdup_printf ("All <b>%s*</b> Clones", exact_name(this_proj -> chemistry -> label[sa]));
    }
#ifdef GTK4
    create_selection_item (menu, view, str, "asp", aid, id, 0, -1, ac, handler_c, & atoid[id][2]);
#else
    add_menu_child (menu, create_selection_item(view, str, id, 0, -1, ac, handler_c, & atoid[id][2]));
#endif
    g_free (str);
  }
  else
  {
    if (this_proj -> atoms[s][ai].coord[0] > -1 && this_proj -> atoms[s][bi].coord[0] > -1)
    {
      i = this_proj -> atoms[s][ai].coord[0];
      j = this_proj -> atoms[s][bi].coord[0];
      str = g_strdup_printf ("All %s <b>%d</b> - %s <b>%d</b> Bonds",
                             exact_name(this_proj -> chemistry -> label[sa]), this_proj -> coord -> geolist[0][sa][i],
                             exact_name(this_proj -> chemistry -> label[sb]), this_proj -> coord -> geolist[0][sb][j]);
#ifdef GTK4
     create_selection_item (menu, view, str, "atcb", aid, id, -1, -1, ac, G_CALLBACK(select_action_for_all_bonds), GINT_TO_POINTER(id+CONTEXTACT));
#else
     add_menu_child (menu, create_selection_item(view, str, id, -1, -1, ac, G_CALLBACK(select_action_for_all_bonds), GINT_TO_POINTER(id+CONTEXTACT)));
#endif
      g_free (str);
    }
    if (this_proj -> atoms[s][ai].coord[1] > -1 && this_proj -> atoms[s][bi].coord[1] > -1)
    {
      i = this_proj -> atoms[s][ai].coord[1];
      j = this_proj -> atoms[s][bi].coord[1];
      str = g_strdup_printf ("All <b>%s</b> - <b>%s</b> Bonds",  env_name (this_proj, i, sa, 1, NULL), env_name (this_proj, j, sb, 1, NULL));
#ifdef GTK4
      create_selection_item (menu, view, str, "apcb", aid, id, -1, -1, ac, G_CALLBACK(select_action_for_all_bonds), GINT_TO_POINTER(id+2*CONTEXTACT));
#else
      add_menu_child (menu, create_selection_item(view, str, id, -1, -1, ac, G_CALLBACK(select_action_for_all_bonds), GINT_TO_POINTER(id+2*CONTEXTACT)));
#endif
      g_free (str);
    }
    str = g_strdup_printf ("All %s - %s Bonds", exact_name(this_proj -> chemistry -> label[sa]), exact_name(this_proj -> chemistry -> label[sb]));
#ifdef GTK4
    create_selection_item (menu, view, str, "acb", aid, id, -1, -1, ac, G_CALLBACK(select_action_for_all_bonds), GINT_TO_POINTER(id));
#else
    add_menu_child (menu, create_selection_item(view, str, id, -1, -1, ac, G_CALLBACK(select_action_for_all_bonds), GINT_TO_POINTER(id)));
#endif
    g_free (str);
  }
  if (id > 3 && id < 6)
  {
    gchar * mat[2]={"All Hidden Atom(s)/Bond(s)", "All Visible Atom(s)/Bond(s)"};
#ifdef GTK4
    strb = g_strdup_printf ("all-hv-%d-%d", aid, id);
    append_opengl_item (view, menu, mat[id%2], strb, 0, NULL, IMG_NONE, NULL, FALSE, G_CALLBACK(select_action_for_all), GINT_TO_POINTER(id+2*CONTEXTACT), FALSE, FALSE, FALSE, TRUE);
    g_free (strb);
#else
    gtk3_menu_item (menu, mat[id%2], IMG_NONE, NULL, G_CALLBACK(select_action_for_all), GINT_TO_POINTER(id+2*CONTEXTACT), FALSE, 0, 0, FALSE, FALSE, FALSE);
#endif
  }
  return menu;
}

void popup_selection (glwin * view, double ptx, double pty, int se, int pe, int ai, int bi, int ac)
{
  int p = view -> proj;
  int s = view -> anim -> last -> img -> step;
  int i, j;
  gchar * str, * strp;
  gchar * menu_names[CONTEXTACT] = {"Select", "Unselect", "Label", "Unlabel", "Show", "Hide", "Style", "Color", "Edit as New Project", "Remove", "Replace", "Copy"};
  GCallback handlers[CONTEXTACT][3] = {{G_CALLBACK(select_unselect_this_atom), G_CALLBACK(select_unselect_coord), G_CALLBACK(select_unselect_atoms)},
                                       {G_CALLBACK(select_unselect_this_atom), G_CALLBACK(select_unselect_coord), G_CALLBACK(select_unselect_atoms)},
                                       {G_CALLBACK(label_unlabel_this_atom), G_CALLBACK(label_unlabel_coord), G_CALLBACK(label_unlabel_atoms)},
                                       {G_CALLBACK(label_unlabel_this_atom), G_CALLBACK(label_unlabel_coord), G_CALLBACK(label_unlabel_atoms)},
                                       {G_CALLBACK(show_hide_this_atom), G_CALLBACK(show_hide_the_coord), G_CALLBACK(show_hide_the_atoms)},
                                       {G_CALLBACK(show_hide_this_atom), G_CALLBACK(show_hide_the_coord), G_CALLBACK(show_hide_the_atoms)},
                                       {G_CALLBACK(style_this_atom), G_CALLBACK(style_the_coord), G_CALLBACK(style_the_atoms)},
                                       {NULL, NULL, NULL},
                                       {NULL, G_CALLBACK(edit_coord), G_CALLBACK(edit_atoms)},
                                       {G_CALLBACK(remove_this_atom), G_CALLBACK(remove_the_coord), G_CALLBACK(remove_the_atoms)},
                                       {G_CALLBACK(replace_this_atom), G_CALLBACK(replace_the_coord), G_CALLBACK(replace_the_atoms)},
                                       {G_CALLBACK(copy_this_atom), G_CALLBACK(copy_the_coord), G_CALLBACK(copy_the_atoms)}};
#ifdef GTK4
  if (view_pop_actions) g_object_unref (view_pop_actions);
  view_pop_actions = g_simple_action_group_new ();
  GMenu * gmenu = g_menu_new ();
#else
  GtkWidget * menu = gtk_menu_new ();
  GtkWidget * sel;
#endif
#ifdef DEBUG
  g_debug ("POP_SEL:: se= %d, ai= %d, bi= %d, ac= %d", se, ai, bi, ac);
#endif // DEBUG

  opengl_project_changed (p);
  selected_atom = ai;
  selected_btom = bi;
  selected_aspec = opengl_project -> atoms[s][ai].sp;
  selected_bspec = -1;
  selected_status = ! opengl_project -> atoms[s][ai].pick[get_to_be_selected (opengl_project -> modelgl)];
  is_selected = opengl_project -> atoms[s][ai].pick[pe];
  is_labelled = opengl_project -> atoms[s][ai].label[ac];
#ifdef DEBUG
  if (is_selected != se) g_debug ("Something is wrong in popup");
#endif // DEBUG

  for (i=0; i<CONTEXTACT; i++)
  {
    for (j=0; j<4; j++)
    {
      atoid[i][j].a = j;
      atoid[i][j].b = opengl_project -> atoms[s][ai].coord[j];
      atoid[i][j].c = ((i-2*(i/2)) == 0 ? 1 : 0);
/*#ifdef DEBUG
      g_debug ("atoid[%d][%d].a= %d, atoid[%d][%d].b= %d, atoid[%d][%d].c= %d",
               i, j, atoid[i][j].a, i, j, atoid[i][j].b, i, j, atoid[i][j].c);
#endif*/
    }
  }
  if (bi > -1)
  {
    btoid.a = opengl_project -> atoms[s][bi].coord[0];
    btoid.b = opengl_project -> atoms[s][bi].coord[1];
    selected_bspec = opengl_project -> atoms[s][bi].sp;
    struct distance dist = distance_3d (& opengl_project -> cell, (opengl_project -> cell.npt) ? s : 0, & opengl_project -> atoms[s][ai], & opengl_project -> atoms[s][bi]);
    if (ac == 0)
    {
      str = g_strdup_printf ("%s<sub>%d</sub> - %s<sub>%d</sub>",
                             exact_name(opengl_project -> chemistry -> label[selected_aspec]), ai+1, exact_name(opengl_project -> chemistry -> label[selected_bspec]), bi+1);
    }
    else
    {
      str = g_strdup_printf ("%s<sub>%d</sub> - %s<sub>%d</sub>",
                             exact_name(opengl_project -> chemistry -> label[selected_aspec]), ai+1, exact_name(opengl_project -> chemistry -> label[selected_bspec]), bi+1);
    }
    if (dist.pbc)
    {
      strp = g_strdup_printf ("<b>%s \t d= %.3f Å (PBC)</b>", str, dist.length);
    }
    else
    {
      strp = g_strdup_printf ("<b>%s \t d= %.3f Å</b>", str, dist.length);
    }
#ifdef GTK4
    append_opengl_item (view, gmenu, strp, "w-dist", 0, NULL, IMG_NONE, NULL, FALSE, NULL, NULL, FALSE, FALSE, FALSE, TRUE);
#else
    sel = create_menu_item_from_widget (markup_label (strp, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
    add_menu_child (menu, sel);
#endif
    g_free (strp);
    for (i=0; i<2; i++)
    {
      if (opengl_project -> modelgl -> adv_bonding[i])
      {
        j = opengl_project -> atoms[s][ai].coord[2+i] + 1;
        str = (! i) ? g_strdup_printf ("Fragment N°:\t<b>%d</b>", j) : g_strdup_printf ("Molecule N°:\t<b>%d</b>", j);
#ifdef GTK4
        append_opengl_item (view, gmenu, str, "w-dist-fm", 0, NULL, IMG_NONE, NULL, FALSE, NULL, NULL, FALSE, FALSE, FALSE, TRUE);
#else
        sel = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
        add_menu_child (menu, sel);
#endif
        g_free (str);
      }
    }
  }
  else
  {
    if (ac == 0)
    {
      str = g_strdup_printf ("<b>%s<sub>%d</sub></b>", exact_name(opengl_project -> chemistry -> label[selected_aspec]), ai+1);
    }
    else
    {
      str = g_strdup_printf ("<b>%s<sub>%d</sub><sup>*</sup></b>", exact_name(opengl_project -> chemistry -> label[selected_aspec]), ai+1);
    }
#ifdef GTK4
    append_opengl_item (view, gmenu, str, "w-ac", 0, NULL, IMG_NONE, NULL, FALSE, NULL, NULL, FALSE, FALSE, FALSE, TRUE);
#else
    sel = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
    add_menu_child (menu, sel);
#endif
    g_free (str);
    str = g_strdup_printf ("\tx : <b>%f</b>\n\ty : <b>%f</b>\n\tz : <b>%f</b>",
                           opengl_project -> atoms[s][ai].x, opengl_project -> atoms[s][ai].y, opengl_project -> atoms[s][ai].z);
#ifdef GTK4
    append_opengl_item (view, gmenu, str, "xyz", 0, NULL, IMG_NONE, NULL, FALSE, NULL, NULL, FALSE, FALSE, FALSE, TRUE);
#else
    sel = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
    add_menu_child (menu, sel);
#endif
    g_free (str);
    if (opengl_project -> atoms[s][ai].coord[0] > -1)
    {
      i = opengl_project -> atoms[s][ai].coord[0];
      str = g_strdup_printf ("Total Coordination:  <b>%d</b>", opengl_project -> coord -> geolist[0][selected_aspec][i]);
#ifdef GTK4
      append_opengl_item (view, gmenu, str, "w-tc", 0, NULL, IMG_NONE, NULL, FALSE, NULL, NULL, FALSE, FALSE, FALSE, TRUE);
#else
      sel = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
      add_menu_child (menu, sel);
#endif
      g_free (str);
      i = opengl_project -> atoms[s][ai].coord[1];
      str = g_strdup_printf ("Partial Coordination: <b>%s</b>",
                             env_name (opengl_project, i, selected_aspec, 1, NULL));
#ifdef GTK4
      append_opengl_item (view, gmenu, str, "w-pc", 0, NULL, IMG_NONE, NULL, FALSE, NULL, NULL, FALSE, FALSE, FALSE, TRUE);
#else
      sel = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
      add_menu_child (menu, sel);
#endif
      g_free (str);
    }
    for (i=0; i<2; i++)
    {
      if (opengl_project -> modelgl -> adv_bonding[i])
      {
        j = opengl_project -> atoms[s][ai].coord[2+i] + 1;
        str = (! i) ? g_strdup_printf ("Fragment N°:\t<b>%d</b>", j) : g_strdup_printf ("Molecule N°:\t<b>%d</b>", j);
#ifdef GTK4
        append_opengl_item (view, gmenu, str, "w-fm", 0, NULL, IMG_NONE, NULL, FALSE, NULL, NULL, FALSE, FALSE, FALSE, TRUE);
#else
        sel = create_menu_item_from_widget (markup_label (str, -1, -1, 0.0, 0.5), FALSE, FALSE, FALSE);
        add_menu_child (menu, sel);
#endif
        g_free (str);
      }
    }
  }

  //if (fc)
  //{
  //  add_menu_child (menu, sel);
  //  add_menu_separator (menu);
    // atoms menu
    //add_menu_child (menu, field_atom_menu(p, s, ai, fc-2));
    // bonds menu, if ai involved in bonding, or bi > -1
    //add_menu_child (menu, field_bond_menu(p, s, ai, bi));
    // angles menu, if ai involved in bonding with more than 2 neighbors
    //add_menu_child (menu, field_angle_menu(p, s, ai, bi));
  //}

#ifdef GTK4
  GMenu * menua = g_menu_new ();
  g_menu_append_section (gmenu, NULL, (GMenuModel *)menua);
#else
  add_menu_separator (menu);
#endif
  gboolean go;

  for (i=0; i<12; i++)
  {
    go = TRUE;
    if (i < 9 && view -> mode == EDITION) go = FALSE;
    if ((i > 8 && i < 11) && view -> mode == ANALYZE) go = FALSE;
    if (i == 0 && view -> anim -> last -> img -> selected[pe] -> selected == opengl_project -> natomes) go = FALSE;
    if (i == 1 && view -> anim -> last -> img -> selected[pe] -> selected == 0) go = FALSE;
    if (i == 2 && view -> labelled == opengl_project -> natomes*opengl_project -> steps) go = FALSE;
    if (i == 3 && view -> labelled == 0) go = FALSE;
    if (bi > -1 && i == 7) go = FALSE;
    j = (i == 8 && view -> mode == ANALYZE) ? 11 : (i == 11 && view -> mode == ANALYZE) ? 8 : i;
    if (go)
    {
#ifdef GTK4
      g_menu_append_submenu (menua, menu_names[j], (GMenuModel *)selection_menu (i, view, ai, bi, ac, j, handlers[j][0], handlers[j][1], handlers[j][2]));
#else
      add_menu_child (menu, menu_item_new_with_submenu(menu_names[j], TRUE, selection_menu (view, ai, bi, ac, j, handlers[j][0], handlers[j][1], handlers[j][2])));
#endif
    }
  }
#ifdef GTK4
  GtkWidget * menu = gtk_popover_menu_new_from_model_full ((GMenuModel *)gmenu, GTK_POPOVER_MENU_NESTED);
  gtk_widget_set_parent (menu,  view -> win);
  str = g_strdup_printf ("gl-%d", view -> action_id);
  gtk_widget_insert_action_group (menu, str, G_ACTION_GROUP(view_pop_actions));
  g_free (str);
  i = (view -> mode == ANALYZE) ? 410 : 260;
  i += (view -> adv_bonding[0] && bi < 0) ? 30 : 0;
  i += (view -> adv_bonding[1] && bi < 0) ? 30 : 0;
  if (bi > -1) i -= 80;
  gtk_widget_set_size_request (menu, -1, i);
  pop_menu_at_pointer (menu, ptx, pty);
#else
  pop_menu_at_pointer (menu, NULL);
#endif
}

#ifdef GTK4
G_MODULE_EXPORT void reset_coords (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void reset_coords (GtkWidget * widg, gpointer data)
#endif
{
  glwin * view = (glwin *)data;
  reset_coordinates (get_project_by_id(view -> proj), 2);
  int shaders[5] = {ATOMS, BONDS, POLYS, RINGS, SELEC};
  re_create_md_shaders (5, shaders, get_project_by_id(view -> proj));
  view -> create_shaders[MEASU] = TRUE;
  view -> create_shaders[PICKS] = TRUE;
  init_coordinates (get_project_by_id(view -> proj), 1, FALSE, TRUE);
  update (view);
}

#ifdef GTK4
G_MODULE_EXPORT void turn_rebuild (GSimpleAction * action, GVariant * parameter, gpointer data)
#else
G_MODULE_EXPORT void turn_rebuild (GtkWidget * widg, gpointer data)
#endif
{
  tint * dat =(tint *)data;
  glwin * view = get_project_by_id(dat -> a) -> modelgl;
#ifdef GTK4
  view -> rebuild[dat -> b][0] = ! view -> rebuild[dat -> b][0];
  view -> rebuild[dat -> b][1] = view -> rebuild[dat -> b][0];
  update_menu_bar (view);
#else
  view -> rebuild[dat -> b][0] = check_menu_item_get_active ((gpointer)widg);
  view -> rebuild[dat -> b][1] = view -> rebuild[dat -> b][0];
  if (widg != view -> rbuild[dat -> b])
  {
    check_menu_item_set_active ((gpointer)view -> rbuild[dat -> b], view -> rebuild[dat -> b][0]);
  }
#endif
}

G_MODULE_EXPORT void to_center_this_molecule (GtkWidget * widg, gpointer data)
{
  glwin * view = (glwin *)data;
  gboolean center = ! get_project_by_id(view -> proj) -> cell.crystal ? TRUE :
                    ask_yes_no ("Center crystal atom(s)", "Are you sure, this can affect the visual representation of the unit cell ?", GTK_MESSAGE_QUESTION, view -> win);
  if (center) center_this_molecule (view);
}

#ifdef GTK4
GMenu * tools_section (glwin * view, int popm)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Tools", (GMenuModel *)menu_tools(view, popm));
  return menu;
}

GMenu * anim_section (glwin * view)
{
  GMenu * menu = g_menu_new ();
  g_menu_append_submenu (menu, "Animate", (GMenuModel *)menu_anim(view));
  return menu;
}
#endif

void popup_main_menu (glwin * view, double ptx, double pty)
{
  GtkWidget * menu;
  opengl_project_changed (view -> proj);
#ifdef GTK4
  if (view_pop_actions) g_object_unref (view_pop_actions);
  view_pop_actions = g_simple_action_group_new ();
  GMenu * gmenu = g_menu_new ();
  if (view -> mode == ANALYZE)
  {
    g_menu_append_section (gmenu, NULL, (GMenuModel *)prepare_opengl_menu(view));
    if (get_project_by_id(view -> proj) -> natomes)
    {
      g_menu_append_section (gmenu, NULL, (GMenuModel *)prepare_model_menu(view));
      g_menu_append_section (gmenu, NULL, (GMenuModel *)prepare_coord_menu(view));
    }
    g_menu_append_section (gmenu, NULL, (GMenuModel *)tools_section(view, 0));
    g_menu_append_section (gmenu, NULL, (GMenuModel *)menu_view(view, 1));
    g_menu_append_section (gmenu, NULL, (GMenuModel *)anim_section(view));
  }
  else
  {
    g_menu_append_section (gmenu, NULL, (GMenuModel *)tools_section(view, 1));
    g_menu_append_submenu (gmenu, "Insert", (GMenuModel *)add_edition_sub_menu (view, "ins", 0, G_CALLBACK(to_add_object), & view -> colorp[0][0]));
    if (opengl_project -> steps == 1) g_menu_append_section (gmenu, NULL, (GMenuModel *)extract_section(view));
    append_opengl_item (view, gmenu, "Reset Motion", "res-mot", 0, NULL, IMG_STOCK, MEDIA_LOOP, FALSE, G_CALLBACK(reset_coords), view, FALSE, FALSE, FALSE, TRUE);
  }
  g_menu_append_section (gmenu, NULL, (GMenuModel*)menu_reset(view));
  g_menu_append_section (gmenu, NULL, (GMenuModel*)menu_fullscreen(view));

  menu = gtk_popover_menu_new_from_model_full ((GMenuModel *)gmenu, GTK_POPOVER_MENU_NESTED);
  gtk_popover_present ((GtkPopover *)menu);
  gchar * str = g_strdup_printf ("gl-%d", view -> action_id);
  gtk_widget_insert_action_group (menu, str, G_ACTION_GROUP(view_pop_actions));
  g_free (str);
  gtk_widget_set_parent (menu,  view -> win);
  gtk_widget_set_size_request (menu, -1, (view -> mode == ANALYZE) ? 750 : 292);
  pop_menu_at_pointer (menu, ptx, pty);
#else
  menu = gtk_menu_new ();
  GtkWidget * item;
  if (view -> mode == ANALYZE)
  {
    menu_items_opengl (menu, view, 1);
    add_menu_separator (menu);
    if (get_project_by_id(view -> proj) -> nspec)
    {
      add_menu_child (menu, menu_item_new_with_submenu ("Atom(s)", TRUE, menu_atoms (view, 1, 0)));
      add_menu_child (menu, menu_item_new_with_submenu ("Bond(s)", TRUE, menu_bonds (view, 1, 0)));
      add_menu_child (menu, menu_item_new_with_submenu ("Clone(s)", TRUE, menu_clones (view, 1)));
      add_menu_child (menu, menu_box_axis (view, 1, 0));
      add_menu_separator (menu);
      add_menu_child (menu, menu_coord (view, 1));
      add_menu_child (menu, menu_item_new_with_submenu ("Polyhedra", TRUE, menu_poly (view, 1)));
      add_menu_child (menu, menu_item_new_with_submenu ("Rings", view -> rings, menu_rings (view, 1, 0)));
      add_menu_child (menu, menu_item_new_with_submenu ("Chain(s)", view -> chains, add_menu_coord (view, 1, 9)));
      add_menu_child (menu, menu_item_new_with_submenu ("Fragment(s)", opengl_project -> coord -> totcoord[2], add_menu_coord (view, 1, 2)));
      add_menu_child (menu, menu_item_new_with_submenu ("Molecule(s)", opengl_project -> coord -> totcoord[3], add_menu_coord (view, 1, 3)));
#ifdef MENU_ICONS
      add_menu_child (menu, gtk3_image_menu_item ("Advanced", IMG_STOCK, (gpointer)DPROPERTIES, G_CALLBACK(coord_properties), (gpointer)& view -> colorp[30][0], "Ctrl+E", FALSE, FALSE, FALSE));
#else
      add_advanced_item (menu, G_CALLBACK(coord_properties), (gpointer)& view -> colorp[30][0], TRUE, GDK_KEY_e, GDK_CONTROL_MASK);
#endif
      add_menu_separator (menu);
    }
    add_menu_child (menu, menu_item_new_with_submenu ("Tools", TRUE, menu_tools(view, 1)));
    add_menu_separator (menu);
    menu_items_view (menu, view, 1);
    add_menu_separator (menu);
    add_menu_child (menu, menu_anim (view, 1));
  }
  else
  {
    item = create_menu_item (TRUE, "Tools");
    menu_item_set_submenu (item, menu_tools(view, 1));
    add_menu_child (menu, item);
    add_menu_separator (menu);

    item = create_menu_item (TRUE, "Insert");
    add_edition_sub_menu (item, G_CALLBACK(to_add_object), & view -> colorp[0][0]);
    add_menu_child (menu, item);
    add_menu_separator (menu);

    if (opengl_project -> steps == 1)
    {
      gtk3_menu_item (menu, "Extract/Rebuild on Motion", IMG_STOCK, (gpointer)ECUT, G_CALLBACK(turn_rebuild), & view -> colorp[0][0], FALSE, 0, 0, TRUE, FALSE, view -> rebuild[0][0]);
      gtk3_menu_item (menu, "Extract/Rebuild on Copy", IMG_STOCK, (gpointer)ECUT, G_CALLBACK(turn_rebuild), & view -> colorp[1][0], FALSE, 0, 0, TRUE, FALSE, view -> rebuild[1][0]);
    }
    gtk3_menu_item (menu, "Reset Motion", IMG_STOCK, (gpointer)MEDIA_LOOP, G_CALLBACK(reset_coords), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
  }
  add_menu_separator (menu);
  gtk3_menu_item (menu, "Reset View", IMG_STOCK, (gpointer)FITBEST, G_CALLBACK(to_reset_view), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
  gtk3_menu_item (menu, "Center Molecule", IMG_STOCK, (gpointer)FITBEST, G_CALLBACK(to_center_this_molecule), (gpointer)view, FALSE, 0, 0, FALSE, FALSE, FALSE);
  add_menu_separator (menu);
  if (! view -> fullscreen)
  {
#ifdef MENU_ICONS
    add_menu_child (menu, gtk3_image_menu_item ("Fullscreen", IMG_STOCK, (gpointer)FULLSCREEN, G_CALLBACK(set_full_screen), (gpointer)view, "Ctrl+F", FALSE, FALSE, FALSE));
#else
    item = gtk3_menu_item (menu, "Fullscreen", IMG_STOCK, (gpointer)FULLSCREEN, G_CALLBACK(set_full_screen), (gpointer)view, TRUE, GDK_KEY_f, GDK_CONTROL_MASK, FALSE, FALSE, FALSE);
#endif
  }
  else
  {
#ifdef MENU_ICONS
    add_menu_child (menu, gtk3_image_menu_item ("Exit Fullscreen", IMG_STOCK, (gpointer)FULLSCREEN, G_CALLBACK(set_full_screen), (gpointer)view, "Escape", FALSE, FALSE, FALSE));
#else
    item = gtk3_menu_item (menu, "Exit Fullscreen", IMG_STOCK, (gpointer)FULLSCREEN, G_CALLBACK(set_full_screen), (gpointer)view, TRUE, GDK_KEY_Escape, 0, FALSE, FALSE, FALSE);
#endif
  }
  pop_menu_at_pointer (menu, NULL);
#endif
}
