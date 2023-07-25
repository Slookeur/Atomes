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
* This header file: 'bind.h'
*
*  Contains: 

*  Called by: 

  gui/bdcall.c
  gui/calc_menu.c
  gui/callbacks.c
  gui/chainscall.c
  gui/edit_menu.c
  gui/grcall.c
  gui/initc.c
  gui/interface.c
  gui/main.c
  gui/msdcall.c
  gui/ringscall.c
  gui/spcall.c
  gui/sqcall.c
  opengl/glview.c
  project/close_p.c
  project/init_p.c
  project/open_p.c
  project/project.c
  project/update_p.c
  calc/dl_poly/dlp_control.c
  calc/dl_poly/dlp_field.c
  opengl/edit/cbuild_action.c
  opengl/edit/cbuild_edit.c
  opengl/win/initchain.c
  opengl/win/initcoord.c
  opengl/win/initmol.c
  opengl/win/m_style.c
  opengl/win/popup.c
  opengl/win/w_coord.c
  opengl/win/w_crystal.c
  opengl/win/w_library.c
  opengl/win/w_volumes.c
  project/readers/read_c3d.c
  project/readers/read_cif.c
  project/readers/read_coord.c
  project/readers/read_hist.c
  project/readers/read_npt.c
  project/readers/read_pdb.c
  project/readers/read_trj.c
  project/readers/read_vas.c
  project/readers/read_xyz.c

*/

#ifndef BIND_H_
#define BIND_H_

extern int write_xyz_ (char *,
                       int *,
                       int *,
                       int *);

extern int read_xyz_ (char *,
                      int *,
                      int *);

extern int read_c3d_ (char *,
                      int *);

extern int write_c3d_ (char *,
                       int *,
                       int *,
                       int *);

extern int read_pdb_ (char *,
                      int *);

extern int read_trj_ (char *,
                      int *,
                      int *,
                      int *,
                      int *);

extern int read_vas_ (char *,
                      int *,
                      int *,
                      int *,
                      int *);

extern int getnspec_ (int *);

extern void getlabel_ (int *,
                       char *);

extern double fdmax_ (int *);
extern double fkmin_ (int *);
extern double oglmax_ ();

extern double random3_ (int *);

extern int smooth_and_save_ (double *,
                             double *,
                             double *,
                             int *,
                             int *,
                             int *);

extern double set_mass_ (int *);
extern double set_radius_ (int *,
                           int *);
extern double set_neutron_ (int *);

extern void profree_ ();
extern int chemistry_ ();

extern void prep_pos_ (int *,
                       int *);

extern void prep_spec_ (double *,
                        int *);

extern int prep_data_ ();

extern int bonds_ (int *,
                   int *,
                   int *);

extern int alloc_data_ (int *,
                        int *,
                        int *);

extern void read_pos_ (double *,
                       double *,
                       double *);

extern void read_data_ (int *,
                        int *);

extern void read_chem_ (double *,
                        double *,
                        double *,
                        double *);

extern int add_cells_ (int *,
                       int *,
                       int *);

extern int shift_box_center_ (int *,
                              int *,
                              double[3],
                              int *);

extern void lattice_ (int *,
                      int *,
                      double[3][3],
                      double[3],
                      double[3],
                      int *,
                      int *,
                      int *);

extern void sendcuts_ (int *,
                       int *,
                       double *);

extern int rundmtx_ (int *,
                     int *,
                     int *);

extern int bonding_ (int *,
                     int *,
                     int *,
                     int *,
                     double *,
                     double *,
                     char *);

extern int molecules_ (int *,
                       int *);

extern int bond_angles_ (int *);
extern int bond_diedrals_ (int *);
extern int dihedrals_for_mol_ ();

extern void label_spec_ (int *,
                         int *,
                         int *);

extern int initrings_ (int *,
                       int *,
                       int *,
                       int *,
                       int *,
                       int *);

extern int initchains_ (int *,
                        int *,
                        int *,
                        int *,
                        int *,
                        int *,
                        int *);

extern int g_of_r_ (int *,
                    double *,
                    int *);

extern int s_of_q_ (double *,
                    double *,
                    int *);

extern int cqvf_ (double *,
                  double *,
                  int *,
                  double *,
                  double *);

extern int s_of_k_ (int *,
                    int *);

extern int send_gr_ (int *,
                     int *,
                     double *,
                     double *,
                     double *);

extern int send_sq_ (int *,
                     int *,
                     double *,
                     double *,
                     double *);

extern int g_of_r_fft_ (int *,
                        double *,
                        double *);

extern int msd_ (double *,
                 int *);

extern int sphericals_ (int *,
                        int *,
                        int *,
                        int *,
                        int *);
#endif
