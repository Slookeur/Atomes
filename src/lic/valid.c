/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

// Linux or OSX
#ifdef LINUX
#  include <sys/ioctl.h>
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <ifaddrs.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <pwd.h>

// Linux only
#  ifndef __APPLE__
#    include <linux/if.h>
#  endif

#endif

#ifdef AF_LINK
#  include <net/if_dl.h>
#endif

#ifdef AF_PACKET
#  include <netpacket/packet.h>
#endif

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#ifdef G_OS_WIN32
#  include <winsock2.h>
#endif
#include "global.h"
#include "workspace.h"
#include "regdata.h"
#include "interface.h"
#ifdef G_OS_WIN32
#  include <iphlpapi.h>
#endif

int key[4];
int tm[2]={17, 23};
int keya, keyb;
gchar * mad = NULL;
unsigned char * mac;
gboolean firstrun=TRUE;

GtkWidget * ok;
GtkWidget * entrya;

int mac_addr_sys ()
{
  size_t sizead;
  gchar * macthe = NULL;
  int i;
  unsigned char ** macs;
  int num_mac;

#ifdef G_OS_WIN32

/* implementation for Win32 > Win2000 */

  DWORD dwSize = 0;
  DWORD dwRetVal = 0;

  MIB_IFTABLE * pIfTable;
  MIB_IFROW * pIfRow;

  dwSize = sizeof (MIB_IFTABLE);
  pIfTable = (MIB_IFTABLE *) malloc (dwSize);
  if (pIfTable != NULL)
  {
    if (GetIfTable(pIfTable, &dwSize, FALSE) == ERROR_INSUFFICIENT_BUFFER)
    {
      free (pIfTable);
      pIfTable = (MIB_IFTABLE *) malloc (dwSize);
    }
    if ((dwRetVal = GetIfTable(pIfTable, &dwSize, FALSE)) == NO_ERROR)
    {
      if (pIfTable -> dwNumEntries > 0)
      {
        num_mac = pIfTable -> dwNumEntries;
        pIfRow = (MIB_IFROW *) & pIfTable -> table[0];
        macs = malloc (num_mac*sizeof*macs);
        for (i = 0; i < num_mac; i++)
        {
          pIfRow = (MIB_IFROW *) & pIfTable -> table[i];
          sizead = sizeof (pIfRow -> bPhysAddr);
          macs[i] = malloc (sizead * sizeof*macs[i]);
          memcpy (macs[i], pIfRow -> bPhysAddr, sizead);
        }
      }
    }
    free (pIfTable);
    pIfTable = NULL;
  }
  else
  {
    return 0;
  }

#endif

#ifdef LINUX

#  ifndef __APPLE__

/* implementation for Linux using ioctl   */
/* Only allows acces to active interfaces */
/*
  struct ifreq * ifr, * item;
  struct ifconf ifc;
  char buf[1024];
  int s;

  s = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
  if (s < 0)
  {
    return 0;
  }
  else
  {
    ifc.ifc_len = sizeof(buf);
    ifc.ifc_buf = buf;
    if (ioctl (s, SIOCGIFCONF, & ifc) < 0)
    {
      return 0;
    }
    ifr = ifc.ifc_req;
    num_mac = ifc.ifc_len / sizeof(struct ifreq);
    macs = malloc (num_mac*sizeof*macs);
#ifdef DEBUG
    g_debug ("NUM OF INTERFACE: i= %d", num_mac);
#endif
    for (i = 0; i < num_mac; i ++)
    {
      item = & ifr[i];
#ifdef DEBUG
      g_debug ("DEBUG:MAC_ADRESS: i= %d, name= %s", i, item -> ifr_name);
#endif
      if (ioctl(s, SIOCGIFHWADDR, item) == 0)
      {
        sizead = sizeof (item -> ifr_hwaddr.sa_data);
        macs[i] = malloc (sizead*sizeof*macs[i]);
        memcpy (macs[i], item -> ifr_hwaddr.sa_data, sizead);
      }
    }
    close(s);
  }
*/

/* implementation for Linux using AF_PACKET */
/* Allows acces to all interfaces           */

  struct ifaddrs * ifad, * ifad_next;
  struct sockaddr_ll * sdl;
  if (getifaddrs (& ifad) == -1)
  {
    return 0;
  }
  num_mac = 0;
  ifad_next = ifad -> ifa_next;
  while (ifad_next != NULL)
  {
    if (ifad -> ifa_addr -> sa_family == AF_PACKET)
    {
      num_mac ++;
    }
    ifad_next = ifad_next -> ifa_next;
  }
  macs = malloc (num_mac*sizeof*macs);
  ifad_next = ifad -> ifa_next;
  i = -1;
  while (ifad_next != NULL)
  {
    if (ifad -> ifa_addr -> sa_family == AF_PACKET)
    {
      i ++;
      sdl = (struct sockaddr_ll *) ifad_next -> ifa_addr;
      sizead = sdl-> sll_halen;
      macs[i] = malloc (sizead*sizeof*macs[i]);
	  memcpy (macs[i], sdl -> sll_addr, sizead);
    }
    ifad_next = ifad_next -> ifa_next;
  }
  freeifaddrs (ifad);

#  else

/* implementation for MacOSX using AF_LINK */
/* Allows acces to all interfaces          */
  struct ifaddrs * ifad, * ifad_next;
  struct sockaddr_dl * sdl;

  if (getifaddrs (& ifad) == -1)
  {
    return 0;
  }
  num_mac = 0;
  ifad_next = ifad -> ifa_next;
  while (ifad_next != NULL)
  {
    if (ifad -> ifa_addr -> sa_family == AF_LINK)
    {
      num_mac ++;
    }
    ifad_next = ifad_next -> ifa_next;
  }
  macs = malloc (num_mac*sizeof*macs);
  ifad_next = ifad -> ifa_next;
  i = -1;
  while (ifad_next != NULL)
  {
    if (ifad -> ifa_addr -> sa_family == AF_LINK)
    {
      i ++;
      sdl = (struct sockaddr_dl *) ifad_next -> ifa_addr;
      sizead = sdl -> sdl_alen;
      macs[i] = malloc (sizead*sizeof*macs[i]);
	  memcpy (macs[i], sdl-> sdl_data + sdl-> sdl_nlen, sizead);
    }
    ifad_next = ifad_next -> ifa_next;
  }
  freeifaddrs (ifad);

#  endif

#endif

  for (i = 0; i < num_mac; i++)
  {
    macthe = g_strdup_printf ("%.2x-%.2x-%.2x-%.2x-%.2x-%.2x",
                              macs[i][0], macs[i][1], macs[i][2],
                              macs[i][3], macs[i][4], macs[i][5]);
#ifdef DEBUG
    g_debug ("MAC_ADRESS: macthe= %s", macthe);
#endif
    if (strcmp((char *)macthe, (char *)THEMAC) == 0)
    {
      g_free (macthe);
      mac = (unsigned char *) g_strdup_printf ("%s", macs[i]);
      free (macs);
      return 1;
    }
    else
    {
      g_free (macthe);
    }
  }
  free (macs);
  return 2;
}

void runvalidnum()
{
  int i, j, k, l;
  const gchar * userid;;

  i=0;
  for (j=0; j<4; j++)
  {
    if (key[j] == 0) i=1;
  }
  if (i == 0)
  {
    i = key[1]/1740.0;
    i = key[1] - i*1740 + 56890;
//    g_print ("i= %d\n",i);
    j = (key[2]-23657)*9;
    j = j / 8354;
    j = (key[2]-23657)*9 - j*8354 + 17503;
//    g_print ("j= %d\n",j);
    k = key[0]/2376.0;
    k = key[0] - k*2376 + 5697;
//    g_print ("k= %d\n",k);
    l = key[3];

    if (l == i+j+k)
    {
      userid = entry_get_text (GTK_ENTRY(entrya));
      if (strcmp ((char *)userid, (char *)REGUSER) == 0)
      {
        widget_set_sensitive (ok, 1);
      }
    }
  }
}

G_MODULE_EXPORT void set_key (GtkEntry * entry, gpointer data)
{
  if (strcmp ((char *)entry_get_text (entry), (char *)"") != 0)
  {
    key[GPOINTER_TO_INT(data)]=atof(entry_get_text (entry));
    runvalidnum();
  }
}

void compkeys (gchar * mot)
{
  double id;
  int j;
#ifdef DEBUG
  g_debug ("COMPKEYS: mad= %s, mot= %s", mad, mot);
#endif
  id = atof(mot);
#ifdef DEBUG
  g_debug ("COMPKEYS: mot= %s, id= %lf", mot, id);
#endif
  for (j=0; j<4; j++)
  {
#ifdef DEBUG
    g_debug ("COMPKEYS: i= %d, key[i]= %d", j, key[j]);
#endif
    id = id * key[j];
  }
#ifdef DEBUG
  g_debug ("COMPKEYS: id= %lf", id);
#endif
  id = sqrt(id);
  id = sqrt(id);
#ifdef DEBUG
  g_debug ("COMPKEYS: id= %lf", id);
#endif
  keya = id;
  keyb = 100000000*(id - keya);
#ifdef DEBUG
  g_debug ("COMPKEYS: keya= %d, keyb= %d", keya, keyb);
#endif
}

int valid ()
{
  double vd;
  int ka, kb;
  int i, j, k, l,  m, n, o, p, q;
  FILE *fp;
  gchar * val;

  fp=fopen(ATOMES_CONFIG, "r");
#ifdef DEBUG
  g_debug ("VALID: Opening Atomes config file");
#endif
  if (fp == NULL)
  {
    return 0;
  }
  else
  {
    fscanf(fp, "%015d%015d%010d%010d%010d%010d%01d%010d%010d%010d%010d",
           & kb, & ka, & i, & j, & k, & l, & m, & n, & o, & p, & q);
#ifdef DEBUG
    g_debug ("VALID: keya= %d, keyb= %d", ka, kb);
#endif
    if (m == 0)
    {
      firstrun = FALSE;
    }
    fclose(fp);
    val = g_strdup_printf ("%d.%d", ka, kb);
    vd = atof(val);
    g_free(val);
#ifdef DEBUG
    g_debug ("VALID: vd= %lf", vd);
#endif
    vd = vd * vd;
    vd = vd * vd;
#ifdef DEBUG
    g_debug ("VALID: i= %d, n= %d", i, n);
#endif
    key[3]=i+n*13;
#ifdef DEBUG
    g_debug ("VALID: key[3]= %d", key[3]);
    g_debug ("VALID: j= %d, o= %d", j, o);
#endif
    key[2]=j+o*18;
#ifdef DEBUG
    g_debug ("VALID: key[2]= %d", key[2]);
    g_debug ("VALID: k= %d, p= %d", k, p);
#endif
    key[0]=k+p*7;
#ifdef DEBUG
    g_debug ("VALID: key[0]= %d", key[0]);
    g_debug ("VALID: l= %d, q= %d", l, q);
#endif
    key[1]=l+q*3;
#ifdef DEBUG
    g_debug ("VALID: key[1]= %d", key[1]);
    g_debug ("VALID: vd= %lf", vd);
#endif
    for (i=0; i<4; i++)
    {
#ifdef DEBUG
      g_debug ("VALID: i= %d, key[i]= %d", i, key[i]);
#endif
      vd = vd / key[i];
    }
    vd = round (vd);
#ifdef DEBUG
    g_debug ("VALID: vd= %lf, atof(mad)= %lf", vd, atof(mad));
#endif
    if (vd == atof(mad))
    {
#ifdef DEBUG
      g_debug ("VALID: Atomes config is good");
#endif
      return 1;
    }
    else
    {
#ifdef DEBUG
      g_debug ("VALID: Atomes config is not good");
#endif
      return 0;
    }
  }
}

void record (gchar * vmad)
{
  FILE *fp;
  int i, j, k, l, m, n, o, p, q;

  compkeys (vmad);
  fp=fopen(ATOMES_CONFIG, "w");
  n = key[3]/13;
  i = key[3] - n*13;
  o = key[2]/18;
  j = key[2] - o*18;
  p = key[0]/7;
  k = key[0] - p*7;
  q = key[1]/3;
  l = key[1] - q*3;
  m=0;
#ifdef DEBUG
  g_debug ("RECORD: i= %010d, n=%010d", i, n);
  g_debug ("RECORD: j= %010d, o=%010d", j, o);
  g_debug ("RECORD: k= %010d, p=%010d", k, p);
  g_debug ("RECORD: l= %010d, q=%010d", l, q);
#endif
  fprintf (fp, "%015d%015d%010d%010d%010d%010d%01d%010d%010d%010d%010d",
           keyb, keya, i, j, k, l, m, n, o, p, q);
  fclose(fp);
}

int valimac ()
{
  int i;
  char * wrongad = "You need to purchase a valid Atomes license\n"
                   "if you want to run Atomes on this computer.";
  char * notacces = "It is impossible to access hardware information\n"
                    "Please let us know if this is a bug.";

  i = mac_addr_sys();
  if (i == 0)
  {
    if (mad != NULL) g_free (mad);
    mad = NULL;
    show_error (notacces, 1, MainWindow);
  }
  else if (i == 1)
  {
    if (mad != NULL) g_free (mad);
    mad = g_strdup_printf ("%o%o%o%o%o%o",
                           mac[0], mac[1], mac[2],
                           mac[3], mac[4], mac[5]);
    free (mac);
  }
  else if (i == 2)
  {
    if (mad != NULL) g_free (mad);
    mad = NULL;
    show_error (wrongad, 1, MainWindow);
  }
  return i;
}

gboolean validated;

G_MODULE_EXPORT void run_validask (GtkDialog * dial, gint response_id, gpointer data)
{
  char * thanks = "<b>Thank you for supporting the Atomes program.</b>\n"
                  "\n"
                  " Check the about dialog box for license information.\n"
                  "\n"
                  "  * Do you need support ?\n"
                  "  * Do you want to report a bug ?";
  char * hmmm = "<b>Something is wrong</b>\n"
                "\n"
                "You happen to have a valid license number\n"
                "however your hardware was not properly register.\n"
                "Please get in touch with the Atomes program developer.\n"
                "\n"
                "  * Do you need support ?\n"
                "  * Do you want to report a bug ?";
  validated = FALSE;
  if (response_id == GTK_RESPONSE_APPLY)
  {
    destroy_this_dialog (dial);
    if (valimac() == 1)
    {
      record (mad);
      g_free (mad);
      show_info (thanks, 1, MainWindow);
      gtk_widget_hide (register_button);
      widget_set_sensitive (MainView, 1);
      correct_this_window_title (MainWindow, (active_project) ? g_strdup_printf ("%s - %s", PACKAGE, prepare_for_title (active_project -> name)) : g_strdup_printf ("%s", PACKAGE));
      validated = TRUE;
    }
    else
    {
      show_info (hmmm, 1, MainWindow);
    }
  }
  else if (response_id == GTK_RESPONSE_CLOSE)
  {
    destroy_this_dialog (dial);
  }
}

G_MODULE_EXPORT gboolean validask ()
{
  GtkWidget *iask, *atombut;
  GtkWidget *dialog_ask;
  GtkWidget *hboxa, *hboxb, *hboxc;
  GtkWidget *entryb[4];
  GtkWidget *questa, *questb;
  const gchar * weburl = ATOMES_URL;
  int i;

  iask = dialogmodal ("Software Registration", GTK_WINDOW(MainWindow));
  dialog_ask = dialog_get_content_area (iask);
  // gtk_box_set_spacing (GTK_BOX(dialog_ask), 15);

  hboxa = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, dialog_ask, hboxa, TRUE, TRUE, 0);
  questa = markup_label("<b>User name:</b>", 200, -1, 0.0, 0.5);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, questa, FALSE, FALSE, 5);
  entrya = gtk_entry_new ();
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxa, entrya, FALSE, FALSE, 1);
  gtk_widget_set_size_request (entrya, 225, -1);
  gtk_entry_set_alignment (GTK_ENTRY(entrya), 0.0);
  gtk_widget_show(entrya);

  hboxb = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_VERTICAL, dialog_ask, hboxb, TRUE, TRUE, 5);
  gtk_widget_show(hboxb);
  questb = markup_label("<b>License number:</b>", 200, -1, 0.0, 0.5);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxb, questb, FALSE, FALSE, 5);
  hboxc = create_hbox (0);
  add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxb, hboxc, FALSE, FALSE, 0);
  for (i=0; i<4; i++)
  {
    key[i] = 0.0;
    entryb[i] = create_entry(G_CALLBACK(set_key), 55, 5, TRUE, GINT_TO_POINTER(i));
    add_box_child_start (GTK_ORIENTATION_HORIZONTAL, hboxc, entryb[i], TRUE, TRUE, 1);
    // gtk_widget_set_size_request (entryb[i], 50, -1);
    gtk_entry_set_max_length (GTK_ENTRY(entryb[i]), 5);
    gtk_entry_set_alignment (GTK_ENTRY(entryb[i]), 0.0);
  }

  ok =  create_button ("Ok", IMG_STOCK, YES, -1, -1, GTK_RELIEF_NORMAL, NULL, NULL);
  gtk_dialog_add_action_widget (GTK_DIALOG (iask), ok, GTK_RESPONSE_APPLY);
  widget_set_sensitive(ok, 0);
  atombut = gtk_link_button_new_with_label (weburl, "Visit Atomes website");
  gtk_dialog_add_action_widget (GTK_DIALOG (iask), atombut, GTK_RESPONSE_NO);

  run_this_gtk_dialog (iask, G_CALLBACK(run_validask), NULL);
  return validated;
}

gboolean validate ()
{
  if (valimac () == 1)
  {
    if (valid ())
    {
      gtk_widget_hide (register_button);
      return TRUE;
    }
    else if (firstrun)
    {
      return validask ();
    }
    else
    {
      g_free(mad);
      return FALSE;
    }
  }
  else
  {
    return FALSE;
  }
}

gboolean saving_option ()
{
  if (! registered_atomes)
  {
    show_warning ("Saving features are only available in the registered version of Atomes", MainWindow);
    registered_atomes = validate ();
    return registered_atomes;
  }
  else if (testing_atomes)
  {
    show_warning ("Saving features are disabled in this testing version of Atomes", MainWindow);
    return FALSE;
  }
  else
  {
    return TRUE;
  }
}
