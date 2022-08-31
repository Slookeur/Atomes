/* This file is part of Atomes.

Atomes is free software: you can redistribute it and/or modify it under the terms
of the GNU Affero General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

Atomes is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with Atomes.
If not, see <https://www.gnu.org/licenses/> */

#include <libxml/encoding.h>
#include <libxml/xmlwriter.h>
#include <libxml/xmlreader.h>
#include <libxml/parser.h>

#include "global.h"
#include "interface.h"
#include "callbacks.h"

#define MY_ENCODING "UTF-8"

extern int open_coordinate_file (int id);

#define NFORMATS 7

char * reg_types[NFORMATS] = {"XYZ file",
                              "Chem3D file",
                              "CPMD trajectory",
                              "VASP trajectory",
                              "multiple XYZ file",
                              "multiple Chem3D file",
                              "PDB file"};

size_t strfind (char * ida)
{
  size_t a, b, c;

  a=strlen(ida);
  b = 0;
  for ( c=0 ; c < a ; c++)
  {
    if (ida[c] != ' ')
    {
      b++;
    }
  }
  return b;
}

int XmlwriterFilename (const char *uri)
{
  int rc;
  int i, j;
  xmlTextWriterPtr writer;

  char isaacinfo[16] = " I.S.A.A.C.S. v";
  char xmlinfo[11]=" XML file ";
  const xmlChar intro[50]="";
  char * val;
  char * ncut;
  size_t lgt;

  /* Create a new XmlWriter for uri, with no compression. */
  writer = xmlNewTextWriterFilename(uri, 0);
  if (writer == NULL) return 0;
  rc = xmlTextWriterSetIndent(writer, 1);
  if (rc < 0) return 0;
  /* Start the document with the xml default for the version,
   * encoding MY_ENCODING and the default for the standalone
   * declaration. */
  rc = xmlTextWriterStartDocument(writer, NULL, MY_ENCODING, NULL);
  if (rc < 0) return 0;


  strcpy((char *)intro, isaacinfo);
  strcat((char *)intro, VERSION);
  strcat((char *)intro, xmlinfo);
  rc = xmlTextWriterWriteComment(writer, intro);
  if (rc < 0) return 0;
  rc = xmlTextWriterStartElement(writer, BAD_CAST "isaacs-xml");
  if (rc < 0) return 0;

  /* Start the "data" element. Since thist is the first
   * element, this will be the root element of the document.
   <data>
	  <type> </type>
	  <file> <file>
   </data>*/

  rc = xmlTextWriterWriteComment(writer, (const xmlChar *)" Data format and file containing the configuration(s) ");
  if (rc < 0) return 0;
  rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"data");
  if (rc < 0) return 0;
  rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"type", "%s", reg_types[active_project -> tfile]);
  if (rc < 0) return 0;
  rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"file", "%s", active_project -> coordfile);
  if (rc < 0) return 0;
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;

  /* Start the "chemistry" element.
  <chemistry>
    <atoms> </atoms>
    <species number=" ">
      <active_project -> label id="0"> </active_project -> label>
      ...
    </species>
    <element symbol="">
	  <name> </name>
	  <z> </z>
	  <mass> </mass>
	  <rad> </rad>
	  <radius> </radius>
	  <nscatt> </nscatt>
	  <xscatt> </xscatt>
    </element>
  </chemistry> */

  rc = xmlTextWriterWriteComment(writer, (const xmlChar *)" Chemistry information ");
  if (rc < 0) return 0;
  rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"chemistry");
  if (rc < 0) return 0;
  rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"atoms", "%d", active_project -> natomes);
  if (rc < 0) return 0;
  rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"species");
  if (rc < 0) return 0;
  val=g_strdup_printf("%d",active_project -> nspec);
  rc = xmlTextWriterWriteAttribute(writer, BAD_CAST (const xmlChar *)"number", BAD_CAST val);
  g_free (val);
  if (rc < 0) return 0;
  for ( i=0 ; i<active_project -> nspec ; i++)
  {
    rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"label");
    if (rc < 0) return 0;
    val=g_strdup_printf("%d",i);
    rc = xmlTextWriterWriteAttribute(writer, BAD_CAST (const xmlChar *)"id", BAD_CAST val);
    g_free (val);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatString (writer, "%s", active_chem -> label[i]);
    if (rc < 0) return 0;
    rc = xmlTextWriterEndElement(writer);
    if (rc < 0) return 0;
  }
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;
  for ( i=0 ; i<active_project -> nspec ; i++)
  {
    rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"element");
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteAttribute(writer, BAD_CAST (const xmlChar *)"symbol", BAD_CAST active_chem -> label[i]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"name", "%s ", active_chem -> element[i]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"z", "%d", (int)active_chem -> chem_prop[CHEM_Z][i]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"mass", "%f", active_chem -> chem_prop[CHEM_M][i]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"rad", "%d", 0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"radius", "%f", active_chem -> chem_prop[CHEM_R][i]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"nscatt", "%f", active_chem -> chem_prop[CHEM_N][i]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"xscatt", "%f", active_chem -> chem_prop[CHEM_Z][i]);
    if (rc < 0) return 0;
    rc = xmlTextWriterEndElement(writer);
    if (rc < 0) return 0;
  }
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;

  /* Start the "box" element.
  <box>
    <edges>
      <a></a>
      <b></b>
      <c></c>
    </edges>
    <angles> </angles>
    <vectors>
      <a.x></a.x>
      <a.y></a.y>
      <a.z></a.z>
      <b.x></b.x>
      <b.y></b.y>
      <b.z></b.z>
      <c.x></c.x>
      <c.y></c.y>
      <c.z></c.z>
    </vectors>
  </box> */

  rc = xmlTextWriterWriteComment(writer, (const xmlChar *)" Box information ");
  if (rc < 0) return 0;
  rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"box");
  if (rc < 0) return 0;
  if (active_cell -> ltype == 1)
  {
    rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"edges");
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"a", "%f", active_box -> param[0][0]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"b", "%f", active_box -> param[0][1]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"c", "%f", active_box -> param[0][2]);
    if (rc < 0) return 0;
    rc = xmlTextWriterEndElement(writer);
    if (rc < 0) return 0;
    rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"angles");
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"alpha", "%f", active_box -> param[1][0]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"beta", "%f", active_box -> param[1][1]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"gamma", "%f", active_box -> param[1][2]);
    if (rc < 0) return 0;
    rc = xmlTextWriterEndElement(writer);
    if (rc < 0) return 0;
  }
  else
  {
    rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"edges");
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"a", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"b", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"c", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"angles");
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"alpha", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"beta", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"gamma", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterEndElement(writer);
    if (rc < 0) return 0;
  }
  rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"vectors");
  if (rc < 0) return 0;
  if (active_cell -> ltype == 2)
  {
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"a.x", "%f", active_box -> vect[0][0]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"a.y", "%f", active_box -> vect[0][1]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"a.z", "%f", active_box -> vect[0][2]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"b.x", "%f", active_box -> vect[1][0]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"b.y", "%f", active_box -> vect[1][1]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"b.z", "%f", active_box -> vect[1][2]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"c.x", "%f", active_box -> vect[2][0]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"c.y", "%f", active_box -> vect[2][1]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"c.z", "%f", active_box -> vect[2][2]);
    if (rc < 0) return 0;
  }
  else
  {
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"a.x", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"a.y", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"a.z", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"b.x", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"b.y", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"b.z", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"c.x", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"c.y", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"c.z", "%f", 0.0);
    if (rc < 0) return 0;
  }
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;

 /* Start the "pbc" element.
  <pbc>
    <apply></apply>
    <fractional></fractional>
    <fractype></fractype>
  </pbc>
  */

  rc = xmlTextWriterWriteComment(writer, (const xmlChar *)" PBC information ");
  if (rc < 0) return 0;
  rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"pbc");
  if (rc < 0) return 0;
  if (active_cell -> pbc)
  {
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"apply", "%s", "TRUE");
  }
  else
  {
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"apply", "%s", "FALSE");
  }
  if (rc < 0) return 0;
  if (active_cell -> frac)
  {
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"fractional", "%s", "TRUE");
  }
  else
  {
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"fractional", "%s", "FALSE");
  }
  if (rc < 0) return 0;
  rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"fractype", "%d", active_cell -> frac);
  if (rc < 0) return 0;
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;

 /* Start the "cutoffs" element.
  <cutoffs>
    <total></total>
    <partials>
      <active_chem -> label[spi]-active_chem -> label[spj]></active_chem -> label[spi]-active_chem -> label[spj]>
    </partials>
  </cutoffs>
  */

  rc = xmlTextWriterWriteComment(writer, (const xmlChar *)" Cutoffs information ");
  if (rc < 0) return 0;
  rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"cutoffs");
  if (rc < 0) return 0;
  rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"total", "%f", active_chem -> grtotcutoff);
  if (rc < 0) return 0;
  rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"partials");
  if (rc < 0) return 0;
  for ( i=0 ; i<active_project -> nspec ; i++ )
  {
    for ( j=0 ; j<active_project -> nspec ; j++ )
    {
      lgt = 10;
      lgt += strfind (active_chem -> label[i]) + strlen("-") + strfind(active_chem -> label[j]);
      ncut = g_malloc0 (lgt*sizeof*ncut);
      strncpy (ncut, active_chem -> label[i], strfind(active_chem -> label[i]));
      strcat (ncut, "-");
      strncat (ncut, active_chem -> label[j], strfind(active_chem -> label[j]));
      rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST ncut, "%f", active_chem -> cutoffs[i][j]);
      if (rc < 0) return 0;
      g_free (ncut);
      ncut=NULL;
    }
  }
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;

  /* Start the "time series" element.
  <time-series>
    <dt></dt>
    <unit></unit>
    <ndt></ndt>
  </time-series>
  */

  if (active_project -> steps > 1)
  {
    rc = xmlTextWriterWriteComment(writer, (const xmlChar *)" Time series ");
    if (rc < 0) return 0;
    rc = xmlTextWriterStartElement(writer, BAD_CAST (const xmlChar *)"time-series");
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"dt", "%f", 0.0);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"unit", "t [%s]", untime[active_project -> tunit]);
    if (rc < 0) return 0;
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"ndt", "%d", 1);
    if (rc < 0) return 0;
    rc = xmlTextWriterEndElement(writer);
    if (rc < 0) return 0;
  }

  /* Start the "project" element.
  <project></project>
  */

  rc = xmlTextWriterWriteComment(writer, (const xmlChar *)" Apply project ");
  if (rc < 0) return 0;
  if (active_project -> run)
  {
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"project", "%s", "TRUE");
  }
  else
  {
    rc = xmlTextWriterWriteFormatElement(writer, BAD_CAST (const xmlChar *)"project", "%s", "FALSE");
  }
  if (rc < 0) return 0;
// Closing "</isaacs-xml>"
  rc = xmlTextWriterEndElement(writer);
  if (rc < 0) return 0;

  rc = xmlTextWriterEndDocument(writer);
  if (rc < 0) return 0;

  xmlFreeTextWriter(writer);
  return 1;
}

int write_xml (const char * filetosave)
{
  /* first, the file version */
  int res=XmlwriterFilename(filetosave);
  /*
   * Cleanup function for the XML library.
   */
  xmlCleanupParser();
  /*
   * this is to debug memory for regression tests
   */
  xmlMemoryDump();
  return res;
}

gboolean file_exists(const char * filename)
{
  FILE * file;
  if ((file = fopen(filename, "r")))
  {
    fclose(file);
    return TRUE;
  }
  return FALSE;
}

xmlNodePtr findnode (xmlNodePtr startnode, char * nname)
{
  xmlNodePtr tmp;

  tmp = startnode;
  while (g_strcmp0 ((char *)(tmp -> name), nname) != 0)
  {
    if (tmp -> next == NULL)
    {
      return NULL;
    }
    tmp = tmp -> next;
  }
  return tmp;
}

int setprop (xmlNodePtr pnode)
{
  int i, j, res;
  xmlAttrPtr pspec;
  xmlNodePtr idn, ptnode, ptn;
  xmlChar * data;
  double val;

  ptnode=pnode;
  for ( i=0 ; i<active_project -> nspec ; i++)
  {
    ptnode = findnode(ptnode, "element");
    if (ptnode == NULL)
    {
      res=6;
      goto pend;
    }
    pspec = ptnode -> properties;
    idn = pspec -> children;
    data = xmlNodeGetContent(idn);
    if (g_strcmp0 ((char *)data, active_chem -> label[i]) != 0)
    {
      res=6;
      goto pend;
    }
    ptnode = ptnode -> children;
    ptn = findnode(ptnode, "name");
    if (ptn == NULL)
    {
      res=6;
      goto pend;
    }
    data = xmlNodeGetContent(ptn);
    if (g_strcmp0 ((char *)data, active_chem -> element[i]) != 0)
    {
      res=6;
      goto pend;
    }
    ptn = findnode(ptnode, "z");
    if (ptn == NULL)
    {
      res=6;
      goto pend;
    }
    data= xmlNodeGetContent(ptn);
    val = atof((char *)data);
    j = val;
    if ((int)active_chem -> chem_prop[CHEM_Z][i] != j)
    {
      res=6;
      goto pend;
    }
    ptn = findnode(ptnode, "mass");
    if (ptn == NULL)
    {
      res=6;
      goto pend;
    }
    data= xmlNodeGetContent(ptn);
    val = atof((char *)data);
    ptn = findnode(ptnode, "rad");
    if (ptn == NULL)
    {
      res=6;
      goto pend;
    }
    data= xmlNodeGetContent(ptn);
    val = atof((char *)data);
    ptn = findnode(ptnode, "radius");
    if (ptn == NULL)
    {
      res=6;
      goto pend;
    }
    data= xmlNodeGetContent(ptn);
    val = atof((char *)data);
    ptn = findnode(ptnode, "nscatt");
    if (ptn == NULL)
    {
      res=6;
      goto pend;
    }
    data= xmlNodeGetContent(ptn);
    val = atof((char *)data);
    ptn = findnode(ptnode, "xscatt");
    if (ptn == NULL)
    {
      res=6;
      goto pend;
    }
    data= xmlNodeGetContent(ptn);
    val = atof((char *)data);
    ptnode = ptnode -> parent;
    ptnode = ptnode -> next;
  }
  res = 0;
pend:
  return res;
}

int testopening (char * tdata, char * tfichier)
{
  int i, j;
  char * err;

  j=-2;
  for ( i=0 ; i<NFORMATS ; i ++ )
  {
    if (g_strcmp0 (reg_types[i], tdata) == 0) j=i;
  }
  if (j == 4) j=0;
  if (j == 5) j=2;
  if (j == 6) j=4;
  if (j != -2)
  {
    if (file_exists (tfichier))
    {
      active_project -> tfile = NCFORMATS;
      active_project -> coordfile = tfichier;
      if (open_coordinate_file (j) == 0)
      {
        return 0;
      }
      else
      {
        return 11;
      }
    }
    else
    {
      j=4;
      err=g_strdup_printf("The %s\n %s\n from the XML file does not exist\n", tdata, tfichier);
      show_error (err, 0, MainWindow);
      g_free (err);
      return j;
    }
  }
  else
  {
    show_error ("Unknown data format in XML file\n", 0, MainWindow);
    j=4;
    return j;
  }
}

int setchemistry (xmlNodePtr xsnode)
{
  xmlNodePtr idn, cs, xnode;
  xmlAttrPtr xspec;
  xmlChar * data;
  int ats, res, i;
  double val;

  xnode = xsnode -> children;
  cs = findnode(xnode, "atoms");
  if (cs == NULL)
  {
    res=5;
    goto xend;
  }
  data = xmlNodeGetContent(cs);
  val = atof((char *)data);
  ats = val;
  if (ats != active_project -> natomes)
  {
    show_warning ("The number of atoms in the XML file\n"
                  "is not the same that the number of atoms\n"
                  "in the file containing the coordinates.\n"
                  "Other information from the XML file\n"
                  "will be ignored\n", MainWindow);
    res=5;
    goto xend;
  }
  else
  {
    cs = findnode(xnode, "species");
    if (cs == NULL)
    {
      res=5;
      goto xend;
    }
    xspec = cs -> properties;
    idn = xspec -> children;
    data = xmlNodeGetContent(idn);
    val = atof((char *)data);
    ats = val;
    if (ats != active_project -> nspec)
    {
      show_warning ("The number of chemical species in the XML file\n"
                    "is not the same that the number of chemical species\n"
                    "in the file that contains the atomic coordinates.\n", MainWindow);
      res = 5;
      goto xend;
    }
    else
    {
      cs = cs -> children;
      for ( i=0 ; i<active_project -> nspec ; i++)
      {
        cs = findnode(cs, "label");
        if (cs == NULL)
        {
          res=5;
          goto xend;
        }
        data = xmlNodeGetContent(cs);
        if (g_strcmp0 ((char *)data, active_chem -> label[i]) != 0)
        {
          res=5;
          goto xend;
        }
        xspec = cs -> properties;
        idn = xspec -> children;
        data = xmlNodeGetContent(idn);
        val = atof((char *)data);
        ats = val;
        if (ats != i)
        {
          res=5;
          goto xend;
        }
        cs = cs -> next;
      }
      xnode = xsnode -> children;
      res = setprop (xnode);
    }
  }

xend:
  return res;
}

int setbox (xmlNodePtr boxnode)
{
  int box;
  xmlNodePtr bnode, ba, bb;
  xmlChar * data;
  double val;

  bnode = boxnode -> children;
  ba=findnode(bnode, "edges");
  if (ba == NULL)
  {
    box=7;
    goto bend;
  }
  ba = ba -> children;
  bb=findnode(ba, "a");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> param[0][0]= val;
  bb=findnode(ba, "b");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> param[0][1]= val;
  bb=findnode(ba, "c");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> param[0][2]= val;
  ba=findnode(bnode, "angles");
  if (ba == NULL)
  {
    box=7;
    goto bend;
  }
  ba = ba -> children;
  bb=findnode(ba, "alpha");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> param[1][0] = val;
  bb=findnode(ba, "beta");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> param[1][1]= val;
  bb=findnode(ba, "gamma");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  if (active_box -> param[0][0] != 0.0 && active_box -> param[0][1] != 0.0 && active_box -> param[0][2] != 0.0
      && active_box -> param[1][0] != 0.0 && active_box -> param[1][1] != 0.0 && active_box -> param[1][2] != 0.0)
  {
    active_cell -> ltype = 1;
  }
  else
  {
    active_cell -> ltype = 0;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> param[1][2]= val;
  ba=findnode(bnode, "vectors");
  if (ba == NULL)
  {
    box=7;
    goto bend;
  }
  ba = ba -> children;
  bb=findnode(ba, "a.x");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[0][0]= val;
  bb=findnode(ba, "a.y");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[0][1]= val;
  bb=findnode(ba, "a.z");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[0][2]= val;
  bb=findnode(ba, "b.x");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[1][0]= val;
  bb=findnode(ba, "b.y");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[1][1]= val;
  bb=findnode(ba, "b.z");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[1][2]= val;
  bb=findnode(ba, "c.x");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[2][0]= val;
  bb=findnode(ba, "c.y");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[2][1]= val;
  bb=findnode(ba, "c.z");
  if (bb == NULL)
  {
    box=7;
    goto bend;
  }
  data =  xmlNodeGetContent(bb);
  val = atof((char *)data);
  active_box -> vect[2][2]= val;

  if (active_box -> vect[0][0] != 0.0 && active_box -> vect[0][1] != 0.0 && active_box -> vect[0][2] != 0.0
      && active_box -> vect[1][0] != 0.0 && active_box -> vect[1][1] != 0.0 && active_box -> vect[1][2] != 0.0
      && active_box -> vect[2][0] != 0.0 && active_box -> vect[2][1] != 0.0 && active_box -> vect[2][2] != 0.0)
  {
    active_cell -> ltype = 2;
  }
  box = 0;

bend:
  return box;
}

int setpbc (xmlNodePtr pbcnode)
{
  int pbc;//, j;
  xmlNodePtr pnode, bnode;
  xmlChar * data;
  //double val;

  pnode = pbcnode -> children;
  bnode=findnode(pnode, "apply");
  if (bnode == NULL)
  {
    pbc=8;
    goto pend;
  }
  data = xmlNodeGetContent(bnode);
  active_cell -> pbc = 0;
  if (g_strcmp0 ((char *)data, (char *)"TRUE") == 0)
  {
    active_cell -> pbc = 1;
  }
  bnode=findnode(pnode, "fractional");
  if (bnode == NULL)
  {
    pbc=8;
    goto pend;
  }
  data = xmlNodeGetContent(bnode);
  active_cell -> frac = 0;
  if (g_strcmp0 ((char *)data, (char *)"TRUE") == 0)
  {
    bnode=findnode(pnode, "fractype");
    if (bnode == NULL)
    {
      pbc=8;
      goto pend;
    }
    data = xmlNodeGetContent(bnode);
    //val=atof((char *)data);
    //j = val;
    active_cell -> frac = 1;
  }
  pbc=0;

pend:
  return pbc;
}

int setcutoffs (xmlNodePtr cutnode)
{
  int cut;
  int i, j;
  xmlNodePtr cnode, cn;
  xmlChar * data;
  double val;
  char * ncut;
  size_t lgt;

  cnode = cutnode -> children;
  cn = findnode(cnode, "total");
  if (cn == NULL)
  {
    cut=9;
    goto cend;
  }
  data = xmlNodeGetContent(cn);
  val=atof((char *)data);
  active_chem -> grtotcutoff = val;
  cn = findnode(cnode, "partials");
  if (cn == NULL)
  {
    cut=9;
    goto cend;
  }
  cnode = cn -> children;
  for ( i=0 ; i<active_project -> nspec ; i++ )
  {
    for ( j=0 ; j<active_project -> nspec ; j++ )
    {
      lgt=10;
      lgt+=strfind(active_chem -> label[i]) + strlen("-") + strfind(active_chem -> label[j]);
      ncut = g_malloc0 (lgt*sizeof*ncut);
      strncpy (ncut, active_chem -> label[i], strfind(active_chem -> label[i]));
      strcat (ncut, "-");
      strncat (ncut, active_chem -> label[j], strfind(active_chem -> label[j]));
      cn = findnode(cnode, ncut);
      if (cn == NULL)
      {
        cut=9;
        goto cend;
      }
      data = xmlNodeGetContent(cn);
      val=atof((char *)data);
      active_chem -> cutoffs[i][j] = val;
      g_free (ncut);
      ncut=NULL;
    }
  }
  cut=0;

cend:
  return cut;
}

int settime(xmlNodePtr timenode)
{
  int i, j;
  //double val, delta_t;
  //int tunit, ndtbs;
  int tps;
  xmlChar * data;
  xmlNodePtr tnode, tn;

  tnode = timenode -> children;
  tn = findnode(tnode, "dt");
  if (tn == NULL)
  {
    tps=10;
    goto tend;
  }
  data = xmlNodeGetContent(tn);
  //val = atof((char *)data);
  //delta_t = val;
  tn = findnode(tnode, "unit");
  if (tn == NULL)
  {
    tps=10;
    goto tend;
  }
  data = xmlNodeGetContent(tn);
  j=-1;
  for ( i=0 ; i<6 ; i++)
  {
    if (g_strcmp0 ((char *)data, g_strdup_printf ("t [%s]", untime[i])) == 0) j=i;
  }
  if (j == -1)
  {
    tps=10;
    goto tend;
  }
  if (j < 5)
  {
    //tunit = j+1;
  }
  tn = findnode(tnode, "ndt");
  if (tn != NULL)
  {
    data = xmlNodeGetContent(tn);
    //val = atof((char *)data);
    //ndtbs = val;
  }
  tps=0;

tend:
  return tps;
}

int check_xml (const char * filetocheck)
{
  int res;
  xmlDoc * doc;
  xmlTextReaderPtr reader;
  const xmlChar xisaacs[11]="isaacs-xml";
  xmlChar * cdata, * cfichier;
  xmlNodePtr racine, node;
  /*
   * build an xmlReader for that file
   */
  reader = xmlReaderForFile(filetocheck, NULL, 0);
  if (reader != NULL)
  {
    res = 0;
    doc = xmlParseFile(filetocheck);
    if (doc == NULL)
    {
      res=1;
      goto end;
    }
    else
    {
      racine = xmlDocGetRootElement(doc);
      if (g_strcmp0 ((char *)(racine->name), (char *)xisaacs) != 0)
      {
        res=2;
        goto end;
      }
      else
      {
        node = racine -> children;
        node = findnode(node, "data");
        if (node == NULL)
        {
          res=3;
          goto end;
        }
        node = node -> children;
        node = findnode(node, "type");
        if (node == NULL)
        {
          res=3;
          goto end;
        }
        cdata = xmlNodeGetContent(node);
        node = findnode(node, "file");
        if (node == NULL)
        {
          res=3;
          goto end;
        }
        cfichier = xmlNodeGetContent(node);
        res= testopening((char *)cdata, (char *)cfichier);
        if (res != 0)
        {
          goto end;
        }
        else
        {
          node = racine -> children;
          node = findnode(node, "chemistry");
          if (node == NULL)
          {
            res=3;
            goto end;
          }
          res=setchemistry(node);
          if (res != 0)
          {
            goto end;
          }
          node = racine -> children;
          node = findnode(node, "pbc");
          if (node == NULL)
          {
            res=3;
            goto end;
          }
          res=setpbc(node);
          if (res != 0)
          {
             goto end;
          }
          node = racine -> children;
          node = findnode(node, "box");
          if (node == NULL)
          {
            res=3;
            goto end;
          }
          active_cell -> box = g_malloc0(sizeof*active_cell -> box);
          active_box = & active_cell -> box[0];
          res=setbox(node);
          if (res != 0)
          {
             goto end;
          }
          node = racine -> children;
          node = findnode(node, "cutoffs");
          if (node == NULL)
          {
            res=3;
            goto end;
          }
          active_chem -> cutoffs = allocddouble (active_project -> nspec, active_project -> nspec);
          res=setcutoffs (node);
          if (res != 0)
          {
             goto end;
          }
          if (active_project -> steps > 1)
          {
            node = racine -> children;
            node = findnode(node, "time-series");
            if (node == NULL)
            {
              res=3;
              goto end;
            }
            res=settime (node);
            if (res != 0)
            {
               goto end;
            }
          }
          node = racine -> children;
          node = findnode(node, "project");
          if (node == NULL)
          {
            res=3;
            goto end;
          }
          cdata=xmlNodeGetContent(node);
          if (g_strcmp0 ((char *)cdata, (char *)"TRUE") == 0)
          {
            active_project -> run = 1;
          }
          else
          {
            active_project -> run = 0;
          }
        }
      }
    }
  }
  else
  {
    res=1;
  }

end:
  /*
   * Cleanup function for the XML library.
   */
  if (reader != NULL) xmlFreeTextReader(reader);
  /*
   *Free the global variables that may
   *have been allocated by the parser.
   */
  xmlCleanupParser();

  return res;
}

gchar * open_xml (const char * filetoread)
{
  int oxml;
  oxml = check_xml(filetoread);

  switch(oxml)
  {
    case 1:
      return ("Impossible to open the Isaacs project file (XML) file\n");
      break;
    case 2:
      return ("The file is not a valid Isaacs project file (XML) file\n");
      break;
    case 3:
      return ("The Isaacs project file (XML) file is incomplete\n");
      break;
    case 4:
      return ("Problem(s) in the &lt;data&gt; section of the Isaacs project file (XML)\n");
      break;
    case 5:
      return ("Problem(s) in the &lt;chemistry&gt; section of the Isaacs project file (XML)\n");
      break;
    case 6:
      return ("Problem(s) in the &lt;element&gt; section of the Isaacs project file (XML)\n");
      break;
    case 7:
      return ("Problem(s) in the &lt;box&gt; section of the Isaacs project file (XML)\n");
      break;
    case 8:
      return ("Problem(s) in the &lt;pbc&gt; section of the Isaacs project file (XML)\n");
      break;
    case 9:
      return ("Problem(s) in the &lt;cutoffs&gt; section of the Isaacs project file (XML)\n");
      break;
    case 10:
      return ("Problem(s) in the &lt;time-series&gt; section of the Isaacs project file (XML)\n");
      break;
    default:
      return NULL;
      break;
  }
}
