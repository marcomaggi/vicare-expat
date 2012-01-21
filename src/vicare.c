/*
  Part of: Vicare/Expat
  Contents: Vicare backend for Expat
  Date: Sat Jan 21, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <expat.h>

#define EX_PARSER(PARSER)	IK_POINTER_DATA_VOIDP(PARSER)
#define EX_CALLBACK(CALLBACK)	IK_POINTER_DATA_VOIDP(CALLBACK)


/** --------------------------------------------------------------------
 ** Callbacks.
 ** ----------------------------------------------------------------- */

ikptr
ik_expat_set_element_decl_handler (ikptr s_parser, ikptr s_callback)
{
  XML_SetElementDeclHandler(EX_PARSER(s_parser), EX_CALLBACK(s_callback));
  return void_object;
}
ikptr
ik_expat_set_attlist_decl_handler (ikptr s_parser, ikptr s_callback)
{
  XML_SetAttlistDeclHandler(EX_PARSER(s_parser), EX_CALLBACK(s_callback));
  return void_object;
}
ikptr
ik_expat_set_xml_decl_handler (ikptr s_parser, ikptr s_callback)
{
  XML_SetXmlDeclHandler(EX_PARSER(s_parser), EX_CALLBACK(s_callback));
  return void_object;
}


/** --------------------------------------------------------------------
 ** Parser.
 ** ----------------------------------------------------------------- */

ikptr
ik_expat_parser_create (ikptr s_encoding, ikpcb * pcb)
{
  const XML_Char *	encoding = IK_BYTEVECTOR_DATA_VOIDP(s_encoding);
  XML_Parser		parser;
  parser = XML_ParserCreate(encoding);
  return ika_pointer_alloc(pcb, (ik_ulong)parser);
}
ikptr
ik_expat_parser_create_ns (ikptr s_encoding, ikptr s_namespace_separator, ikpcb * pcb)
{
  const XML_Char *	encoding  = IK_BYTEVECTOR_DATA_VOIDP(s_encoding);
  XML_Char		separator = (XML_Char)ik_integer_to_int(s_namespace_separator);
  XML_Parser		parser;
  parser = XML_ParserCreateNS(encoding, separator);
  return ika_pointer_alloc(pcb, (ik_ulong)parser);
}
#if 0 /* Excluded because, so far, I have  no idea about how to create a
	 meaningful  memory  functions  suite.   (Marco Maggi;  Jan  21,
	 2012) */
ikptr
ik_expat_parser_create_mm (ikptr s_encoding, ikptr s_namespace_separator, ikpcb * pcb)
{
  const XML_Char *	encoding = IK_BYTEVECTOR_DATA_VOIDP(s_encoding);
  XML_Char		separator = (XML_Char)ik_integer_to_int(s_namespace_separator);
  const XML_Memory_Handling_Suite * memsuite = NULL;
  XML_Parser		parser;
  parser = XML_ParserCreate_MM(encoding, memsuite, separator);
  return ika_pointer_alloc(pcb, (ik_ulong)parser);
}
#endif

/* end of file */
