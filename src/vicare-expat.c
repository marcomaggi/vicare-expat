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

#define DECLARE_SINGLE_CALLBACK_SETTER(IK_SUFFIX,EX_FUNCTION)	\
  ikptr								\
  ik_expat_ ## IK_SUFFIX (ikptr s_parser, ikptr s_callback)	\
  {								\
    EX_FUNCTION(EX_PARSER(s_parser), EX_CALLBACK(s_callback));	\
    return void_object;						\
  }

#define DECLARE_DOUBLE_CALLBACK_SETTER(IK_SUFFIX,EX_FUNCTION)		\
  ikptr									\
  ik_expat_ ## IK_SUFFIX (ikptr s_parser, ikptr s_start, ikptr s_end)	\
  {									\
    EX_FUNCTION(EX_PARSER(s_parser), EX_CALLBACK(s_start), EX_CALLBACK(s_end));	\
    return void_object;							\
  }

DECLARE_SINGLE_CALLBACK_SETTER(set_element_decl_handler, XML_SetElementDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_attlist_decl_handler, XML_SetAttlistDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_xml_decl_handler,     XML_SetXmlDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_entity_decl_handler,  XML_SetEntityDeclHandler)
DECLARE_DOUBLE_CALLBACK_SETTER(set_element_handler,      XML_SetElementHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_start_element_handler,  XML_SetStartElementHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_end_element_handler,    XML_SetEndElementHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_character_data_handler, XML_SetCharacterDataHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_processing_instruction_handler, XML_SetProcessingInstructionHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_comment_handler,            XML_SetCommentHandler)
DECLARE_DOUBLE_CALLBACK_SETTER(set_cdata_section_handler,      XML_SetCdataSectionHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_start_cdata_section_handler,XML_SetStartCdataSectionHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_end_cdata_section_handler,  XML_SetEndCdataSectionHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_default_handler,            XML_SetDefaultHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_default_handler_expand,     XML_SetDefaultHandlerExpand)
DECLARE_DOUBLE_CALLBACK_SETTER(set_doctype_decl_handler,       XML_SetDoctypeDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_start_doctype_decl_handler, XML_SetStartDoctypeDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_end_doctype_decl_handler,   XML_SetEndDoctypeDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_unparsed_entity_decl_handler, XML_SetUnparsedEntityDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_notation_decl_handler,      XML_SetNotationDeclHandler)
DECLARE_DOUBLE_CALLBACK_SETTER(set_namespace_decl_handler,     XML_SetNamespaceDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_start_namespace_decl_handler, XML_SetStartNamespaceDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_end_namespace_decl_handler, XML_SetEndNamespaceDeclHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_not_standalone_handler,     XML_SetNotStandaloneHandler)
DECLARE_SINGLE_CALLBACK_SETTER(set_external_entity_ref_handler,XML_SetExternalEntityRefHandler)


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
ikptr
ik_expat_parser_reset (ikptr s_parser, ikptr s_encoding)
{
  XML_Parser		parser   = EX_PARSER(s_parser);
  const XML_Char *	encoding = IK_BYTEVECTOR_DATA_VOIDP(s_encoding);
  XML_Bool		rv;
  rv = XML_ParserReset(parser, encoding);
  return rv? true_object : false_object;
}

/* end of file */
