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
DECLARE_SINGLE_CALLBACK_SETTER(set_skipped_entity_handler,     XML_SetSkippedEntityHandler)


ikptr
ik_expat_set_external_entity_ref_handler_arg (ikptr s_parser, ikptr s_pointer)
{
  XML_SetExternalEntityRefHandlerArg(EX_PARSER(s_parser), IK_POINTER_DATA_VOIDP(s_pointer));
  return void_object;
}
ikptr
ik_expat_set_unknown_encoding_handler (ikptr s_parser, ikptr s_callback, ikptr s_pointer)
{
  XML_SetUnknownEncodingHandler(EX_PARSER(s_parser), EX_CALLBACK(s_callback),
				IK_POINTER_DATA_VOIDP(s_pointer));
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
ikptr
ik_expat_parser_reset (ikptr s_parser, ikptr s_encoding)
{
  XML_Parser		parser   = EX_PARSER(s_parser);
  const XML_Char *	encoding = IK_BYTEVECTOR_DATA_VOIDP(s_encoding);
  XML_Bool		rv;
  rv = XML_ParserReset(parser, encoding);
  return rv? true_object : false_object;
}
ikptr
ik_expat_default_current (ikptr s_parser)
{
  XML_DefaultCurrent(EX_PARSER(s_parser));
  return void_object;
}
ikptr
ik_expat_set_user_data (ikptr s_parser, ikptr s_pointer)
{
  XML_SetUserData(EX_PARSER(s_parser), IK_BYTEVECTOR_DATA_VOIDP(s_pointer));
  return void_object;
}
ikptr
ik_expat_get_user_data (ikptr s_parser, ikpcb * pcb)
{
  return ika_pointer_alloc(pcb, (ik_ulong)XML_GetUserData(EX_PARSER(s_parser)));
}
ikptr
ik_expat_set_encoding (ikptr s_parser, ikptr s_encoding, ikpcb * pcb)
{
  const XML_Char *	encoding = IK_BYTEVECTOR_DATA_VOIDP(s_encoding);
  enum XML_Status	rv;
  rv = XML_SetEncoding(EX_PARSER(s_parser), encoding);
  return ika_integer_from_int(pcb, (int)rv);
}
ikptr
ik_expat_use_parser_as_handler_arg (ikptr s_parser)
{
  XML_UseParserAsHandlerArg(EX_PARSER(s_parser));
  return void_object;
}
ikptr
ik_expat_user_foreign_dtd (ikptr s_parser, ikptr s_use_dtd, ikpcb * pcb)
{
  XML_Bool		use_dtd = (true_object == s_use_dtd)? XML_TRUE : XML_FALSE;
  enum XML_Error	rv;
  rv = XML_UseForeignDTD(EX_PARSER(s_parser), use_dtd);
  return ika_integer_from_int(pcb, (int)rv);
}

/* ------------------------------------------------------------------ */

ikptr
ik_expat_set_base (ikptr s_parser, ikptr s_base, ikpcb * pcb)
{
  const XML_Char *	base = IK_BYTEVECTOR_DATA_VOIDP(s_base);
  enum XML_Status	rv;
  rv = XML_SetBase(EX_PARSER(s_parser), base);
  return ika_integer_from_int(pcb, (int)rv);
}
ikptr
ik_expat_get_base (ikptr s_parser, ikpcb * pcb)
{
  const XML_Char *	base;
  base = XML_GetBase(EX_PARSER(s_parser));
  return ika_bytevector_from_cstring(pcb, base);
}

/* ------------------------------------------------------------------ */

ikptr
ik_expat_parse (ikptr s_parser, ikptr s_buffer, ikptr s_buflen, ikptr s_is_final, ikpcb * pcb)
{
  const char *	buffer;
  int		buflen;
  int		is_final = (true_object == s_is_final)? 1 : 0;
  enum XML_Status	rv;
  if (ik_is_pointer(s_buffer)) {
    buffer = IK_POINTER_DATA_VOIDP(s_buffer);
    buflen = ik_integer_to_int(s_buflen);
  } else { /* is bytevector */
    buffer = IK_BYTEVECTOR_DATA_CHARP(s_buffer);
    buflen = (int)IK_BYTEVECTOR_LENGTH(s_buffer);
  }
  rv = XML_Parse(EX_PARSER(s_parser), buffer, buflen, is_final);
  return ika_integer_from_int(pcb, (int)rv);
}
ikptr
ik_expat_get_buffer (ikptr s_parser, ikptr s_length, ikpcb * pcb)
{
  int		len = ik_integer_to_int(s_length);
  void *	buffer;
  buffer = XML_GetBuffer(EX_PARSER(s_parser), len);
  return ika_pointer_alloc(pcb, (ik_ulong)buffer);
}
ikptr
ik_expat_parse_buffer (ikptr s_parser, ikptr s_buflen, ikptr s_is_final, ikpcb * pcb)
{
  int	buflen   = ik_integer_to_int(s_buflen);
  int	is_final = (true_object == s_is_final)? 1 : 0;
  enum XML_Status	rv;
  rv = XML_ParseBuffer(EX_PARSER(s_parser), buflen, is_final);
  return ika_integer_from_int(pcb, (int)rv);
}
ikptr
ik_expat_stop_parser (ikptr s_parser, ikptr s_resumable, ikpcb * pcb)
{
  XML_Bool		resumable = (true_object == s_resumable)? XML_TRUE : XML_FALSE;
  enum XML_Status	rv;
  rv = XML_StopParser(EX_PARSER(s_parser), resumable);
  return ika_integer_from_int(pcb, (int)rv);
}
ikptr
ik_expat_resume_parser (ikptr s_parser, ikpcb * pcb)
{
  enum XML_Status	rv;
  rv = XML_ResumeParser(EX_PARSER(s_parser));
  return ika_integer_from_int(pcb, (int)rv);
}
ikptr
ik_expat_get_parsing_status (ikptr s_parser, ikptr s_status)
{
  XML_ParsingStatus	status;
  XML_GetParsingStatus(EX_PARSER(s_parser), &status);
  IK_FIELD(s_status, 0) = IK_FIX(status.parsing);
  IK_FIELD(s_status, 1) = status.finalBuffer? true_object : false_object;
  return void_object;
}

/* ------------------------------------------------------------------ */

ikptr
ik_expat_external_entity_parser_create (ikptr s_parser, ikptr s_context, ikptr s_encoding, ikpcb * pcb)
{
  const XML_Char *	context  = IK_POINTER_DATA_VOIDP(s_context);
  const XML_Char *	encoding = IK_BYTEVECTOR_DATA_VOIDP(s_encoding);
  XML_Parser		entity_parser;
  entity_parser = XML_ExternalEntityParserCreate(EX_PARSER(s_parser), context, encoding);
  return ika_pointer_alloc(pcb, (ik_ulong)entity_parser);
}
ikptr
ik_expat_set_param_entity_parsing (ikptr s_parser, ikptr s_parsing, ikpcb * pcb)
{
  enum XML_ParamEntityParsing parsing = ik_integer_to_int(s_parsing);
  int	rv;
  rv = XML_SetParamEntityParsing(EX_PARSER(s_parser), parsing);
  return ika_integer_from_int(pcb, rv);
}
ikptr
ik_expat_get_error_code (ikptr s_parser, ikpcb * pcb)
{
  enum XML_Error	rv;
  rv = XML_GetErrorCode(EX_PARSER(s_parser));
  return ika_integer_from_int(pcb, rv);
}

/* ------------------------------------------------------------------ */

ikptr
ik_expat_get_current_line_number (ikptr s_parser, ikpcb * pcb)
{
  XML_Size	rv;
  rv = XML_GetCurrentLineNumber(EX_PARSER(s_parser));
  return ika_integer_from_ulong(pcb, (ik_ulong)rv);
}
ikptr
ik_expat_get_current_column_number (ikptr s_parser, ikpcb * pcb)
{
  XML_Size	rv;
  rv = XML_GetCurrentColumnNumber(EX_PARSER(s_parser));
  return ika_integer_from_ulong(pcb, (ik_ulong)rv);
}
ikptr
ik_expat_get_current_byte_index (ikptr s_parser, ikpcb * pcb)
{
  XML_Size	rv;
  rv = XML_GetCurrentByteIndex(EX_PARSER(s_parser));
  return ika_integer_from_ulong(pcb, (ik_ulong)rv);
}
ikptr
ik_expat_get_current_byte_count (ikptr s_parser, ikpcb * pcb)
{
  int	rv;
  rv = XML_GetCurrentByteCount(EX_PARSER(s_parser));
  return ika_integer_from_int(pcb, rv);
}
ikptr
ik_expat_get_input_context (ikptr s_parser, ikpcb * pcb)
/* Return a  vector describing the  current input buffer:  pointer, byte
   offset, byte size. */
{
  const char *	buffer;
  int		offset;
  int		size;
  ikptr		s_vec;
  buffer = XML_GetInputContext(EX_PARSER(s_parser), &offset, &size);
  s_vec  = ika_vector_alloc_and_init(pcb, 3);
  pcb->root0 = &s_vec;
  {
    IK_ITEM(s_vec, 0) = ika_pointer_alloc(pcb, (ik_ulong)buffer);
    IK_ITEM(s_vec, 1) = ika_integer_from_int(pcb, offset);
    IK_ITEM(s_vec, 2) = ika_integer_from_int(pcb, size);
  }
  pcb->root0 = NULL;
  return s_vec;
}

/* ------------------------------------------------------------------ */

ikptr
ik_expat_free_content_model (ikptr s_parser, ikptr s_model)
{
  XML_Content *	model = IK_POINTER_DATA_VOIDP(s_model);
  XML_FreeContentModel(EX_PARSER(s_parser), model);
  return void_object;
}
ikptr
ik_expat_parser_free (ikptr s_parser)
{
  XML_ParserFree(EX_PARSER(s_parser));
  return void_object;
}



/** --------------------------------------------------------------------
 ** Attributes.
 ** ----------------------------------------------------------------- */

ikptr
ik_expat_get_specified_attribute_count (ikptr s_parser, ikpcb * pcb)
{
  int	rv;
  rv = XML_GetSpecifiedAttributeCount(EX_PARSER(s_parser));
  return ika_integer_from_int(pcb, (int)rv);
}
ikptr
ik_expat_get_id_attribute_index (ikptr s_parser, ikpcb * pcb)
{
  int	rv;
  rv = XML_GetIdAttributeIndex(EX_PARSER(s_parser));
  return ika_integer_from_int(pcb, (int)rv);
}


/** --------------------------------------------------------------------
 ** Error reporting.
 ** ----------------------------------------------------------------- */

ikptr
ik_expat_error_string (ikptr s_code, ikpcb * pcb)
{
  enum XML_Error	code = (enum XML_Error)ik_integer_to_int(s_code);
  const XML_LChar *	rv;
  rv = XML_ErrorString(code);
  return ika_bytevector_from_cstring(pcb, rv);
}


/** --------------------------------------------------------------------
 ** Miscellaneous functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_expat_version (ikpcb * pcb)
{
  const XML_LChar *	rv;
  rv = XML_ExpatVersion();
  return ika_bytevector_from_cstring(pcb, rv);
}
ikptr
ik_expat_version_info (ikpcb * pcb)
{
  XML_Expat_Version	info;
  ikptr			s_vec;
  info = XML_ExpatVersionInfo();
  s_vec  = ika_vector_alloc_and_init(pcb, 3);
  pcb->root0 = &s_vec;
  {
    IK_ASS(IK_ITEM(s_vec, 0), ika_integer_from_int(pcb, info.major));
    IK_ASS(IK_ITEM(s_vec, 1), ika_integer_from_int(pcb, info.minor));
    IK_ASS(IK_ITEM(s_vec, 2), ika_integer_from_int(pcb, info.micro));
  }
  pcb->root0 = NULL;
  return s_vec;
}
ikptr
ik_expat_get_feature_list (ikpcb * pcb)
{
  const XML_Feature *	feats;
  int			number_of_feats;
  ikptr			s_feats;
  feats = XML_GetFeatureList();
  /* First acquire the number of features. */
  for (number_of_feats=0; XML_FEATURE_END != feats[number_of_feats].feature; ++number_of_feats);
  s_feats = ika_vector_alloc_and_init(pcb, number_of_feats);
  pcb->root0 = &s_feats;
  {
    int		i;
    for (i=0; i<number_of_feats; ++i) {
      IK_ASS(IK_ITEM(s_feats, i), ika_vector_alloc_and_init(pcb, 3));
      IK_ASS(IK_ITEM(IK_ITEM(s_feats, i), 0),
	     ika_integer_from_int(pcb, feats[i].feature));
      IK_ASS(IK_ITEM(IK_ITEM(s_feats, i), 1),
	     ika_bytevector_from_cstring(pcb, feats[i].name));
      switch (feats[i].feature) {
      case XML_FEATURE_SIZEOF_XML_CHAR:
      case XML_FEATURE_SIZEOF_XML_LCHAR:
      case XML_FEATURE_CONTEXT_BYTES:
	IK_ASS(IK_ITEM(IK_ITEM(s_feats, i), 2),
	       ika_integer_from_long(pcb, feats[i].value));
	break;
      default:
	IK_ITEM(IK_ITEM(s_feats, i), 2) = false_object;
      }
    }
  }
  pcb->root0 = NULL;
  return s_feats;
}

/* end of file */
