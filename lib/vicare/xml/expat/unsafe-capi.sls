;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: unsafe interface to the C language API
;;;Date: Tue Aug 20, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare xml expat unsafe-capi)
  (export

    ;; version functions
    vicare-expat-version-interface-current
    vicare-expat-version-interface-revision
    vicare-expat-version-interface-age
    vicare-expat-version

    ;; parser functions
    XML_ParserCreate
    XML_ParserCreateNS
    XML_ParserReset
    XML_ParserFree
    XML_ExternalEntityParserCreate
    XML_SetExternalEntityRefHandlerArg
    XML_FreeContentModel
    XML_UseParserAsHandlerArg
    XML_SetParamEntityParsing
;;;Not publicly interfaced.
;;;
;;; XML_SetUnknownEncodingHandler
    XML_SetEncoding
    XML_SetUserData
    XML_GetUserData
    XML_SetBase
    XML_GetBase
    XML_UseForeignDTD
    XML_SetReturnNSTriplet
    XML_Parse
    XML_GetBuffer
    XML_ParseBuffer
    XML_StopParser
    XML_ResumeParser
    XML_GetParsingStatus
    XML_GetInputContext

    ;; elements
    XML_DefaultCurrent

    ;; attributes
    XML_GetSpecifiedAttributeCount
    XML_GetIdAttributeIndex

    ;; error reporting
    XML_ErrorString
    XML_GetErrorCode
    XML_GetCurrentLineNumber
    XML_GetCurrentColumnNumber
    XML_GetCurrentByteIndex
    XML_GetCurrentByteCount

    ;; miscellaneous functions
    XML_ExpatVersion
    XML_ExpatVersionInfo
    XML_GetFeatureList

    ;; handler setter functions
    XML_SetElementDeclHandler
    XML_SetAttlistDeclHandler
    XML_SetXmlDeclHandler
    XML_SetEntityDeclHandler
    XML_SetStartElementHandler
    XML_SetEndElementHandler
    XML_SetCharacterDataHandler
    XML_SetProcessingInstructionHandler
    XML_SetCommentHandler
    XML_SetStartCdataSectionHandler
    XML_SetEndCdataSectionHandler
    XML_SetDefaultHandler
    XML_SetDefaultHandlerExpand
    XML_SetStartDoctypeDeclHandler
    XML_SetEndDoctypeDeclHandler
    XML_SetUnparsedEntityDeclHandler
    XML_SetNotationDeclHandler
    XML_SetStartNamespaceDeclHandler
    XML_SetEndNamespaceDeclHandler
    XML_SetNotStandaloneHandler
    XML_SetExternalEntityRefHandler
    XML_SetSkippedEntityHandler

    XML_SetElementHandler
    XML_SetCdataSectionHandler
    XML_SetDoctypeDeclHandler
    XML_SetNamespaceDeclHandler

    ;; struct XML_Content raw accessors
    XML_Content.type
    XML_Content.quant
    XML_Content.name
    XML_Content.numchildren
    XML_Content.children

;;; --------------------------------------------------------------------
;;; still to be implemented

    )
  (import (vicare))


;;;; version functions

(define-inline (vicare-expat-version-interface-current)
  (foreign-call "ikrt_expat_version_interface_current"))

(define-inline (vicare-expat-version-interface-revision)
  (foreign-call "ikrt_expat_version_interface_revision"))

(define-inline (vicare-expat-version-interface-age)
  (foreign-call "ikrt_expat_version_interface_age"))

(define-inline (vicare-expat-version)
  (foreign-call "ikrt_expat_version"))


;;;; parser functions

(define-inline (XML_ParserCreate encoding-fx)
  (foreign-call "ikrt_expat_xml_parser_create" encoding-fx))

(define-inline (XML_ParserCreateNS encoding namespace-separator)
  (foreign-call "ikrt_expat_xml_parser_create_ns" encoding namespace-separator))

(define-inline (XML_ParserReset parser encoding-fx)
  (foreign-call "ikrt_expat_xml_parser_reset" parser encoding-fx))

(define-inline (XML_ParserFree parser)
  (foreign-call "ikrt_expat_xml_parser_free" parser))

;;; --------------------------------------------------------------------

;;Not publicly interfaced.
;;
;; (define-inline (XML_SetUnknownEncodingHandler parser callback pointer)
;;   (foreign-call "ikrt_expat_xml_set_unknown_encoding_handler" parser callback pointer))

(define-inline (XML_SetExternalEntityRefHandlerArg parser pointer)
  (foreign-call "ikrt_expat_xml_set_external_entity_ref_handler_arg" parser pointer))

(define-inline (XML_FreeContentModel parser model)
  (foreign-call "ikrt_expat_xml_free_content_model" parser model))

(define-inline (XML_UseParserAsHandlerArg parser)
  (foreign-call "ikrt_expat_xml_use_parser_as_handler_arg" parser))

(define-inline (XML_ExternalEntityParserCreate parser context encoding-fx)
  (foreign-call "ikrt_expat_xml_external_entity_parser_create" parser context encoding-fx))

(define-inline (XML_SetParamEntityParsing parser parsing)
  (foreign-call "ikrt_expat_xml_set_param_entity_parsing" parser parsing))

;;; --------------------------------------------------------------------

(define-inline (XML_SetEncoding parser encoding)
  (foreign-call "ikrt_expat_xml_set_encoding" parser encoding))

(define-inline (XML_SetUserData parser pointer)
  (foreign-call "ikrt_expat_xml_set_user_data" parser pointer))

(define-inline (XML_GetUserData parser)
  (foreign-call "ikrt_expat_xml_get_user_data" parser))

(define-inline (XML_SetBase parser base)
  (foreign-call "ikrt_expat_xml_set_base" parser base))

(define-inline (XML_GetBase parser)
  (foreign-call "ikrt_expat_xml_get_base" parser))

(define-inline (XML_UseForeignDTD parser use-dtd?)
  (foreign-call "ikrt_expat_xml_user_foreign_dtd" parser use-dtd?))

(define-inline (XML_SetReturnNSTriplet parser do-nst?)
  (foreign-call "ikrt_expat_xml_set_return_ns_triplet" parser do-nst?))

;;; --------------------------------------------------------------------

(define-inline (XML_Parse parser buf.data buf.len final?)
  (foreign-call "ikrt_expat_xml_parse" parser buf.data buf.len final?))

(define-inline (XML_GetBuffer parser buf.len)
  (foreign-call "ikrt_expat_xml_get_buffer" parser buf.len))

(define-inline (XML_ParseBuffer parser buf.len final?)
  (foreign-call "ikrt_expat_xml_parse_buffer" parser buf.len final?))

;;; --------------------------------------------------------------------

(define-inline (XML_StopParser parser resumable?)
  (foreign-call "ikrt_expat_xml_stop_parser" parser resumable?))

(define-inline (XML_ResumeParser parser)
  (foreign-call "ikrt_expat_xml_resume_parser" parser))

;;; --------------------------------------------------------------------

(define-inline (XML_GetParsingStatus parser status)
  (foreign-call "ikrt_expat_xml_get_parsing_status" parser status))

(define-inline (XML_GetInputContext parser)
  (foreign-call "ikrt_expat_xml_get_input_context" parser))


;;;; elements

(define-inline (XML_DefaultCurrent parser)
  (foreign-call "ikrt_expat_xml_default_current" parser))


;;;; attributes

(define-inline (XML_GetSpecifiedAttributeCount parser)
  (foreign-call "ikrt_expat_xml_get_specified_attribute_count" parser))

(define-inline (XML_GetIdAttributeIndex parser)
  (foreign-call "ikrt_expat_xml_get_id_attribute_index" parser))


;;;; error reporting

(define-inline (XML_ErrorString code)
  (latin1->string (foreign-call "ikrt_expat_xml_error_string" code)))

(define-inline (XML_GetErrorCode parser)
  (foreign-call "ikrt_expat_xml_get_error_code" parser))

(define-inline (XML_GetCurrentLineNumber parser)
  (foreign-call "ikrt_expat_xml_get_current_line_number" parser))

(define-inline (XML_GetCurrentColumnNumber parser)
  (foreign-call "ikrt_expat_xml_get_current_column_number" parser))

(define-inline (XML_GetCurrentByteIndex parser)
  (foreign-call "ikrt_expat_xml_get_current_byte_index" parser))

(define-inline (XML_GetCurrentByteCount parser)
  (foreign-call "ikrt_expat_xml_get_current_byte_count" parser))


;;;; miscellaneous functions

(define-inline (XML_ExpatVersion)
  (foreign-call "ikrt_expat_xml_version"))

(define-inline (XML_ExpatVersionInfo)
  (foreign-call "ikrt_expat_xml_version_info"))

(define-inline (XML_GetFeatureList)
  (foreign-call "ikrt_expat_xml_get_feature_list"))


;;;; handler setter functions

(let-syntax ((declare (syntax-rules ()
			((_ ?c-func ?who)
			 (define-inline (?who parser callback)
			   (foreign-call ?c-func parser callback))))))
  (declare "ikrt_expat_xml_set_element_decl_handler"		XML_SetElementDeclHandler)
  (declare "ikrt_expat_xml_set_attlist_decl_handler"		XML_SetAttlistDeclHandler)
  (declare "ikrt_expat_xml_set_xml_decl_handler"		XML_SetXmlDeclHandler)
  (declare "ikrt_expat_xml_set_entity_decl_handler"		XML_SetEntityDeclHandler)
  (declare "ikrt_expat_xml_set_start_element_handler"		XML_SetStartElementHandler)
  (declare "ikrt_expat_xml_set_end_element_handler"		XML_SetEndElementHandler)
  (declare "ikrt_expat_xml_set_character_data_handler"		XML_SetCharacterDataHandler)
  (declare "ikrt_expat_xml_set_processing_instruction_handler"	XML_SetProcessingInstructionHandler)
  (declare "ikrt_expat_xml_set_comment_handler"			XML_SetCommentHandler)
  (declare "ikrt_expat_xml_set_start_cdata_section_handler"	XML_SetStartCdataSectionHandler)
  (declare "ikrt_expat_xml_set_end_cdata_section_handler"	XML_SetEndCdataSectionHandler)
  (declare "ikrt_expat_xml_set_default_handler"			XML_SetDefaultHandler)
  (declare "ikrt_expat_xml_set_default_handler_expand"		XML_SetDefaultHandlerExpand)
  (declare "ikrt_expat_xml_set_start_doctype_decl_handler"	XML_SetStartDoctypeDeclHandler)
  (declare "ikrt_expat_xml_set_end_doctype_decl_handler"	XML_SetEndDoctypeDeclHandler)
  (declare "ikrt_expat_xml_set_unparsed_entity_decl_handler"	XML_SetUnparsedEntityDeclHandler)
  (declare "ikrt_expat_xml_set_notation_decl_handler"		XML_SetNotationDeclHandler)
  (declare "ikrt_expat_xml_set_start_namespace_decl_handler"	XML_SetStartNamespaceDeclHandler)
  (declare "ikrt_expat_xml_set_end_namespace_decl_handler"	XML_SetEndNamespaceDeclHandler)
  (declare "ikrt_expat_xml_set_not_standalone_handler"		XML_SetNotStandaloneHandler)
  (declare "ikrt_expat_xml_set_external_entity_ref_handler"	XML_SetExternalEntityRefHandler)
  (declare "ikrt_expat_xml_set_skipped_entity_handler"		XML_SetSkippedEntityHandler))

;;; --------------------------------------------------------------------

(define-inline (XML_SetElementHandler parser start-callback end-callback)
  (foreign-call "ikrt_expat_xml_set_element_handler" parser start-callback end-callback))

(define-inline (XML_SetCdataSectionHandler parser start-callback end-callback)
  (foreign-call "ikrt_expat_xml_set_cdata_section_handler" parser start-callback end-callback))

(define-inline (XML_SetDoctypeDeclHandler parser start-callback end-callback)
  (foreign-call "ikrt_expat_xml_set_doctype_decl_handler" parser start-callback end-callback))

(define-inline (XML_SetNamespaceDeclHandler parser start-callback end-callback)
  (foreign-call "ikrt_expat_xml_set_namespace_decl_handler" parser start-callback end-callback))


;;;; struct XML_Content raw accessors

(define-inline (XML_Content.type pointer)
  (foreign-call "ikrt_expat_xml_content_type_ref" pointer))

(define-inline (XML_Content.quant pointer)
  (foreign-call "ikrt_expat_xml_content_quant_ref" pointer))

(define-inline (XML_Content.name pointer)
  (foreign-call "ikrt_expat_xml_content_name_ref" pointer))

(define-inline (XML_Content.numchildren pointer)
  (foreign-call "ikrt_expat_xml_content_numchildren_ref" pointer))

(define-inline (XML_Content.children pointer index)
  (foreign-call "ikrt_expat_xml_content_children_ref" pointer index))


;;;; done

)

;;; end of file
