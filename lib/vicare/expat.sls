;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: Expat binding backend
;;;Date: Sat Jan 21, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
#!(load-shared-library "vicare-expat")
(library (vicare expat)
  (export
    XML_SetElementDeclHandler
    XML_SetAttlistDeclHandler
    XML_SetXmlDeclHandler
    XML_ParserCreate
    XML_ParserCreateNS
    XML_ParserCreate_MM
    XML_ParserReset
    XML_SetEntityDeclHandler
    XML_SetElementHandler
    XML_SetStartElementHandler
    XML_SetEndElementHandler
    XML_SetCharacterDataHandler
    XML_SetProcessingInstructionHandler
    XML_SetCommentHandler
    XML_SetCdataSectionHandler
    XML_SetStartCdataSectionHandler
    XML_SetEndCdataSectionHandler
    XML_SetDefaultHandler
    XML_SetDefaultHandlerExpand
    XML_SetDoctypeDeclHandler
    XML_SetStartDoctypeDeclHandler
    XML_SetEndDoctypeDeclHandler
    XML_SetUnparsedEntityDeclHandler
    XML_SetNotationDeclHandler
    XML_SetNamespaceDeclHandler
    XML_SetStartNamespaceDeclHandler
    XML_SetEndNamespaceDeclHandler
    XML_SetNotStandaloneHandler
    XML_SetExternalEntityRefHandler
    XML_SetExternalEntityRefHandlerArg
    XML_SetSkippedEntityHandler
    XML_SetUnknownEncodingHandler
    XML_DefaultCurrent
    XML_SetReturnNSTriplet
    XML_SetUserData
    XML_SetEncoding
    XML_UseParserAsHandlerArg
    XML_UseForeignDTD
    XML_SetBase
    XML_GetBase
    XML_GetSpecifiedAttributeCount
    XML_GetIdAttributeIndex
    XML_Parse
    XML_GetBuffer
    XML_ParseBuffer
    XML_StopParser
    XML_ResumeParser
    XML_GetParsingStatus
    XML_ExternalEntityParserCreate
    XML_SetParamEntityParsing
    XML_GetErrorCode
    XML_GetCurrentLineNumber
    XML_GetCurrentColumnNumber
    XML_GetCurrentByteIndex
    XML_GetCurrentByteCount
    XML_GetInputContext
    XML_FreeContentModel
    XML_MemMalloc
    XML_MemRealloc
    XML_MemFree
    XML_ParserFree
    XML_ErrorString
    XML_ExpatVersion
    XML_GetFeatureList)
  (import (vicare)
    (vicare syntactic-extensions)
    (prefix (vicare ffi) ffi.)
    (prefix (vicare words) words.))


;;;; arguments validation

(define-argument-validation (parser who obj)
  (parser? obj)
  (assertion-violation who "expected Expat parser as argument" obj))

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (pointer who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (callback who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected callback as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

(define-argument-validation (false/bytevector who obj)
  (or (not obj) (bytevector? obj))
  (assertion-violation who "expected false or bytevector as argument" obj))

(define-argument-validation (false/encoding-symbol who obj)
  (or (not obj) (memq obj '(UTF-8 UTF-16 ISO-8859-1)))
  (assertion-violation who "expected false or Expat encoding symbol as argument" obj))


;;;; helpers

(define (%document-encoding-symbol->fixnum encoding)
  (case encoding
    ((UTF-8)		1)
    ((UTF-16)		2)
    ((ISO-8859-1)	3)
    ;;Honour   the  encoding   specified   in  the
    ;;document.
    (else		0)))


;;;; callbacks

(let-syntax ((declare (syntax-rules ()
			((_ ?func ?who)
			 (define (?who parser callback)
			   (define who '?who)
			   (with-arguments-validation (who)
			       ((parser		parser)
				(callback	callback))
			     (foreign-call ?func parser callback)))))))
  (declare "set_element_decl_handler"		XML_SetElementDeclHandler)
  (declare "set_attlist_decl_handler"		XML_SetAttlistDeclHandler)
  (declare "set_xml_decl_handler"		XML_SetXmlDeclHandler)
  (declare "set_entity_decl_handler"		XML_SetEntityDeclHandler)
  (declare "set_start_element_handler"		XML_SetStartElementHandler)
  (declare "set_end_element_handler"		XML_SetEndElementHandler)
  (declare "set_character_data_handler"		XML_SetCharacterDataHandler)
  (declare "set_processing_instruction_handler" XML_SetProcessingInstructionHandler)
  (declare "set_comment_handler"		XML_SetCommentHandler)
  (declare "set_start_cdata_section_handler"	XML_SetStartCdataSectionHandler)
  (declare "set_end_cdata_section_handler"	XML_SetEndCdataSectionHandler)
  (declare "set_default_handler"		XML_SetDefaultHandler)
  (declare "set_default_handler_expand"		XML_SetDefaultHandlerExpand)
  (declare "set_start_doctype_decl_handler"	XML_SetStartDoctypeDeclHandler)
  (declare "set_end_doctype_decl_handler"	XML_SetEndDoctypeDeclHandler)
  (declare "set_unparsed_entity_decl_handler"	XML_SetUnparsedEntityDeclHandler)
  (declare "set_notation_decl_handler"		XML_SetNotationDeclHandler)
  (declare "set_start_namespace_decl_handler"	XML_SetStartNamespaceDeclHandler)
  (declare "set_end_namespace_decl_handler"	XML_SetEndNamespaceDeclHandler)
  (declare "set_not_standalone_handler"		XML_SetNotStandaloneHandler)
  (declare "set_external_entity_ref_handler"	XML_SetExternalEntityRefHandler)
  (declare "set_skipped_entity_handler"		XML_SetSkippedEntityHandler))

(let-syntax ((declare (syntax-rules ()
			((_ ?func ?who)
			 (define (?who parser start-callback end-callback)
			   (define who '?who)
			   (with-arguments-validation (who)
			       ((parser		parser)
				(callback	start-callback)
				(callback	end-callback))
			     (foreign-call ?func parser start-callback end-callback)))))))
  (declare "set_element_handler"		XML_SetElementHandler)
  (declare "set_cdata_section_handler"		XML_SetCdataSectionHandler)
  (declare "set_doctype_decl_handler"		XML_SetDoctypeDeclHandler)
  (declare "set_namespace_decl_handler"		XML_SetNamespaceDeclHandler))

;;; --------------------------------------------------------------------

(define (XML_SetExternalEntityRefHandlerArg parser pointer)
  (define who 'XML_SetExternalEntityRefHandlerArg)
  (with-arguments-validation (who)
      ((parser	parser)
       (pointer	pointer))
    (foreign-call "ik_expat_set_external_entity_ref_handler_arg" parser pointer)))

(define (XML_SetUnknownEncodingHandler parser callback pointer)
  (define who 'XML_SetUnknownEncodingHandler)
  (with-arguments-validation (who)
      ((parser		parser)
       (callback	callback)
       (pointer		pointer))
    (foreign-call "ik_expat_set_unknown_encoding_handler" parser callback pointer)))


;;;; parser structure

(define-struct parser
  (pointer))

(define (%struct-parser-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[expat-parser")
  (%display " pointer=")	(%display (parser-pointer S))
  (%display "]"))

(define %parser-guardian
  (make-guardian))

(define (%free-allocated-guardian)
  (do ((P (%parser-guardian) (%parser-guardian)))
      ((not P))
    (foreign-call "ik_expat_parser_free" P)))


;;;; parsers

(define XML_ParserCreate
  (case-lambda
   (()
    (XML_ParserCreate #f))
   ((encoding)
    (define who 'XML_ParserCreate)
    (with-arguments-validation (who)
	((false/encoding-symbol	encoding))
      (let ((rv (foreign-call "ik_expat_parser_create" (%document-encoding-symbol->fixnum encoding))))
	(if rv
	    (%parser-guardian rv)
	  (error who "error allocating Expat parser" encoding)))))))

ik_expat_parser_create_ns (encoding namespace_separator)

ik_expat_free_content_model (parser model)

ik_expat_parser_free (parser)


ik_expat_parser_reset (parser encoding)

ik_expat_default_current (parser)

ik_expat_set_user_data (parser pointer)

ik_expat_get_user_data (parser)

ik_expat_set_encoding (parser encoding)

ik_expat_use_parser_as_handler_arg (parser)

ik_expat_user_foreign_dtd (parser use_dtd)

ik_expat_set_base (parser base)

ik_expat_get_base (parser)

ik_expat_parse (parser buffer buflen is_final)

ik_expat_get_buffer (parser length)

ik_expat_parse_buffer (parser buflen is_final)

ik_expat_stop_parser (parser resumable)

ik_expat_resume_parser (parser)

ik_expat_get_parsing_status (parser status)

ik_expat_external_entity_parser_create (parser context encoding)

ik_expat_set_param_entity_parsing (parser parsing)

ik_expat_get_error_code (parser)

ik_expat_get_current_line_number (parser)

ik_expat_get_current_column_number (parser)

ik_expat_get_current_byte_index (parser)

ik_expat_get_current_byte_count (parser)

ik_expat_get_input_context (parser)



;;;; attributes

(define (XML_GetSpecifiedAttributeCount parser)
  (define who 'XML_GetSpecifiedAttributeCount)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_specified_attribute_count" parser)))

(define (XML_GetIdAttributeIndex parser)
  (define who 'XML_GetIdAttributeIndex)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_id_attribute_index" parser)))


;;;; error reporting

(define (XML_ErrorString code)
  (define who 'XML_ErrorString)
  (with-arguments-validation (who)
      ((words.signed-int	code))
    (foreign-call "ik_expat_error_string" code)))


;;;; miscellaneous functions

(define (XML_ExpatVersion)
  (foreign-call "ik_expat_version"))

(define (XML_ExpatVersionInfo)
  (foreign-call "ik_expat_version_info"))

(define (XML_GetFeatureList)
  (foreign-call "ik_expat_get_feature_list"))


;;;; done

(set-rtd-printer! (type-descriptor parsre) %struct-parser-printer)

)

;;; end of file
