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

    XML_ParserCreate
    XML_ParserCreateNS
;;;XML_ParserCreate_MM
    XML_ParserReset
    XML_DefaultCurrent
    XML_SetReturnNSTriplet
    XML_SetUserData
    XML_SetEncoding
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
    XML_ParserFree
    XML_ErrorString
    XML_ExpatVersion
    XML_GetFeatureList

    ;; callback setters
    XML_SetAttlistDeclHandler
    XML_SetCdataSectionHandler
    XML_SetCharacterDataHandler
    XML_SetCommentHandler
    XML_SetDefaultHandler
    XML_SetDefaultHandlerExpand
    XML_SetDoctypeDeclHandler
    XML_SetElementDeclHandler
    XML_SetElementHandler
    XML_SetEndCdataSectionHandler
    XML_SetEndDoctypeDeclHandler
    XML_SetEndElementHandler
    XML_SetEndNamespaceDeclHandler
    XML_SetEntityDeclHandler
    XML_SetExternalEntityRefHandler
    XML_SetNamespaceDeclHandler
    XML_SetNotStandaloneHandler
    XML_SetNotationDeclHandler
    XML_SetProcessingInstructionHandler
    XML_SetSkippedEntityHandler
    XML_SetStartCdataSectionHandler
    XML_SetStartDoctypeDeclHandler
    XML_SetStartElementHandler
    XML_SetStartNamespaceDeclHandler
    XML_SetUnknownEncodingHandler
    XML_SetUnparsedEntityDeclHandler
    XML_SetXmlDeclHandler

    ;; auxiliary callback functions
    XML_SetExternalEntityRefHandlerArg
    XML_UseParserAsHandlerArg

    ;; callback makers
    XML_AttlistDeclHandler
    XML_CharacterDataHandler
    XML_CommentHandler
    XML_DefaultHandler
    XML_ElementDeclHandler
    XML_EndCdataSectionHandler
    XML_EndDoctypeDeclHandler
    XML_EndElementHandler
    XML_EndNamespaceDeclHandler
    XML_EntityDeclHandler
    XML_ExternalEntityRefHandler
    XML_NotStandaloneHandler
    XML_NotationDeclHandler
    XML_ProcessingInstructionHandler
    XML_SkippedEntityHandler
    XML_StartCdataSectionHandler
    XML_StartDoctypeDeclHandler
    XML_StartElementHandler
    XML_StartNamespaceDeclHandler
    XML_UnknownEncodingHandler
    XML_UnparsedEntityDeclHandler
    XML_XmlDeclHandler

    ;; parsing status object
    make-parsing-status			parsing-status?
    parsing-status-parsing		parsing-status-final-buffer?)
  (import (vicare)
    (vicare expat constants)
    (vicare syntactic-extensions)
    (prefix (vicare ffi) ffi.)
    (prefix (vicare words) words.))


;;;; arguments validation

(define-argument-validation (boolean who obj)
  (boolean? obj)
  (assertion-violation who "expected boolean as argument" obj))

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

;;; --------------------------------------------------------------------

(define-argument-validation (false/bytevector who obj)
  (or (not obj) (bytevector? obj))
  (assertion-violation who "expected false or bytevector as argument" obj))

(define-argument-validation (pointer/bytevector who obj)
  (or (ffi.pointer? obj) (bytevector? obj))
  (assertion-violation who "expected pointer or bytevector as argument" obj))

(define-argument-validation (false/encoding-symbol who obj)
  (or (not obj) (memq obj '(UTF-8 UTF-16 ISO-8859-1)))
  (assertion-violation who "expected false or Expat encoding symbol as argument" obj))

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who "expected signed int as argument" obj))

(define-argument-validation (false/non-negative-signed-int who obj)
  (or (not obj) (and (words.signed-int? obj) (<= 0 obj)))
  (assertion-violation who "expected false or positive signed int as argument" obj))

(define-argument-validation (encoding-symbol who obj)
  (memq obj '(UTF-8 UTF-16 ISO-8859-1))
  (assertion-violation who "expected Expat encoding symbol as argument" obj))

(define-argument-validation (ascii-char who obj)
  (and (char? obj) (<= 0 (char->integer obj) 127))
  (assertion-violation who "expected Scheme character in the ASCII range as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (parser who obj)
  (ffi.pointer? obj)
  (assertion-violation who "expected pointer to Expat parser as argument" obj))


;;;; data structures

(define %parser-guardian
  (make-guardian))

(define (%free-allocated-guardian)
  (do ((P (%parser-guardian) (%parser-guardian)))
      ((not P))
    (foreign-call "ik_expat_parser_free" P)))

;;; --------------------------------------------------------------------

(define-struct parsing-status
  (parsing final-buffer?))

(define (%struct-parsing-status-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[expat-parsing-status")
  (%display " parsing=")	(%display (let ((status (parsing-status-parsing S)))
					    (cond ((= status XML_INITIALIZED)
						   "XML_INITIALIZED")
						  ((= status XML_PARSING)
						   "XML_PARSING")
						  ((= status XML_FINISHED)
						   "XML_FINISHED")
						  ((= status XML_SUSPENDED)
						   "XML_SUSPENDED")
						  (else "<unknown status>"))))
  (%display " final-buffer?=")	(%display (parsing-status-final-buffer? S))
  (%display "]"))


;;;; helpers

(define (%document-encoding-symbol->fixnum who encoding)
  (case encoding
    ((#f)		0) ;honour the encoding specified in the document
    ((UTF-8)		1)
    ((UTF-16)		2)
    ((ISO-8859-1)	3)
    ((US-ASCII)		4)
    (else
     (assertion-violation who "bad encoding selection" encoding))))


;;;; callback setters

(let-syntax ((declare (syntax-rules ()
			((_ ?func ?who)
			 (define (?who parser callback)
			   (define who '?who)
			   (with-arguments-validation (who)
			       ((parser		parser)
				(callback	callback))
			     (foreign-call ?func parser callback)))))))
  (declare "ik_expat_set_element_decl_handler"		XML_SetElementDeclHandler)
  (declare "ik_expat_set_attlist_decl_handler"		XML_SetAttlistDeclHandler)
  (declare "ik_expat_set_xml_decl_handler"		XML_SetXmlDeclHandler)
  (declare "ik_expat_set_entity_decl_handler"		XML_SetEntityDeclHandler)
  (declare "ik_expat_set_start_element_handler"		XML_SetStartElementHandler)
  (declare "ik_expat_set_end_element_handler"		XML_SetEndElementHandler)
  (declare "ik_expat_set_character_data_handler"	XML_SetCharacterDataHandler)
  (declare "ik_expat_set_processing_instruction_handler" XML_SetProcessingInstructionHandler)
  (declare "ik_expat_set_comment_handler"		XML_SetCommentHandler)
  (declare "ik_expat_set_start_cdata_section_handler"	XML_SetStartCdataSectionHandler)
  (declare "ik_expat_set_end_cdata_section_handler"	XML_SetEndCdataSectionHandler)
  (declare "ik_expat_set_default_handler"		XML_SetDefaultHandler)
  (declare "ik_expat_set_default_handler_expand"	XML_SetDefaultHandlerExpand)
  (declare "ik_expat_set_start_doctype_decl_handler"	XML_SetStartDoctypeDeclHandler)
  (declare "ik_expat_set_end_doctype_decl_handler"	XML_SetEndDoctypeDeclHandler)
  (declare "ik_expat_set_unparsed_entity_decl_handler"	XML_SetUnparsedEntityDeclHandler)
  (declare "ik_expat_set_notation_decl_handler"		XML_SetNotationDeclHandler)
  (declare "ik_expat_set_start_namespace_decl_handler"	XML_SetStartNamespaceDeclHandler)
  (declare "ik_expat_set_end_namespace_decl_handler"	XML_SetEndNamespaceDeclHandler)
  (declare "ik_expat_set_not_standalone_handler"	XML_SetNotStandaloneHandler)
  (declare "ik_expat_set_external_entity_ref_handler"	XML_SetExternalEntityRefHandler)
  (declare "ik_expat_set_skipped_entity_handler"	XML_SetSkippedEntityHandler))

(let-syntax ((declare (syntax-rules ()
			((_ ?func ?who)
			 (define (?who parser start-callback end-callback)
			   (define who '?who)
			   (with-arguments-validation (who)
			       ((parser		parser)
				(callback	start-callback)
				(callback	end-callback))
			     (foreign-call ?func parser start-callback end-callback)))))))
  (declare "ik_expat_set_element_handler"		XML_SetElementHandler)
  (declare "ik_expat_set_cdata_section_handler"		XML_SetCdataSectionHandler)
  (declare "ik_expat_set_doctype_decl_handler"		XML_SetDoctypeDeclHandler)
  (declare "ik_expat_set_namespace_decl_handler"	XML_SetNamespaceDeclHandler))

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

;;; --------------------------------------------------------------------

(define (XML_FreeContentModel parser model)
  (define who 'XML_FreeContentModel)
  (with-arguments-validation (who)
      ((parser		parser)
       (pointer		model))
    (foreign-call "ik_expat_free_content_model" parser model)))

(define (XML_UseParserAsHandlerArg parser)
  (define who 'XML_UseParserAsHandlerArg)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_use_parser_as_handler_arg" parser)))


;;;; callback makers

;; typedef void (XMLCALL *XML_ElementDeclHandler) (void *userData,
;;                                                 const XML_Char *name,
;;                                                 XML_Content *model);
(define XML_ElementDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer)))

;; typedef void (XMLCALL *XML_AttlistDeclHandler) (
;;                                     void            *userData,
;;                                     const XML_Char  *elname,
;;                                     const XML_Char  *attname,
;;                                     const XML_Char  *att_type,
;;                                     const XML_Char  *dflt,
;;                                     int              isrequired);
(define XML_AttlistDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_XmlDeclHandler) (void           *userData,
;;                                             const XML_Char *version,
;;                                             const XML_Char *encoding,
;;                                             int             standalone);
(define XML_XmlDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_StartElementHandler) (void *userData,
;;                                                  const XML_Char *name,
;;                                                  const XML_Char **atts);
(define XML_StartElementHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer)))

;; typedef void (XMLCALL *XML_EndElementHandler) (void *userData,
;;                                                const XML_Char *name);
(define XML_EndElementHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer)))

;; typedef void (XMLCALL *XML_CharacterDataHandler) (void *userData,
;;                                                   const XML_Char *s,
;;                                                   int len);
(define XML_CharacterDataHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_ProcessingInstructionHandler) (void *userData,
;; 							       const XML_Char *target,
;; 							       const XML_Char *data) ;
(define XML_ProcessingInstructionHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer)))

;; typedef void (XMLCALL *XML_CommentHandler) (void *userData,
;;                                             const XML_Char *data);
(define XML_CommentHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer)))

;; typedef void (XMLCALL *XML_StartCdataSectionHandler) (void *userData);
(define XML_StartCdataSectionHandler
  (ffi.make-c-callback-maker 'void '(pointer)))

;; typedef void (XMLCALL *XML_EndCdataSectionHandler) (void *userData);
(define XML_EndCdataSectionHandler
  (ffi.make-c-callback-maker 'void '(pointer)))

;; typedef void (XMLCALL *XML_DefaultHandler) (void *userData,
;;                                             const XML_Char *s,
;;                                             int len);
(define XML_DefaultHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_StartDoctypeDeclHandler) (
;;                                             void *userData,
;;                                             const XML_Char *doctypeName,
;;                                             const XML_Char *sysid,
;;                                             const XML_Char *pubid,
;;                                             int has_internal_subset);
(define XML_StartDoctypeDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_EndDoctypeDeclHandler)(void *userData);
(define XML_EndDoctypeDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer)))

;; typedef void (XMLCALL *XML_EntityDeclHandler) (
;;                               void *userData,
;;                               const XML_Char *entityName,
;;                               int is_parameter_entity,
;;                               const XML_Char *value,
;;                               int value_length,
;;                               const XML_Char *base,
;;                               const XML_Char *systemId,
;;                               const XML_Char *publicId,
;;                               const XML_Char *notationName);
(define XML_EntityDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer signed-int pointer
					     signed-int pointer pointer
					     pointer pointer)))

;; typedef void (XMLCALL *XML_UnparsedEntityDeclHandler) (
;;                                     void *userData,
;;                                     const XML_Char *entityName,
;;                                     const XML_Char *base,
;;                                     const XML_Char *systemId,
;;                                     const XML_Char *publicId,
;;                                     const XML_Char *notationName);
(define XML_UnparsedEntityDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer pointer pointer pointer)))

;; typedef void (XMLCALL *XML_NotationDeclHandler) (
;;                                     void *userData,
;;                                     const XML_Char *notationName,
;;                                     const XML_Char *base,
;;                                     const XML_Char *systemId,
;;                                     const XML_Char *publicId);
(define XML_NotationDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer pointer pointer)))

;; typedef void (XMLCALL *XML_StartNamespaceDeclHandler) (
;;                                     void *userData,
;;                                     const XML_Char *prefix,
;;                                     const XML_Char *uri);
(define XML_StartNamespaceDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer pointer)))

;; typedef void (XMLCALL *XML_EndNamespaceDeclHandler) (
;;                                     void *userData,
;;                                     const XML_Char *prefix);
(define XML_EndNamespaceDeclHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer)))

;; typedef int (XMLCALL *XML_NotStandaloneHandler) (void *userData);
(define XML_NotStandaloneHandler
  (ffi.make-c-callback-maker 'signed-int '(pointer)))

;; typedef int (XMLCALL *XML_ExternalEntityRefHandler) (
;;                                     XML_Parser parser,
;;                                     const XML_Char *context,
;;                                     const XML_Char *base,
;;                                     const XML_Char *systemId,
;;                                     const XML_Char *publicId);
(define XML_ExternalEntityRefHandler
  (ffi.make-c-callback-maker 'signed-int '(pointer pointer pointer pointer pointer)))

;; typedef void (XMLCALL *XML_SkippedEntityHandler) (
;;                                     void *userData,
;;                                     const XML_Char *entityName,
;;                                     int is_parameter_entity);
(define XML_SkippedEntityHandler
  (ffi.make-c-callback-maker 'void '(pointer pointer signed-int)))

;; typedef int (XMLCALL *XML_UnknownEncodingHandler) (
;;                                     void *encodingHandlerData,
;;                                     const XML_Char *name,
;;                                     XML_Encoding *info);
(define XML_UnknownEncodingHandler
  (ffi.make-c-callback-maker 'signed-int '(pointer pointer pointer)))


;;;; parsers

(define XML_ParserCreate
  (case-lambda
   (()
    (XML_ParserCreate #f))
   ((encoding)
    (define who 'XML_ParserCreate)
    (with-arguments-validation (who)
	((false/encoding-symbol	encoding))
      (let ((rv (foreign-call "ik_expat_parser_create"
			      (%document-encoding-symbol->fixnum who encoding))))
	(if rv
	    (%parser-guardian rv)
	  (error who "error allocating Expat parser" encoding)))))))

(define (XML_ParserCreateNS encoding namespace-separator)
  (define who 'XML_ParserCreateNS)
  (with-arguments-validation (who)
      ((false/encoding-symbol	encoding)
       (ascii-char		namespace-separator))
    (let ((rv (foreign-call "ik_expat_parser_create_ns"
			    (%document-encoding-symbol->fixnum who encoding)
			    (char->integer namespace-separator))))
      (if rv
	  (%parser-guardian rv)
	(error who "error allocating Expat parser" encoding namespace-separator)))))

(define (XML_ParserReset parser encoding)
  (define who 'XML_ParserReset)
  (with-arguments-validation (who)
      ((false/encoding-symbol	encoding))
    (foreign-call "ik_expat_parser_reset" parser encoding)))

(define (XML_ParserFree parser)
  (define who 'XML_ParserFree)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_parser_free" parser)))

;;; --------------------------------------------------------------------

(define (XML_ExternalEntityParserCreate parser context encoding)
  (define who 'XML_ExternalEntityParserCreate)
  (with-arguments-validation (who)
      ((parser		parser)
       (pointer		context)
       (encoding-symbol	encoding))
    (let ((rv (foreign-call "ik_expat_external_entity_parser_create" parser context
			    (%document-encoding-symbol->fixnum who encoding))))
      (if rv
	  (%parser-guardian rv)
	(error who "error allocating Expat entity parser" parser context encoding)))))

(define (XML_SetParamEntityParsing parser parsing)
  (define who 'XML_SetParamEntityParsing)
  (with-arguments-validation (who)
      ((parser	parser)
       (fixnum	parsing))
    (foreign-call "ik_expat_set_param_entity_parsing" parser parsing)))

;;; --------------------------------------------------------------------

(define (XML_SetEncoding parser encoding)
  (define who 'XML_SetEncoding)
  (with-arguments-validation (who)
      ((parser		parser)
       (encoding-symbol	encoding))
    (foreign-call "ik_expat_set_encoding" parser
		  (%document-encoding-symbol->fixnum who encoding))))

(define (XML_SetUserData parser pointer)
  (define who 'XML_SetUserData)
  (with-arguments-validation (who)
      ((parser	parser)
       (pointer	pointer))
    (foreign-call "ik_expat_set_user_data" parser pointer)))

(define (XML_GetUserData parser)
  (define who 'XML_GetUserData)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_user_data" parser)))

;;; --------------------------------------------------------------------

(define (XML_SetBase parser base)
  (define who 'XML_SetBase)
  (with-arguments-validation (who)
      ((parser			parser)
       (false/bytevector	base))
    (foreign-call "ik_expat_set_base" parser base)))

(define (XML_GetBase parser)
  (define who 'XML_GetBase)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_base" parser)))

;;; --------------------------------------------------------------------

(define (XML_UseForeignDTD parser use-dtd?)
  (define who 'XML_UseForeignDTD)
  (with-arguments-validation (who)
      ((parser	parser)
       (boolean	use-dtd?))
    (foreign-call "ik_expat_user_foreign_dtd" parser use-dtd?)))

(define (XML_SetReturnNSTriplet parser do-nst?)
  (define who 'XML_SetReturnNSTriplet)
  (with-arguments-validation (who)
      ((parser	parser)
       (boolean	do-nst?))
    (foreign-call "ik_expat_set_return_ns_triplet" parser do-nst?)))

(define (XML_Parse parser buffer buflen final?)
  (define who 'XML_Parse)
  (with-arguments-validation (who)
      ((parser				parser)
       (pointer/bytevector		buffer)
       (false/non-negative-signed-int	buflen)
       (boolean				final?))
    (foreign-call "ik_expat_parse" parser buffer buflen final?)))

(define (XML_GetBuffer parser buflen)
  (define who 'XML_GetBuffer)
  (with-arguments-validation (who)
      ((parser		parser)
       (signed-int	buflen))
    (foreign-call "ik_expat_get_buffer" parser buflen)))

(define (XML_ParseBuffer parser buflen final?)
  (define who 'XML_ParseBuffer)
  (with-arguments-validation (who)
      ((parser				parser)
       (false/non-negative-signed-int	buflen)
       (boolean				final?))
    (foreign-call "ik_expat_parse_buffer" parser buflen final?)))

;;; --------------------------------------------------------------------

(define (XML_StopParser parser resumable?)
  (define who 'XML_StopParser)
  (with-arguments-validation (who)
      ((parser	parser)
       (boolean	resumable?))
    (foreign-call "ik_expat_stop_parser" parser resumable?)))

(define (XML_ResumeParser parser)
  (define who 'XML_ResumeParser)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_resume_parser" parser)))

;;; --------------------------------------------------------------------

(define (XML_GetParsingStatus parser)
  (define who 'XML_GetParsingStatus)
  (with-arguments-validation (who)
      ((parser	parser))
    (let ((status (make-parsing-status #f #f)))
      (foreign-call "ik_expat_get_parsing_status" parser status)
      status)))

(define (XML_GetInputContext parser)
  (define who 'XML_GetInputContext)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_input_context" parser)))


;;;; elements

(define (XML_DefaultCurrent parser)
  (define who 'XML_DefaultCurrent)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_default_current" parser)))


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
      ((signed-int	code))
    (foreign-call "ik_expat_error_string" code)))

(define (XML_GetErrorCode parser)
  (define who 'XML_GetErrorCode)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_error_code" parser)))

(define (XML_GetCurrentLineNumber parser)
  (define who 'XML_GetCurrentLineNumber)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_current_line_number" parser)))

(define (XML_GetCurrentColumnNumber parser)
  (define who 'XML_GetCurrentColumnNumber)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_current_column_number" parser)))

(define (XML_GetCurrentByteIndex parser)
  (define who 'XML_GetCurrentByteIndex)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_current_byte_index" parser)))

(define (XML_GetCurrentByteCount parser)
  (define who 'XML_GetCurrentByteCount)
  (with-arguments-validation (who)
      ((parser	parser))
    (foreign-call "ik_expat_get_current_byte_count" parser)))


;;;; miscellaneous functions

(define (XML_ExpatVersion)
  (foreign-call "ik_expat_version"))

(define (XML_ExpatVersionInfo)
  (foreign-call "ik_expat_version_info"))

(define (XML_GetFeatureList)
  (foreign-call "ik_expat_get_feature_list"))


;;;; done

(set-rtd-printer! (type-descriptor parsing-status) %struct-parsing-status-printer)

)

;;; end of file
