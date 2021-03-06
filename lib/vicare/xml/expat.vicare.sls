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
;;;Copyright (C) 2012, 2013, 2015, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare xml expat)
  (options typed-language)
  (foreign-library "vicare-expat")
  (export

    ;; version numbers and strings
    vicare-expat-version-interface-current
    vicare-expat-version-interface-revision
    vicare-expat-version-interface-age
    vicare-expat-version

    ;;
    XML_ParserCreate
    XML_ParserCreateNS
;;;XML_ParserCreate_MM
    XML_ParserReset
    XML_DefaultCurrent
    XML_SetReturnNSTriplet
    XML_SetUserData
    XML_GetUserData
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
    XML_ExpatVersionInfo
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
;;;XML_UnknownEncodingHandler	;interface not implemented
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
;;;XML_UnknownEncodingHandler	;interface not implemented
    XML_UnparsedEntityDeclHandler
    XML_XmlDeclHandler

    ;; parsing status object
    make-XML_ParsingStatus		XML_ParsingStatus?
    XML_ParsingStatus-parsing		XML_ParsingStatus-final-buffer?

    ;; DTD element declaration model structure
    make-XML_Content			XML_Content?
    pointer->XML_Content		XML_Content->list
    XML_Content-type			XML_Content-quant
    XML_Content-name			XML_Content-numchildren
    XML_Content-children)
  (import (vicare (0 4 2017 1 (>= 10)))
    (prefix (vicare system structs) structs::)
    (vicare xml expat constants)
    (prefix (vicare xml expat unsafe-capi) capi::)
    (vicare arguments validation)
    (prefix (vicare ffi (or (0 4 2015 5 (>= 27))
			    (0 4 2015 (>= 6))
			    (0 4 (>= 2016))))
	    ffi::)
    (prefix (vicare platform words) words::))


;;;; arguments validation

(define (expat-xml-parser? obj)
  (ffi::pointer? obj))

(define (expat-encoding-symbol? obj)
  (case obj
    ((UTF-8 UTF-16 ISO-8859-1 US-ASCII)		#t)
    (else					#f)))

(define (false-or-expat-encoding-symbol? obj)
  (or (not obj)
      (expat-encoding-symbol? obj)))

(define (ascii-char? obj)
  (and (char? obj)
       (<= 0 (char->integer obj) 127)))

(define (false-or-non-negative-signed-int? obj)
  (or (not obj)
      (and (words::signed-int? obj)
	   (non-negative? obj))))

;;; --------------------------------------------------------------------

(define-argument-validation (callback who obj)
  (ffi::pointer? obj)
  (assertion-violation who "expected callback as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (false/bytevector who obj)
  (or (not obj) (bytevector? obj))
  (assertion-violation who "expected false or bytevector as argument" obj))

(define-argument-validation (pointer/bytevector who obj)
  (or (ffi::pointer? obj) (bytevector? obj))
  (assertion-violation who "expected pointer or bytevector as argument" obj))

(define-argument-validation (false/non-negative-signed-int who obj)
  (or (not obj) (and (words::signed-int? obj) (<= 0 obj)))
  (assertion-violation who "expected false or positive signed int as argument" obj))

(define-argument-validation (encoding-symbol who obj)
  (memq obj '(UTF-8 UTF-16 ISO-8859-1 US-ASCII))
  (assertion-violation who "expected Expat encoding symbol as argument" obj))

(define-argument-validation (false/encoding-symbol who obj)
  (or (not obj) (memq obj '(UTF-8 UTF-16 ISO-8859-1 US-ASCII)))
  (assertion-violation who "expected false or Expat encoding symbol as argument" obj))

(define-argument-validation (ascii-char who obj)
  (and (char? obj) (<= 0 (char->integer obj) 127))
  (assertion-violation who "expected Scheme character in the ASCII range as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (parser who obj)
  (ffi::pointer? obj)
  (assertion-violation who "expected pointer to Expat parser as argument" obj))


;;;; version functions

(define (vicare-expat-version-interface-current)
  (capi::vicare-expat-version-interface-current))

(define (vicare-expat-version-interface-revision)
  (capi::vicare-expat-version-interface-revision))

(define (vicare-expat-version-interface-age)
  (capi::vicare-expat-version-interface-age))

(define (vicare-expat-version)
  (ascii->string (capi::vicare-expat-version)))


;;;; data structures

(define %parser-guardian
  (make-guardian))

(define (%free-allocated-parser)
  (do ((P (%parser-guardian) (%parser-guardian)))
      ((not P))
    (foreign-call "ik_expat_parser_free" P)))

;;; --------------------------------------------------------------------

(structs::define-struct XML_ParsingStatus
  (parsing final-buffer?))

(define (%struct-XML_ParsingStatus-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[expat:XML_ParsingStatus")
  (%display " parsing=")	(%display (let ((status (XML_ParsingStatus-parsing S)))
					    (cond ((= status XML_INITIALIZED)
						   "XML_INITIALIZED")
						  ((= status XML_PARSING)
						   "XML_PARSING")
						  ((= status XML_FINISHED)
						   "XML_FINISHED")
						  ((= status XML_SUSPENDED)
						   "XML_SUSPENDED")
						  (else status))))
  (%display " final-buffer?=")	(%display (XML_ParsingStatus-final-buffer? S))
  (%display "]"))

;;; --------------------------------------------------------------------

(structs::define-struct XML_Content
  (type quant name numchildren children))

(define (%struct-XML_Content-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (define-inline (%write thing)
    (write thing port))
  (%display "#[expat:XML_Content")
  (%display " type=")	(%display (let ((fx (XML_Content-type S)))
				    (cond ((fx=? fx XML_CTYPE_EMPTY)
					   "XML_CTYPE_EMPTY")
					  ((fx=? fx XML_CTYPE_ANY)
					   "XML_CTYPE_ANY")
					  ((fx=? fx XML_CTYPE_MIXED)
					   "XML_CTYPE_MIXED")
					  ((fx=? fx XML_CTYPE_NAME)
					   "XML_CTYPE_NAME")
					  ((fx=? fx XML_CTYPE_CHOICE)
					   "XML_CTYPE_CHOICE")
					  ((fx=? fx XML_CTYPE_SEQ)
					   "XML_CTYPE_SEQ")
					  (else
					   "<unknown>"))))
  (%display " quant=")	(%display (let ((fx (XML_Content-quant S)))
				    (cond ((fx=? fx XML_CQUANT_NONE)
					   "XML_CQUANT_NONE")
					  ((fx=? fx XML_CQUANT_OPT)
					   "XML_CQUANT_OPT")
					  ((fx=? fx XML_CQUANT_REP)
					   "XML_CQUANT_REP")
					  ((fx=? fx XML_CQUANT_PLUS)
					   "XML_CQUANT_PLUS")
					  (else
					   "<unknown>"))))
  (%display " name=")		(%write (XML_Content-name S))
  (%display " numchildren=")	(%display (XML_Content-numchildren S))
  (%display " children=")	(%display (XML_Content-children S))
  (%display "]"))

(define (pointer->XML_Content pointer)
  (define who 'pointer->XML_Content)
  (let* ((type		(XML_Content.type pointer))
	 (quant		(XML_Content.quant pointer))
	 (name		(let ((name (XML_Content.name pointer)))
			  (if (ffi::pointer-null? name)
			      #f
			    (ffi::cstring->string name))))
	 (numchildren	(XML_Content.numchildren pointer))
	 (children	(cond ((bignum? numchildren)
			       (assertion-violation who
				 "number of child elements too big" numchildren))
			      ((fxzero? numchildren)
			       #f)
			      (else
			       (let ((children (make-vector numchildren)))
				 (do ((i 0 (fx+ 1 i)))
				     ((= i numchildren)
				      children)
				   (vector-set! children i
						(pointer->XML_Content
						 (XML_Content.children pointer i)))))))))
    (make-XML_Content type quant name numchildren children)))

(define ({XML_Content->list <list>} S)
  (list (XML_Content-type S)
	(XML_Content-quant S)
	(XML_Content-name S)
	(XML_Content-numchildren S)
	(let ((vec (XML_Content-children S)))
	  (if vec
	      (vector-map XML_Content->list vec)
	    #f))))

(define-inline (XML_Content.type pointer)
  (foreign-call "ik_expat_xml_content_type_ref" pointer))

(define-inline (XML_Content.quant pointer)
  (foreign-call "ik_expat_xml_content_quant_ref" pointer))

(define-inline (XML_Content.name pointer)
  (foreign-call "ik_expat_xml_content_name_ref" pointer))

(define-inline (XML_Content.numchildren pointer)
  (foreign-call "ik_expat_xml_content_numchildren_ref" pointer))

(define-inline (XML_Content.children pointer index)
  (foreign-call "ik_expat_xml_content_children_ref" pointer index))


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

(let-syntax
    ((declare (syntax-rules ()
		((_ ?func ?who)
		 (define* (?who {parser expat-xml-parser?} {callback ffi::c-callback?})
		   (foreign-call ?func parser callback)))
		)))
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

(let-syntax
    ((declare (syntax-rules ()
		((_ ?func ?who)
		 (define* (?who {parser expat-xml-parser?} {start-callback ffi::c-callback?} {end-callback ffi::c-callback?})
		   (foreign-call ?func parser start-callback end-callback)))
		)))
  (declare "ik_expat_set_element_handler"		XML_SetElementHandler)
  (declare "ik_expat_set_cdata_section_handler"		XML_SetCdataSectionHandler)
  (declare "ik_expat_set_doctype_decl_handler"		XML_SetDoctypeDeclHandler)
  (declare "ik_expat_set_namespace_decl_handler"	XML_SetNamespaceDeclHandler))

;;; --------------------------------------------------------------------

(define* (XML_SetExternalEntityRefHandlerArg {parser expat-xml-parser?} {pointer pointer?})
  (foreign-call "ik_expat_set_external_entity_ref_handler_arg" parser pointer))

;; (define* (XML_SetUnknownEncodingHandler {parser expat-xml-parser?} {callback ffi::c-callback?} {pointer pointer?})
;;   (foreign-call "ik_expat_set_unknown_encoding_handler" parser callback pointer))

;;; --------------------------------------------------------------------

(define* (XML_FreeContentModel {parser expat-xml-parser?} {model pointer?})
  (foreign-call "ik_expat_free_content_model" parser model))

(define* (XML_UseParserAsHandlerArg {parser expat-xml-parser?})
  (foreign-call "ik_expat_use_parser_as_handler_arg" parser))


;;;; callback makers

;; typedef void (XMLCALL *XML_ElementDeclHandler) (void *userData,
;;                                                 const XML_Char *name,
;;                                                 XML_Content *model);
(define XML_ElementDeclHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer)))

;; typedef void (XMLCALL *XML_AttlistDeclHandler) (
;;                                     void            *userData,
;;                                     const XML_Char  *elname,
;;                                     const XML_Char  *attname,
;;                                     const XML_Char  *att_type,
;;                                     const XML_Char  *dflt,
;;                                     int              isrequired);
(define XML_AttlistDeclHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_XmlDeclHandler) (void           *userData,
;;                                             const XML_Char *version,
;;                                             const XML_Char *encoding,
;;                                             int             standalone);
(define XML_XmlDeclHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_StartElementHandler) (void *userData,
;;                                                  const XML_Char *name,
;;                                                  const XML_Char **atts);
(define XML_StartElementHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer)))

;; typedef void (XMLCALL *XML_EndElementHandler) (void *userData,
;;                                                const XML_Char *name);
(define XML_EndElementHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer)))

;; typedef void (XMLCALL *XML_CharacterDataHandler) (void *userData,
;;                                                   const XML_Char *s,
;;                                                   int len);
(define XML_CharacterDataHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_ProcessingInstructionHandler) (void *userData,
;; 							       const XML_Char *target,
;; 							       const XML_Char *data) ;
(define XML_ProcessingInstructionHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer)))

;; typedef void (XMLCALL *XML_CommentHandler) (void *userData,
;;                                             const XML_Char *data);
(define XML_CommentHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer)))

;; typedef void (XMLCALL *XML_StartCdataSectionHandler) (void *userData);
(define XML_StartCdataSectionHandler
  (ffi::make-c-callback-maker 'void '(pointer)))

;; typedef void (XMLCALL *XML_EndCdataSectionHandler) (void *userData);
(define XML_EndCdataSectionHandler
  (ffi::make-c-callback-maker 'void '(pointer)))

;; typedef void (XMLCALL *XML_DefaultHandler) (void *userData,
;;                                             const XML_Char *s,
;;                                             int len);
(define XML_DefaultHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_StartDoctypeDeclHandler) (
;;                                             void *userData,
;;                                             const XML_Char *doctypeName,
;;                                             const XML_Char *sysid,
;;                                             const XML_Char *pubid,
;;                                             int has_internal_subset);
(define XML_StartDoctypeDeclHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer pointer signed-int)))

;; typedef void (XMLCALL *XML_EndDoctypeDeclHandler)(void *userData);
(define XML_EndDoctypeDeclHandler
  (ffi::make-c-callback-maker 'void '(pointer)))

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
  (ffi::make-c-callback-maker 'void '(pointer pointer signed-int pointer
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
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer pointer pointer pointer)))

;; typedef void (XMLCALL *XML_NotationDeclHandler) (
;;                                     void *userData,
;;                                     const XML_Char *notationName,
;;                                     const XML_Char *base,
;;                                     const XML_Char *systemId,
;;                                     const XML_Char *publicId);
(define XML_NotationDeclHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer pointer pointer)))

;; typedef void (XMLCALL *XML_StartNamespaceDeclHandler) (
;;                                     void *userData,
;;                                     const XML_Char *prefix,
;;                                     const XML_Char *uri);
(define XML_StartNamespaceDeclHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer pointer)))

;; typedef void (XMLCALL *XML_EndNamespaceDeclHandler) (
;;                                     void *userData,
;;                                     const XML_Char *prefix);
(define XML_EndNamespaceDeclHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer)))

;; typedef int (XMLCALL *XML_NotStandaloneHandler) (void *userData);
(define XML_NotStandaloneHandler
  (ffi::make-c-callback-maker 'signed-int '(pointer)))

;; typedef int (XMLCALL *XML_ExternalEntityRefHandler) (
;;                                     XML_Parser parser,
;;                                     const XML_Char *context,
;;                                     const XML_Char *base,
;;                                     const XML_Char *systemId,
;;                                     const XML_Char *publicId);
(define XML_ExternalEntityRefHandler
  (ffi::make-c-callback-maker 'signed-int '(pointer pointer pointer pointer pointer)))

;; typedef void (XMLCALL *XML_SkippedEntityHandler) (
;;                                     void *userData,
;;                                     const XML_Char *entityName,
;;                                     int is_parameter_entity);
(define XML_SkippedEntityHandler
  (ffi::make-c-callback-maker 'void '(pointer pointer signed-int)))

;;; interface not implemented
;;
;; typedef int (XMLCALL *XML_UnknownEncodingHandler) (
;;                                     void *encodingHandlerData,
;;                                     const XML_Char *name,
;;                                     XML_Encoding *info);
;; (define XML_UnknownEncodingHandler
;;   (ffi::make-c-callback-maker 'signed-int '(pointer pointer pointer)))


;;;; parsers

(case-define* XML_ParserCreate
  (()
   (XML_ParserCreate #f))
  (({encoding false-or-expat-encoding-symbol?})
   (let ((rv (foreign-call "ik_expat_parser_create"
			   (%document-encoding-symbol->fixnum __who__ encoding))))
     (if rv
	 (%parser-guardian rv)
       (error __who__ "error allocating Expat parser" encoding)))))

(define* (XML_ParserCreateNS {encoding false-or-expat-encoding-symbol?} {namespace-separator ascii-char?})
  (let ((rv (foreign-call "ik_expat_parser_create_ns"
			  (%document-encoding-symbol->fixnum __who__ encoding)
			  (char->integer namespace-separator))))
    (if rv
	(%parser-guardian rv)
      (error __who__ "error allocating Expat parser" encoding namespace-separator))))

(case-define* XML_ParserReset
  ((parser)
   (XML_ParserReset parser #f))
  (({parser expat-xml-parser?} {encoding false-or-expat-encoding-symbol?})
   (foreign-call "ik_expat_parser_reset" parser encoding)))

(define* (XML_ParserFree {parser expat-xml-parser?})
  (foreign-call "ik_expat_parser_free" parser))

;;; --------------------------------------------------------------------

(define* (XML_ExternalEntityParserCreate {parser expat-xml-parser?} {context pointer?} {encoding false-or-expat-encoding-symbol?})
  (let ((rv (foreign-call "ik_expat_external_entity_parser_create" parser context
			  (%document-encoding-symbol->fixnum __who__ encoding))))
    (if rv
	(%parser-guardian rv)
      (error __who__ "error allocating Expat entity parser" parser context encoding))))

(define* (XML_SetParamEntityParsing {parser expat-xml-parser?} {parsing fixnum?})
  (foreign-call "ik_expat_set_param_entity_parsing" parser parsing))

;;; --------------------------------------------------------------------

(define* (XML_SetEncoding {parser expat-xml-parser?} {encoding expat-encoding-symbol?})
  (foreign-call "ik_expat_set_encoding" parser
		(%document-encoding-symbol->fixnum __who__ encoding)))

(define* (XML_SetUserData {parser expat-xml-parser?} {pointer pointer?})
  (foreign-call "ik_expat_set_user_data" parser pointer))

(define* (XML_GetUserData {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_user_data" parser))

;;; --------------------------------------------------------------------

(define* (XML_SetBase {parser expat-xml-parser?} {base (or not bytevector?)})
  (foreign-call "ik_expat_set_base" parser base))

(define* (XML_GetBase {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_base" parser))

;;; --------------------------------------------------------------------

(define* (XML_UseForeignDTD {parser expat-xml-parser?} use-dtd?)
  (foreign-call "ik_expat_user_foreign_dtd" parser use-dtd?))

(define* (XML_SetReturnNSTriplet {parser expat-xml-parser?} do-nst?)
  (foreign-call "ik_expat_set_return_ns_triplet" parser do-nst?))

;;; --------------------------------------------------------------------

(case-define* XML_Parse
  ((parser buffer buflen)
   (XML_Parse parser buffer buflen #f))
  (({parser expat-xml-parser?} {buffer (or pointer? bytevector?)} {buflen false-or-non-negative-signed-int?} final?)
   (foreign-call "ik_expat_parse" parser buffer buflen final?)))

(define* (XML_GetBuffer {parser expat-xml-parser?} {buflen words::signed-int?})
  (foreign-call "ik_expat_get_buffer" parser buflen))

(define* (XML_ParseBuffer {parser expat-xml-parser?} {buflen false-or-non-negative-signed-int?} final?)
  (foreign-call "ik_expat_parse_buffer" parser buflen final?))

;;; --------------------------------------------------------------------

(define* (XML_StopParser {parser expat-xml-parser?} resumable?)
  (foreign-call "ik_expat_stop_parser" parser resumable?))

(define* (XML_ResumeParser {parser expat-xml-parser?})
  (foreign-call "ik_expat_resume_parser" parser))

;;; --------------------------------------------------------------------

(define* (XML_GetParsingStatus {parser expat-xml-parser?})
  (receive-and-return (status)
      (make-XML_ParsingStatus #f #f)
    (foreign-call "ik_expat_get_parsing_status" parser status)))

(define* (XML_GetInputContext {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_input_context" parser))


;;;; elements

(define* (XML_DefaultCurrent {parser expat-xml-parser?})
  (foreign-call "ik_expat_default_current" parser))


;;;; attributes

(define* (XML_GetSpecifiedAttributeCount {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_specified_attribute_count" parser))

(define* (XML_GetIdAttributeIndex {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_id_attribute_index" parser))


;;;; error reporting

(define* (XML_ErrorString {code words::signed-int?})
  (latin1->string (foreign-call "ik_expat_error_string" code)))

(define* (XML_GetErrorCode {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_error_code" parser))

(define* (XML_GetCurrentLineNumber {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_current_line_number" parser))

(define* (XML_GetCurrentColumnNumber {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_current_column_number" parser))

(define* (XML_GetCurrentByteIndex {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_current_byte_index" parser))

(define* (XML_GetCurrentByteCount {parser expat-xml-parser?})
  (foreign-call "ik_expat_get_current_byte_count" parser))


;;;; miscellaneous functions

(define (XML_ExpatVersion)
  (latin1->string (foreign-call "ik_expat_version")))

(define (XML_ExpatVersionInfo)
  (foreign-call "ik_expat_version_info"))

(define (XML_GetFeatureList)
  (vector-map (lambda (vec)
		(vector-set! vec 1 (latin1->string (vector-ref vec 1)))
		vec)
    (foreign-call "ik_expat_get_feature_list")))


;;;; done

(structs::set-struct-type-printer! (type-descriptor XML_ParsingStatus) %struct-XML_ParsingStatus-printer)
(structs::set-struct-type-printer! (type-descriptor XML_Content)       %struct-XML_Content-printer)

(post-gc-hooks (cons %free-allocated-parser
		     (post-gc-hooks)))

#| end of library |# )

;;; end of file
