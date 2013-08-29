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
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare xml expat)
  (export

    ;; version numbers and strings
    vicare-expat-version-interface-current
    vicare-expat-version-interface-revision
    vicare-expat-version-interface-age
    vicare-expat-version

;;; --------------------------------------------------------------------

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
  (import (vicare)
    (vicare unsafe operations)
    (vicare arguments validation)
    (vicare arguments general-c-buffers)
    (prefix (vicare ffi) ffi.)
    (prefix (vicare ffi foreign-pointer-wrapper) ffi.)
    (prefix (vicare platform words) words.)
    (vicare xml expat constants)
    (prefix (vicare xml expat unsafe-capi) capi.))


;;;; helpers

(define-constant EXPAT-SUPPORTED-ENCODING-SYMBOLS
  '(UTF-8 UTF-16 ISO-8859-1 US-ASCII))

(define (expat-supported-encoding-symbol? obj)
  (memq obj EXPAT-SUPPORTED-ENCODING-SYMBOLS))

(define (%document-encoding-symbol->fixnum who encoding)
  (if encoding
      (case encoding
	((UTF-8)	1)
	((UTF-16)	2)
	((ISO-8859-1)	3)
	((US-ASCII)	4)
	(else
	 (assertion-violation who
	   "invalid symbol as Expat built-in encoding name" encoding)))
    ;;Honour the encoding specified in the document.
    0))


;;;; arguments validation

(define-argument-validation (false/non-negative-signed-int who obj)
  (or (not obj) (and (words.signed-int? obj) (<= 0 obj)))
  (assertion-violation who "expected false or positive signed int as argument" obj))

(define-argument-validation (encoding-symbol who obj)
  (expat-supported-encoding-symbol? obj)
  (assertion-violation who "expected Expat encoding symbol as argument" obj))

(define-argument-validation (encoding-symbol-or-false who obj)
  (or (not obj) (expat-supported-encoding-symbol? obj))
  (assertion-violation who "expected false or Expat encoding symbol as argument" obj))


;;;; version functions

(define (vicare-expat-version-interface-current)
  (capi.vicare-expat-version-interface-current))

(define (vicare-expat-version-interface-revision)
  (capi.vicare-expat-version-interface-revision))

(define (vicare-expat-version-interface-age)
  (capi.vicare-expat-version-interface-age))

(define (vicare-expat-version)
  (ascii->string (capi.vicare-expat-version)))


;;;; data structures: XML_Parser

(ffi.define-foreign-pointer-wrapper XML_Parser
  (ffi.foreign-destructor capi.XML_ParserFree)
  (ffi.collector-struct-type #f))

(module ()
  (define (%struct-XML_Parser-printer S port sub-printer)
    (define-inline (%display thing)
      (display thing port))
    (%display "#[expat:XML_Parser")
    (%display "]"))

  (set-rtd-printer! (type-descriptor XML_Parser)	%struct-XML_Parser-printer))


;;;; data structures: XML_ParsingStatus

(define-struct XML_ParsingStatus
  (parsing final-buffer?))

(module ()
  (define (%struct-XML_ParsingStatus-printer S port sub-printer)
    (define-inline (%display thing)
      (display thing port))
    (%display "#[expat:XML_ParsingStatus")
    (%display " parsing=")		(%display (let ((status (XML_ParsingStatus-parsing S)))
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

  (set-rtd-printer! (type-descriptor XML_ParsingStatus) %struct-XML_ParsingStatus-printer))


;;;; data structures: XML_Content

(define-struct XML_Content
  (type quant name numchildren children))

(module ()
  (define (%struct-XML_Content-printer S port sub-printer)
    (define-inline (%display thing)
      (display thing port))
    (define-inline (%write thing)
      (write thing port))
    (%display "#[expat:XML_Content")
    (%display " type=")		(%display (let ((fx (XML_Content-type S)))
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
  (set-rtd-printer! (type-descriptor XML_Content) %struct-XML_Content-printer))

(define (pointer->XML_Content pointer)
  (define who 'pointer->XML_Content)
  (let* ((type		(capi.XML_Content.type pointer))
	 (quant		(capi.XML_Content.quant pointer))
	 (name		(let ((name (capi.XML_Content.name pointer)))
			  (if (ffi.pointer-null? name)
			      #f
			    (ffi.cstring->string name))))
	 (numchildren	(capi.XML_Content.numchildren pointer))
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
						 (capi.XML_Content.children pointer i)))))))))
    (make-XML_Content type quant name numchildren children)))

(define (XML_Content->list S)
  (list (XML_Content-type S)
	(XML_Content-quant S)
	(XML_Content-name S)
	(XML_Content-numchildren S)
	(let ((vec (XML_Content-children S)))
	  (if vec
	      (vector-map XML_Content->list vec)
	    #f))))


;;;; callback setters

(let-syntax ((declare (lambda (stx)
			(define (identifier-prefix prefix id)
			  (datum->syntax id (string-append prefix (symbol->string (syntax->datum id)))))
			(syntax-case stx ()
			  ((_ ?who)
			   (with-syntax
			       ((CAPI-FUNC (identifier-prefix "capi." #'?who)))
			     #'(define (?who parser callback)
				 (define who '?who)
				 (with-arguments-validation (who)
				     ((XML_Parser	parser)
				      (c-callback	callback))
				   (CAPI-FUNC parser callback)))))))))
  (declare XML_SetElementDeclHandler)
  (declare XML_SetAttlistDeclHandler)
  (declare XML_SetXmlDeclHandler)
  (declare XML_SetEntityDeclHandler)
  (declare XML_SetStartElementHandler)
  (declare XML_SetEndElementHandler)
  (declare XML_SetCharacterDataHandler)
  (declare XML_SetProcessingInstructionHandler)
  (declare XML_SetCommentHandler)
  (declare XML_SetStartCdataSectionHandler)
  (declare XML_SetEndCdataSectionHandler)
  (declare XML_SetDefaultHandler)
  (declare XML_SetDefaultHandlerExpand)
  (declare XML_SetStartDoctypeDeclHandler)
  (declare XML_SetEndDoctypeDeclHandler)
  (declare XML_SetUnparsedEntityDeclHandler)
  (declare XML_SetNotationDeclHandler)
  (declare XML_SetStartNamespaceDeclHandler)
  (declare XML_SetEndNamespaceDeclHandler)
  (declare XML_SetNotStandaloneHandler)
  (declare XML_SetExternalEntityRefHandler)
  (declare XML_SetSkippedEntityHandler))

(let-syntax ((declare (lambda (stx)
			(define (identifier-prefix prefix id)
			  (datum->syntax id (string-append prefix (symbol->string (syntax->datum id)))))
			(syntax-case stx ()
			  ((_ ?who)
			   (with-syntax
			       ((CAPI-FUNC (identifier-prefix "capi." #'?who)))
			     #'(define (?who parser start-callback end-callback)
				 (define who '?who)
				 (with-arguments-validation (who)
				     ((XML_Parser	parser)
				      (c-callback	start-callback)
				      (c-callback	end-callback))
				   (CAPI-FUNC parser start-callback end-callback)))))))))
  (declare XML_SetElementHandler)
  (declare XML_SetCdataSectionHandler)
  (declare XML_SetDoctypeDeclHandler)
  (declare XML_SetNamespaceDeclHandler))

;;; --------------------------------------------------------------------

(define (XML_SetExternalEntityRefHandlerArg parser pointer)
  (define who 'XML_SetExternalEntityRefHandlerArg)
  (with-arguments-validation (who)
      ((XML_Parser	parser)
       (pointer		pointer))
    (capi.XML_SetExternalEntityRefHandlerArg parser pointer)))

;;Not publicly interfaced.
;;
;; (define (XML_SetUnknownEncodingHandler parser callback pointer)
;;   (define who 'XML_SetUnknownEncodingHandler)
;;   (with-arguments-validation (who)
;;       ((XML_Parser		parser)
;;        (c-callback		callback)
;;        (pointer		pointer))
;;     (capi.XML_SetUnknownEncodingHandler parser callback pointer)))

;;; --------------------------------------------------------------------

(define (XML_FreeContentModel parser model)
  (define who 'XML_FreeContentModel)
  (with-arguments-validation (who)
      ((XML_Parser	parser)
       (pointer		model))
    (capi.XML_FreeContentModel parser model)))

(define (XML_UseParserAsHandlerArg parser)
  (define who 'XML_UseParserAsHandlerArg)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_UseParserAsHandlerArg parser)))


;;;; callback makers

;; typedef void (XMLCALL *XML_ElementDeclHandler) (void *userData,
;;                                                 const XML_Char *name,
;;                                                 XML_Content *model);
(define XML_ElementDeclHandler
  ;;FIXME  This  callback needs  a  pointer  to  parser to  release  the
  ;;XML_Content data structure.
  (let ((maker (ffi.make-c-callback-maker 'void '(pointer pointer pointer))))
    (lambda (user-scheme-callback)
      (maker (lambda (custom-data element-name c-model)
	       (guard (E (else
			  #;(pretty-print E (current-error-port))
			  (void)))
		 (let ((parser (make-XML_Parser/not-owner custom-data))
		       (model  (pointer->XML_Content c-model)))
		   (XML_FreeContentModel parser c-model)
		   (user-scheme-callback parser (cstring->string element-name) model)
		   (void))))))))

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

;;; interface not implemented
;;
;; typedef int (XMLCALL *XML_UnknownEncodingHandler) (
;;                                     void *encodingHandlerData,
;;                                     const XML_Char *name,
;;                                     XML_Encoding *info);
;; (define XML_UnknownEncodingHandler
;;   (ffi.make-c-callback-maker 'signed-int '(pointer pointer pointer)))


;;;; parsers

(define XML_ParserCreate
  (case-lambda
   (()
    (XML_ParserCreate #f))
   ((encoding)
    (define who 'XML_ParserCreate)
    (with-arguments-validation (who)
	((encoding-symbol-or-false	encoding))
      (let ((rv (capi.XML_ParserCreate (%document-encoding-symbol->fixnum who encoding))))
	(if rv
	    (make-XML_Parser/owner rv)
	  (error who "error allocating Expat parser" encoding)))))))

(define (XML_ParserCreateNS encoding namespace-separator)
  (define who 'XML_ParserCreateNS)
  (with-arguments-validation (who)
      ((encoding-symbol-or-false	encoding)
       (char-in-ascii-range		namespace-separator))
    (let ((rv (capi.XML_ParserCreateNS (%document-encoding-symbol->fixnum who encoding)
				       (char->integer namespace-separator))))
      (if rv
	  (make-XML_Parser/owner rv)
	(error who "error allocating Expat parser" encoding namespace-separator)))))

(define XML_ParserReset
  (case-lambda
   ((parser)
    (XML_ParserReset parser #f))
   ((parser encoding)
    (define who 'XML_ParserReset)
    (with-arguments-validation (who)
	((XML_Parser			parser)
	 (encoding-symbol-or-false	encoding))
      (capi.XML_ParserReset parser (%document-encoding-symbol->fixnum who encoding))))))

(define (XML_ParserFree parser)
  (define who 'XML_ParserFree)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    ($XML_Parser-finalise parser)))

;;; --------------------------------------------------------------------

(define (XML_ExternalEntityParserCreate parser context encoding)
  (define who 'XML_ExternalEntityParserCreate)
  (with-arguments-validation (who)
      ((XML_Parser	parser)
       (pointer		context)
       (encoding-symbol	encoding))
    (let ((rv (capi.XML_ExternalEntityParserCreate parser context
						   (%document-encoding-symbol->fixnum who encoding))))
      (if rv
	  (make-XML_Parser/owner rv)
	(error who "error allocating Expat entity parser" parser context encoding)))))

(define (XML_SetParamEntityParsing parser parsing)
  (define who 'XML_SetParamEntityParsing)
  (with-arguments-validation (who)
      ((XML_Parser	parser)
       (fixnum		parsing))
    (capi.XML_SetParamEntityParsing parser parsing)))

;;; --------------------------------------------------------------------

(define (XML_SetEncoding parser encoding)
  (define who 'XML_SetEncoding)
  (with-arguments-validation (who)
      ((XML_Parser		parser)
       (encoding-symbol		encoding))
    (capi.XML_SetEncoding parser (%document-encoding-symbol->fixnum who encoding))))

(define (XML_SetUserData parser pointer)
  (define who 'XML_SetUserData)
  (with-arguments-validation (who)
      ((XML_Parser	parser)
       (pointer		pointer))
    (capi.XML_SetUserData parser pointer)))

(define (XML_GetUserData parser)
  (define who 'XML_GetUserData)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetUserData parser)))

;;; --------------------------------------------------------------------

(define (XML_SetBase parser base)
  (define who 'XML_SetBase)
  (with-arguments-validation (who)
      ((XML_Parser		parser)
       (bytevector/false	base))
    (capi.XML_SetBase parser base)))

(define (XML_GetBase parser)
  (define who 'XML_GetBase)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetBase parser)))

;;; --------------------------------------------------------------------

(define (XML_UseForeignDTD parser use-dtd?)
  (define who 'XML_UseForeignDTD)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_UseForeignDTD parser use-dtd?)))

(define (XML_SetReturnNSTriplet parser do-nst?)
  (define who 'XML_SetReturnNSTriplet)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_SetReturnNSTriplet parser do-nst?)))

;;; --------------------------------------------------------------------

(define XML_Parse
  (case-lambda
   ((parser buf.data buf.len)
    (XML_Parse parser buf.data buf.len #f))
   ((parser buf.data buf.len final?)
    (define who 'XML_Parse)
    (with-arguments-validation (who)
	((XML_Parser		parser)
	 (general-c-string*	buf.data buf.len))
      (with-general-c-strings ((buf.data^ buf.data))
	(capi.XML_Parse parser buf.data^ buf.len final?))))))

(define (XML_GetBuffer parser buf.len)
  (define who 'XML_GetBuffer)
  (with-arguments-validation (who)
      ((XML_Parser	parser)
       (signed-int	buf.len))
    (capi.XML_GetBuffer parser buf.len)))

(define (XML_ParseBuffer parser buf.len final?)
  (define who 'XML_ParseBuffer)
  (with-arguments-validation (who)
      ((XML_Parser			parser)
       (false/non-negative-signed-int	buf.len))
    (capi.XML_ParseBuffer parser buf.len final?)))

;;; --------------------------------------------------------------------

(define (XML_StopParser parser resumable?)
  (define who 'XML_StopParser)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_StopParser parser resumable?)))

(define (XML_ResumeParser parser)
  (define who 'XML_ResumeParser)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_ResumeParser parser)))

;;; --------------------------------------------------------------------

(define (XML_GetParsingStatus parser)
  (define who 'XML_GetParsingStatus)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (receive-and-return (status)
	(make-XML_ParsingStatus #f #f)
      (capi.XML_GetParsingStatus parser status))))

(define (XML_GetInputContext parser)
  (define who 'XML_GetInputContext)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetInputContext parser)))


;;;; elements

(define (XML_DefaultCurrent parser)
  (define who 'XML_DefaultCurrent)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_DefaultCurrent parser)))


;;;; attributes

(define (XML_GetSpecifiedAttributeCount parser)
  (define who 'XML_GetSpecifiedAttributeCount)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetSpecifiedAttributeCount parser)))

(define (XML_GetIdAttributeIndex parser)
  (define who 'XML_GetIdAttributeIndex)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetIdAttributeIndex parser)))


;;;; error reporting

(define (XML_ErrorString code)
  (define who 'XML_ErrorString)
  (with-arguments-validation (who)
      ((signed-int	code))
    (capi.XML_ErrorString code)))

(define (XML_GetErrorCode parser)
  (define who 'XML_GetErrorCode)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetErrorCode parser)))

(define (XML_GetCurrentLineNumber parser)
  (define who 'XML_GetCurrentLineNumber)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetCurrentLineNumber parser)))

(define (XML_GetCurrentColumnNumber parser)
  (define who 'XML_GetCurrentColumnNumber)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetCurrentColumnNumber parser)))

(define (XML_GetCurrentByteIndex parser)
  (define who 'XML_GetCurrentByteIndex)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetCurrentByteIndex parser)))

(define (XML_GetCurrentByteCount parser)
  (define who 'XML_GetCurrentByteCount)
  (with-arguments-validation (who)
      ((XML_Parser	parser))
    (capi.XML_GetCurrentByteCount parser)))


;;;; miscellaneous functions

(define (XML_ExpatVersion)
  (latin1->string (capi.XML_ExpatVersion)))

(define (XML_ExpatVersionInfo)
  (capi.XML_ExpatVersionInfo))

(define (XML_GetFeatureList)
  (vector-map (lambda (vec)
		(vector-set! vec 1 (latin1->string (vector-ref vec 1)))
		vec)
    (capi.XML_GetFeatureList)))


;;;; done

)

;;; end of file
