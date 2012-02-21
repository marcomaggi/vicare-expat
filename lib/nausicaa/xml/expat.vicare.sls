;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Expat
;;;Contents: Nausicaa front end to (vicare expat)
;;;Date: Fri Feb 17, 2012
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


#!r6rs
(library (nausicaa xml expat)
  (export
    <expat-parser>
    <expat-ns-parser>
    <expat-entity-parser>
    <expat-parsing-status>
    encoding:
    namespace-separator:
    current-expat-parser

    &expat-error

    ;; Preprocessor symbols: XML_Bool
    XML_TRUE
    XML_FALSE

    ;; enum XML_Status
    XML_STATUS_ERROR
    XML_STATUS_OK
    XML_STATUS_SUSPENDED

    ;; enum XML_Error
    XML_ERROR_NONE
    XML_ERROR_NO_MEMORY
    XML_ERROR_SYNTAX
    XML_ERROR_NO_ELEMENTS
    XML_ERROR_INVALID_TOKEN
    XML_ERROR_UNCLOSED_TOKEN
    XML_ERROR_PARTIAL_CHAR
    XML_ERROR_TAG_MISMATCH
    XML_ERROR_DUPLICATE_ATTRIBUTE
    XML_ERROR_JUNK_AFTER_DOC_ELEMENT
    XML_ERROR_PARAM_ENTITY_REF
    XML_ERROR_UNDEFINED_ENTITY
    XML_ERROR_RECURSIVE_ENTITY_REF
    XML_ERROR_ASYNC_ENTITY
    XML_ERROR_BAD_CHAR_REF
    XML_ERROR_BINARY_ENTITY_REF
    XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF
    XML_ERROR_MISPLACED_XML_PI
    XML_ERROR_UNKNOWN_ENCODING
    XML_ERROR_INCORRECT_ENCODING
    XML_ERROR_UNCLOSED_CDATA_SECTION
    XML_ERROR_EXTERNAL_ENTITY_HANDLING
    XML_ERROR_NOT_STANDALONE
    XML_ERROR_UNEXPECTED_STATE
    XML_ERROR_ENTITY_DECLARED_IN_PE
    XML_ERROR_FEATURE_REQUIRES_XML_DTD
    XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING
    XML_ERROR_UNBOUND_PREFIX
    XML_ERROR_UNDECLARING_PREFIX
    XML_ERROR_INCOMPLETE_PE
    XML_ERROR_XML_DECL
    XML_ERROR_TEXT_DECL
    XML_ERROR_PUBLICID
    XML_ERROR_SUSPENDED
    XML_ERROR_NOT_SUSPENDED
    XML_ERROR_ABORTED
    XML_ERROR_FINISHED
    XML_ERROR_SUSPEND_PE
    XML_ERROR_RESERVED_PREFIX_XML
    XML_ERROR_RESERVED_PREFIX_XMLNS
    XML_ERROR_RESERVED_NAMESPACE_URI

    ;; enum XML_Content_Type
    XML_CTYPE_EMPTY
    XML_CTYPE_ANY
    XML_CTYPE_MIXED
    XML_CTYPE_NAME
    XML_CTYPE_CHOICE
    XML_CTYPE_SEQ

    ;; enum XML_Content_Quant
    XML_CQUANT_NONE
    XML_CQUANT_OPT
    XML_CQUANT_REP
    XML_CQUANT_PLUS

    ;; enum XML_Parsing
    XML_INITIALIZED
    XML_PARSING
    XML_FINISHED
    XML_SUSPENDED

    ;; enum XML_ParamEntityParsing
    XML_PARAM_ENTITY_PARSING_NEVER
    XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE
    XML_PARAM_ENTITY_PARSING_ALWAYS

    ;; Preprocessor symbols: version numbers
    XML_MAJOR_VERSION
    XML_MINOR_VERSION
    XML_MICRO_VERSION

    ;; enum XML_FeatureEnum
    XML_FEATURE_END
    XML_FEATURE_UNICODE
    XML_FEATURE_UNICODE_WCHAR_T
    XML_FEATURE_DTD
    XML_FEATURE_CONTEXT_BYTES
    XML_FEATURE_MIN_SIZE
    XML_FEATURE_SIZEOF_XML_CHAR
    XML_FEATURE_SIZEOF_XML_LCHAR
    XML_FEATURE_NS
    XML_FEATURE_LARGE_SIZE
    )
  (import (nausicaa)
    (prefix (vicare expat) expat.)
    (vicare expat constants)
    (prefix (vicare ffi) ffi.)
    (prefix (vicare unsafe-operations) unsafe.))


;;;; auxiliary definitions

(define-inline (%clean-callback field)
  (let ((cb field))
    (when cb
      (ffi.free-c-callback cb))))

(define-finaliser parser-finaliser
  (lambda ((P <expat-parser>))
    ;;Notice  that  we do  not  touch the  parser  pointer  here: it  is
    ;;finalised by the guardian in (vicare expat).
    ;;
    (%clean-callback P.AttlistDeclHandler)
    (%clean-callback P.CharacterDataHandler)
    (%clean-callback P.CommentHandler)
    (%clean-callback P.DefaultHandler)
    (%clean-callback P.ElementDeclHandler)
    (%clean-callback P.EndCdataSectionHandler)
    (%clean-callback P.EndDoctypeDeclHandler)
    (%clean-callback P.EndElementHandler)
    (%clean-callback P.EndNamespaceDeclHandler)
    (%clean-callback P.EntityDeclHandler)
    (%clean-callback P.ExternalEntityRefHandler)
    (%clean-callback P.NotStandaloneHandler)
    (%clean-callback P.NotationDeclHandler)
    (%clean-callback P.ProcessingInstructionHandler)
    (%clean-callback P.SkippedEntityHandler)
    (%clean-callback P.StartCdataSectionHandler)
    (%clean-callback P.StartDoctypeDeclHandler)
    (%clean-callback P.StartElementHandler)
    (%clean-callback P.StartNamespaceDeclHandler)
    (%clean-callback P.UnparsedEntityDeclHandler)
    (%clean-callback P.XmlDeclHandler)))

(define-auxiliary-syntaxes
  encoding:
  namespace-separator:)

(define current-expat-parser
  (make-parameter #f
    (lambda (obj)
      (assert (is-a? obj <expat-parser>))
      obj)))

(define-condition &expat-error
  (parent &error)
  (fields code))

(define-inline (%handle-status-code who code)
  (if (unsafe.fx= code XML_STATUS_ERROR)
      (raise (condition (make-who-condition who)
			(make-message-condition (expat.XML_ErrorString code))
			(make &expat-error code)))
    code))

(define-syntax %raise-expat-error
  (syntax-rules ()
    ((_ ?who ?message ?irritants)
     (raise (condition (make-who-condition ?who)
		       (make-message-condition ?message)
		       (make-irritants-condition ?irritants)
		       (make &expat-error XML_ERROR_NONE))))
    ((_ ?who ?message ?irritants ?code)
     (raise (condition (make-who-condition ?who)
		       (make-message-condition ?message)
		       (make-irritants-condition ?irritants)
		       (make &expat-error ?code))))))


(define-class <expat-parser>
  (nongenerative nausicaa:xml:expat:<expat-parser>)
  (fields (immutable	parser)	  ;pointer to parser
	  (mutable	started?) ;boolean

	  ;; all of the following are #f or C callback pointers
	  (mutable AttlistDeclHandler)
	  (mutable CharacterDataHandler)
	  (mutable CommentHandler)
	  (mutable DefaultHandler)
	  (mutable ElementDeclHandler)
	  (mutable EndCdataSectionHandler)
	  (mutable EndDoctypeDeclHandler)
	  (mutable EndElementHandler)
	  (mutable EndNamespaceDeclHandler)
	  (mutable EntityDeclHandler)
	  (mutable ExternalEntityRefHandler)
	  (mutable NotStandaloneHandler)
	  (mutable NotationDeclHandler)
	  (mutable ProcessingInstructionHandler)
	  (mutable SkippedEntityHandler)
	  (mutable StartCdataSectionHandler)
	  (mutable StartDoctypeDeclHandler)
	  (mutable StartElementHandler)
	  (mutable StartNamespaceDeclHandler)
	  (mutable UnparsedEntityDeclHandler)
	  (mutable XmlDeclHandler))

  (maker ()
	 (encoding: #f))

  (protocol
   (lambda (make-top)
     (lambda (encoding)
       (let ((parser (expat.XML_ParserCreate encoding)))
	 (if parser
	     (parser-finaliser ((make-top) parser #f
				#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
	   (%raise-expat-error '<expat-parser>
	     "error building Expat parser" (list encoding)))))))

  (superclass-protocol
   (lambda (make-top)
     (lambda (parser)
       (parser-finaliser ((make-top) parser #f
			  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))))

;;; --------------------------------------------------------------------

  (method reset
    (case-lambda
     (((P <expat-parser>))
      (expat.XML_ParserReset P.parser #f))
     (((P <expat-parser>) encoding)
      (expat.XML_ParserReset P.parser encoding))))

  (method (set-encoding (P <expat-parser>) encoding)
    (define who '<expat-parser>.set-encoding)
    (if P.started?
	(%raise-expat-error who
	  "invalid operation on Expat parser already processing" (list P encoding))
      (%handle-status-code who
	(expat.XML_SetEncoding P.parser encoding))))

  (method (parse (P <expat-parser>) buffer buflen final?)
    (set! P.started? #t)
    (parametrise ((current-expat-parser P))
      (expat.XML_Parse P.parser buffer buflen final?)))

  (method (get-buffer (P <expat-parser>) buflen)
    (or (expat.XML_GetBuffer P.parser buflen)
	(%raise-expat-error '<expat-parser>.get-buffer
	  "error allocating buffer of requested length" (list P buflen))))

  (method (parse-buffer (P <expat-parser>) buflen final?)
    (set! P.started? #t)
    (parametrise ((current-expat-parser P))
      (expat.XML_ParseBuffer P.parser buflen final?)))

  (method (stop-parser (P <expat-parser>) resumable?)
    (expat.XML_StopParser P.parser resumable?))

  (method (resume-parser (P <expat-parser>))
    (parametrise ((current-expat-parser P))
      (expat.XML_ResumeParser P.parser)))

  (method (set-user-data (P <expat-parser>) data)
    (expat.XML_SetUserData P.parser data))

  (method (get-user-data (P <expat-parser>))
    (expat.XML_GetUserData P.parser))

  (method (use-parser-as-handler-arg (P <expat-parser>))
    (expat.XML_UseParserAsHandlerArg P.parser))

  (method (set-base (P <expat-parser>) data)
    (expat.XML_SetBase P.parser data))

  (method (get-base (P <expat-parser>))
    (expat.XML_GetBase P.parser))

  (method (get-specified-attribute-count (P <expat-parser>))
    (expat.XML_GetSpecifiedAttributeCount P.parser))

  (method (get-id-attribute-index (P <expat-parser>))
    (expat.XML_GetIdAttributeIndex P.parser))

  (method (use-foreign-dtd (P <expat-parser>) use-dtd?)
    (expat.XML_UseForeignDTD P.parser use-dtd?))

  (method (get-parsing-status (P <expat-parser>))
    (expat.XML_GetParsingStatus P.parser))

  (method (set-param-entity-parsing (P <expat-parser>) code)
    (expat.XML_SetParamEntityParsing P.parser code))

  (method (get-error-code (P <expat-parser>))
    (expat.XML_GetErrorCode P.parser))

  (method (error-string (P <expat-parser>))
    (expat.XML_ErrorString (expat.XML_GetErrorCode P.parser)))

  (method (get-current-byte-index (P <expat-parser>))
    (expat.XML_GetCurrentByteIndex P.parser))

  (method (get-current-byte-count (P <expat-parser>))
    (expat.XML_GetCurrentByteCount P.parser))

  (method (get-current-column-number (P <expat-parser>))
    (expat.XML_GetCurrentColumnNumber P.parser))

  (method (get-current-line-number (P <expat-parser>))
    (expat.XML_GetCurrentLineNumber P.parser))

;;; --------------------------------------------------------------------

  (method (attlist-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.AttlistDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_AttlistDeclHandler scheme-callback)))
	  (set! P.AttlistDeclHandler cb)
	  (expat.XML_SetAttlistDeclHandler P.parser cb))
      (begin
	(set! P.AttlistDeclHandler #f)
	(expat.XML_SetAttlistDeclHandler P.parser (ffi.null-pointer)))))

  (method (character-data-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.CharacterDataHandler)
    (if scheme-callback
	(let ((cb (expat.XML_CharacterDataHandler scheme-callback)))
	  (set! P.CharacterDataHandler cb)
	  (expat.XML_SetCharacterDataHandler P.parser cb))
      (begin
	(set! P.CharacterDataHandler #f)
	(expat.XML_SetCharacterDataHandler P.parser (ffi.null-pointer)))))

  (method (comment-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.CommentHandler)
    (if scheme-callback
	(let ((cb (expat.XML_CommentHandler scheme-callback)))
	  (set! P.CommentHandler cb)
	  (expat.XML_SetCommentHandler P.parser cb))
      (begin
	(set! P.CommentHandler #f)
	(expat.XML_SetCommentHandler P.parser (ffi.null-pointer)))))

  (method (default-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.DefaultHandler)
    (if scheme-callback
	(let ((cb (expat.XML_DefaultHandler scheme-callback)))
	  (set! P.DefaultHandler cb)
	  (expat.XML_SetDefaultHandler P.parser cb))
      (begin
	(set! P.DefaultHandler #f)
	(expat.XML_SetDefaultHandler P.parser (ffi.null-pointer)))))

  (method (element-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.ElementDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_ElementDeclHandler scheme-callback)))
	  (set! P.ElementDeclHandler cb)
	  (expat.XML_SetElementDeclHandler P.parser cb))
      (begin
	(set! P.ElementDeclHandler #f)
	(expat.XML_SetElementDeclHandler P.parser (ffi.null-pointer)))))

  (method (end-cdata-section-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EndCdataSectionHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EndCdataSectionHandler scheme-callback)))
	  (set! P.EndCdataSectionHandler cb)
	  (expat.XML_SetEndCdataSectionHandler P.parser cb))
      (begin
	(set! P.EndCdataSectionHandler #f)
	(expat.XML_SetEndCdataSectionHandler P.parser (ffi.null-pointer)))))

  (method (end-doctype-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EndDoctypeDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EndDoctypeDeclHandler scheme-callback)))
	  (set! P.EndDoctypeDeclHandler cb)
	  (expat.XML_SetEndDoctypeDeclHandler P.parser cb))
      (begin
	(set! P.EndDoctypeDeclHandler #f)
	(expat.XML_SetEndDoctypeDeclHandler P.parser (ffi.null-pointer)))))

  (method (end-element-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EndElementHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EndElementHandler scheme-callback)))
	  (set! P.EndElementHandler cb)
	  (expat.XML_SetEndElementHandler P.parser cb))
      (begin
	(set! P.EndElementHandler #f)
	(expat.XML_SetEndElementHandler P.parser (ffi.null-pointer)))))

  (method (end-namespace-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EndNamespaceDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EndNamespaceDeclHandler scheme-callback)))
	  (set! P.EndNamespaceDeclHandler cb)
	  (expat.XML_SetEndNamespaceDeclHandler P.parser cb))
      (begin
	(set! P.EndNamespaceDeclHandler #f)
	(expat.XML_SetEndNamespaceDeclHandler P.parser (ffi.null-pointer)))))

  (method (entity-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EntityDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EntityDeclHandler scheme-callback)))
	  (set! P.EntityDeclHandler cb)
	  (expat.XML_SetEntityDeclHandler P.parser cb))
      (begin
	(set! P.EntityDeclHandler #f)
	(expat.XML_SetEntityDeclHandler P.parser (ffi.null-pointer)))))

  (method (external-entity-ref-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.ExternalEntityRefHandler)
    (if scheme-callback
	(let ((cb (expat.XML_ExternalEntityRefHandler scheme-callback)))
	  (set! P.ExternalEntityRefHandler cb)
	  (expat.XML_SetExternalEntityRefHandler P.parser cb))
      (begin
	(set! P.ExternalEntityRefHandler #f)
	(expat.XML_SetExternalEntityRefHandler P.parser (ffi.null-pointer)))))

  (method (not-standalone-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.NotStandaloneHandler)
    (if scheme-callback
	(let ((cb (expat.XML_NotStandaloneHandler scheme-callback)))
	  (set! P.NotStandaloneHandler cb)
	  (expat.XML_SetNotStandaloneHandler P.parser cb))
      (begin
	(set! P.NotStandaloneHandler #f)
	(expat.XML_SetNotStandaloneHandler P.parser (ffi.null-pointer)))))

  (method (notation-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.NotationDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_NotationDeclHandler scheme-callback)))
	  (set! P.NotationDeclHandler cb)
	  (expat.XML_SetNotationDeclHandler P.parser cb))
      (begin
	(set! P.NotationDeclHandler #f)
	(expat.XML_SetNotationDeclHandler P.parser (ffi.null-pointer)))))

  (method (processing-instruction-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.ProcessingInstructionHandler)
    (if scheme-callback
	(let ((cb (expat.XML_ProcessingInstructionHandler scheme-callback)))
	  (set! P.ProcessingInstructionHandler cb)
	  (expat.XML_SetProcessingInstructionHandler P.parser cb))
      (begin
	(set! P.ProcessingInstructionHandler #f)
	(expat.XML_SetProcessingInstructionHandler P.parser (ffi.null-pointer)))))

  (method (skipped-entity-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.SkippedEntityHandler)
    (if scheme-callback
	(let ((cb (expat.XML_SkippedEntityHandler scheme-callback)))
	  (set! P.SkippedEntityHandler cb)
	  (expat.XML_SetSkippedEntityHandler P.parser cb))
      (begin
	(set! P.SkippedEntityHandler #f)
	(expat.XML_SetSkippedEntityHandler P.parser (ffi.null-pointer)))))

  (method (start-cdata-section-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.StartCdataSectionHandler)
    (if scheme-callback
	(let ((cb (expat.XML_StartCdataSectionHandler scheme-callback)))
	  (set! P.StartCdataSectionHandler cb)
	  (expat.XML_SetStartCdataSectionHandler P.parser cb))
      (begin
	(set! P.StartCdataSectionHandler #f)
	(expat.XML_SetStartCdataSectionHandler P.parser (ffi.null-pointer)))))

  (method (start-doctype-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.StartDoctypeDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_StartDoctypeDeclHandler scheme-callback)))
	  (set! P.StartDoctypeDeclHandler cb)
	  (expat.XML_SetStartDoctypeDeclHandler P.parser cb))
      (begin
	(set! P.StartDoctypeDeclHandler #f)
	(expat.XML_SetStartDoctypeDeclHandler P.parser (ffi.null-pointer)))))

  (method (start-element-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.StartElementHandler)
    (if scheme-callback
	(let ((cb (expat.XML_StartElementHandler scheme-callback)))
	  (set! P.StartElementHandler cb)
	  (expat.XML_SetStartElementHandler P.parser cb))
      (begin
	(set! P.StartElementHandler #f)
	(expat.XML_SetStartElementHandler P.parser (ffi.null-pointer)))))

  (method (start-namespace-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.StartNamespaceDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_StartNamespaceDeclHandler scheme-callback)))
	  (set! P.StartNamespaceDeclHandler cb)
	  (expat.XML_SetStartNamespaceDeclHandler P.parser cb))
      (begin
	(set! P.StartNamespaceDeclHandler #f)
	(expat.XML_SetStartNamespaceDeclHandler P.parser (ffi.null-pointer)))))

  (method (unparsed-entity-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.UnparsedEntityDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_UnparsedEntityDeclHandler scheme-callback)))
	  (set! P.UnparsedEntityDeclHandler cb)
	  (expat.XML_SetUnparsedEntityDeclHandler P.parser cb))
      (begin
	(set! P.UnparsedEntityDeclHandler #f)
	(expat.XML_SetUnparsedEntityDeclHandler P.parser (ffi.null-pointer)))))

  (method (xml-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.XmlDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_XmlDeclHandler scheme-callback)))
	  (set! P.XmlDeclHandler cb)
	  (expat.XML_SetXmlDeclHandler P.parser cb))
      (begin
	(set! P.XmlDeclHandler #f)
	(expat.XML_SetXmlDeclHandler P.parser (ffi.null-pointer)))))

  )


(define-class <expat-ns-parser>
  (nongenerative nausicaa:xml:expat:<expat-ns-parser>)
  (inherit <expat-parser>)

  (maker ()
	 (encoding:		#f)
	 (namespace-separator:	#\:))

  (protocol
   (lambda (make-expat-parser)
     (lambda (encoding namespace-separator)
       (let ((parser (expat.XML_ParserCreateNS encoding namespace-separator)))
	 (if parser
	     ((make-expat-parser parser))
	   (%raise-expat-error '<expat-ns-parser>
	     "error building Expat parser" (list encoding namespace-separator)))))))

;;; --------------------------------------------------------------------

  (method (set-return-ns-triplet (P <expat-parser>) do-nst?)
    (expat.XML_SetReturnNSTriplet P.parser do-nst?))

  )


(define-class <expat-entity-parser>
  (nongenerative nausicaa:xml:expat:<expat-entity-parser>)
  (inherit <expat-parser>)

  (maker (root-parser context)
	 (encoding:	'UTF-8))

  (protocol
   (lambda (make-expat-parser)
     (lambda (root-parser context encoding)
       (let ((parser (expat.XML_ExternalEntityParserCreate root-parser context encoding)))
	 (if parser
	     ((make-expat-parser parser))
	   (%raise-expat-error '<expat-entity-parser>
	     "error building Expat parser" (list root-parser context encoding)))))))

  )


(define-label <expat-parsing-status>
  (custom-maker expat.make-XML_ParsingStatus)
  (predicate expat.XML_ParsingStatus?)
  (virtual-fields (immutable parsing		expat.XML_ParsingStatus-parsing)
		  (immutable final-buffer?	expat.XML_ParsingStatus-final-buffer?)))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put '%raise-expat-error  'scheme-indent-function 1)
;; eval: (put '%handle-status-code 'scheme-indent-function 1)
;; End:
