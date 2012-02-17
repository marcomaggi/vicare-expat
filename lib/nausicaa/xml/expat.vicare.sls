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
    encoding:
    namespace-separator:

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
    (prefix (vicare ffi) ffi.))


;;;; auxiliary definitions

(define-inline (%clean-callback field)
  (let ((cb field))
    (when cb
      (ffi.free-c-callback cb))))

(define-finaliser parser-finaliser
  (lambda ((P <expat-parser>))
;;;(pretty-print (list 'collected parser) (current-error-port))
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
    (%clean-callback P.XmlDeclHandler)
    ))

(define-auxiliary-syntaxes
  encoding:
  namespace-separator:)


(define-class <expat-parser>
  (nongenerative nausicaa:xml:expat:<vicare-expat>)
  (fields (immutable parser)	;pointer to parser

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
	 (encoding: #f)
	 (namespace-separator: #f))

  (protocol
   (lambda (make-top)
     (lambda (encoding namespace-separator)
       (let ((parser (if namespace-separator
			 (expat.XML_ParserCreateNS encoding namespace-separator)
		       (expat.XML_ParserCreate encoding))))
	 (if parser
	     (parser-finaliser ((make-top) parser
				#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
	   (error '<expat-parser> "error building Expat parser"))))))

  (method (attlist-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.AttlistDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_AttlistDeclHandler scheme-callback)))
	  (set! P.AttlistDeclHandler cb)
	  (expat.XML_SetAttlistDeclHandler cb))
      (begin
	(set! P.AttlistDeclHandler #f)
	(expat.XML_SetAttlistDeclHandler (ffi.null-pointer)))))

  (method (character-data-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.CharacterDataHandler)
    (if scheme-callback
	(let ((cb (expat.XML_CharacterDataHandler scheme-callback)))
	  (set! P.CharacterDataHandler cb)
	  (expat.XML_SetCharacterDataHandler cb))
      (begin
	(set! P.CharacterDataHandler #f)
	(expat.XML_SetCharacterDataHandler (ffi.null-pointer)))))

  (method (comment-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.CommentHandler)
    (if scheme-callback
	(let ((cb (expat.XML_CommentHandler scheme-callback)))
	  (set! P.CommentHandler cb)
	  (expat.XML_CommentHandler cb))
      (begin
	(set! P.CommentHandler #f)
	(expat.XML_CommentHandler (ffi.null-pointer)))))

  (method (default-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.DefaultHandler)
    (if scheme-callback
	(let ((cb (expat.XML_DefaultHandler scheme-callback)))
	  (set! P.DefaultHandler cb)
	  (expat.XML_DefaultHandler cb))
      (begin
	(set! P.DefaultHandler #f)
	(expat.XML_DefaultHandler (ffi.null-pointer)))))

  (method (element-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.ElementDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_ElementDeclHandler scheme-callback)))
	  (set! P.ElementDeclHandler cb)
	  (expat.XML_ElementDeclHandler cb))
      (begin
	(set! P.ElementDeclHandler #f)
	(expat.XML_ElementDeclHandler (ffi.null-pointer)))))

  (method (end-cdata-section-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EndCdataSectionHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EndCdataSectionHandler scheme-callback)))
	  (set! P.EndCdataSectionHandler cb)
	  (expat.XML_EndCdataSectionHandler cb))
      (begin
	(set! P.EndCdataSectionHandler #f)
	(expat.XML_EndCdataSectionHandler (ffi.null-pointer)))))

  (method (end-doctype-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EndDoctypeDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EndDoctypeDeclHandler scheme-callback)))
	  (set! P.EndDoctypeDeclHandler cb)
	  (expat.XML_EndDoctypeDeclHandler cb))
      (begin
	(set! P.EndDoctypeDeclHandler #f)
	(expat.XML_EndDoctypeDeclHandler (ffi.null-pointer)))))

  (method (end-element-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EndElementHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EndElementHandler scheme-callback)))
	  (set! P.EndElementHandler cb)
	  (expat.XML_EndElementHandler cb))
      (begin
	(set! P.EndElementHandler #f)
	(expat.XML_EndElementHandler (ffi.null-pointer)))))

  (method (end-namespace-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EndNamespaceDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EndNamespaceDeclHandler scheme-callback)))
	  (set! P.EndNamespaceDeclHandler cb)
	  (expat.XML_EndNamespaceDeclHandler cb))
      (begin
	(set! P.EndNamespaceDeclHandler #f)
	(expat.XML_EndNamespaceDeclHandler (ffi.null-pointer)))))

  (method (entity-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.EntityDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_EntityDeclHandler scheme-callback)))
	  (set! P.EntityDeclHandler cb)
	  (expat.XML_EntityDeclHandler cb))
      (begin
	(set! P.EntityDeclHandler #f)
	(expat.XML_EntityDeclHandler (ffi.null-pointer)))))

  (method (external-entity-ref-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.ExternalEntityRefHandler)
    (if scheme-callback
	(let ((cb (expat.XML_ExternalEntityRefHandler scheme-callback)))
	  (set! P.ExternalEntityRefHandler cb)
	  (expat.XML_ExternalEntityRefHandler cb))
      (begin
	(set! P.ExternalEntityRefHandler #f)
	(expat.XML_ExternalEntityRefHandler (ffi.null-pointer)))))

  (method (not-standalone-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.NotStandaloneHandler)
    (if scheme-callback
	(let ((cb (expat.XML_NotStandaloneHandler scheme-callback)))
	  (set! P.NotStandaloneHandler cb)
	  (expat.XML_NotStandaloneHandler cb))
      (begin
	(set! P.NotStandaloneHandler #f)
	(expat.XML_NotStandaloneHandler (ffi.null-pointer)))))

  (method (notation-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.NotationDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_NotationDeclHandler scheme-callback)))
	  (set! P.NotationDeclHandler cb)
	  (expat.XML_NotationDeclHandler cb))
      (begin
	(set! P.NotationDeclHandler #f)
	(expat.XML_NotationDeclHandler (ffi.null-pointer)))))

  (method (processing-instruction-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.ProcessingInstructionHandler)
    (if scheme-callback
	(let ((cb (expat.XML_ProcessingInstructionHandler scheme-callback)))
	  (set! P.ProcessingInstructionHandler cb)
	  (expat.XML_ProcessingInstructionHandler cb))
      (begin
	(set! P.ProcessingInstructionHandler #f)
	(expat.XML_ProcessingInstructionHandler (ffi.null-pointer)))))

  (method (skipped-entity-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.SkippedEntityHandler)
    (if scheme-callback
	(let ((cb (expat.XML_SkippedEntityHandler scheme-callback)))
	  (set! P.SkippedEntityHandler cb)
	  (expat.XML_SkippedEntityHandler cb))
      (begin
	(set! P.SkippedEntityHandler #f)
	(expat.XML_SkippedEntityHandler (ffi.null-pointer)))))

  (method (start-cdata-section-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.StartCdataSectionHandler)
    (if scheme-callback
	(let ((cb (expat.XML_StartCdataSectionHandler scheme-callback)))
	  (set! P.StartCdataSectionHandler cb)
	  (expat.XML_StartCdataSectionHandler cb))
      (begin
	(set! P.StartCdataSectionHandler #f)
	(expat.XML_StartCdataSectionHandler (ffi.null-pointer)))))

  (method (start-doctype-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.StartDoctypeDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_StartDoctypeDeclHandler scheme-callback)))
	  (set! P.StartDoctypeDeclHandler cb)
	  (expat.XML_StartDoctypeDeclHandler cb))
      (begin
	(set! P.StartDoctypeDeclHandler #f)
	(expat.XML_StartDoctypeDeclHandler (ffi.null-pointer)))))

  (method (start-element-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.StartElementHandler)
    (if scheme-callback
	(let ((cb (expat.XML_StartElementHandler scheme-callback)))
	  (set! P.StartElementHandler cb)
	  (expat.XML_StartElementHandler cb))
      (begin
	(set! P.StartElementHandler #f)
	(expat.XML_StartElementHandler (ffi.null-pointer)))))

  (method (start-namespace-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.StartNamespaceDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_StartNamespaceDeclHandler scheme-callback)))
	  (set! P.StartNamespaceDeclHandler cb)
	  (expat.XML_StartNamespaceDeclHandler cb))
      (begin
	(set! P.StartNamespaceDeclHandler #f)
	(expat.XML_StartNamespaceDeclHandler (ffi.null-pointer)))))

  (method (unparsed-entity-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.UnparsedEntityDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_UnparsedEntityDeclHandler scheme-callback)))
	  (set! P.UnparsedEntityDeclHandler cb)
	  (expat.XML_UnparsedEntityDeclHandler cb))
      (begin
	(set! P.UnparsedEntityDeclHandler #f)
	(expat.XML_UnparsedEntityDeclHandler (ffi.null-pointer)))))

  (method (xml-decl-handler (P <expat-parser>) scheme-callback)
    (%clean-callback P.XmlDeclHandler)
    (if scheme-callback
	(let ((cb (expat.XML_XmlDeclHandler scheme-callback)))
	  (set! P.XmlDeclHandler cb)
	  (expat.XML_XmlDeclHandler cb))
      (begin
	(set! P.XmlDeclHandler #f)
	(expat.XML_XmlDeclHandler (ffi.null-pointer)))))

  )


;;;; done

)

;;; end of file
