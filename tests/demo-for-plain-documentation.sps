;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: basic tests to be included in the documentation
;;;Date: Wed Jan 25, 2012
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



#!r6rs
(import (vicare)
  (vicare xml expat)
  (vicare xml expat constants)
  (prefix (vicare ffi) ffi.))


;;;; simple document parsing

(when #f
  (let ()

    (define xml-utf8
      (string->utf8
       "<!-- this is a test document -->\
     <stuff>\
     <thing colour=\"yellow\">\
     <alpha>one</alpha>\
     <beta>two</beta>\
     </thing>\
     <thing>\
     <alpha>123</alpha>\
     <beta>456</beta>\
     </thing>\
     </stuff>"))

    (define (start-callback data element attributes)
      (let ((element    (ffi.cstring->string element))
	    (attributes (ffi.argv->strings attributes)))
	(pretty-print (list 'start element attributes))))

    (define (end-callback data element)
      (let ((element (ffi.cstring->string element)))
	(pretty-print (list 'end element))))

    (define (cdata-callback data buf.ptr buf.len)
      (let ((text (ffi.cstring->string buf.ptr buf.len)))
	(pretty-print (list 'cdata text))))

    (define (comment-callback data cstr)
      (let ((text (ffi.cstring->string cstr)))
	(pretty-print (list 'comment text))))

    (let ((parser	(XML_ParserCreate 'UTF-8))
	  (start	(XML_StartElementHandler  start-callback))
	  (end		(XML_EndElementHandler    end-callback))
	  (cdata	(XML_CharacterDataHandler cdata-callback))
	  (comment	(XML_CommentHandler       comment-callback)))
      (XML_SetElementHandler       parser start end)
      (XML_SetCharacterDataHandler parser cdata)
      (XML_SetCommentHandler       parser comment)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback start)
      (ffi.free-c-callback end)
      (ffi.free-c-callback cdata)
      (ffi.free-c-callback comment))

    (flush-output-port (current-output-port))

    #f))


;;;; XML declarations

(let ()

  (define (%process-standalone standalone)
    (case standalone
      ((-1)	'unspecified)
      ((0)	'non-standalone)
      ((1)	'standalone)
      (else	#f)))

  (define (xml-decl-callback user-data version encoding standalone)
    (pretty-print
     (list 'xml-decl
	   (or (ffi.pointer-null? version)  (ffi.cstring->string version))
	   (or (ffi.pointer-null? encoding) (ffi.cstring->string encoding))
	   (%process-standalone standalone))))

  (define (doit xml-utf8)
    (let ((parser	(XML_ParserCreate))
	  (xml-decl	(XML_XmlDeclHandler xml-decl-callback)))
      (XML_SetXmlDeclHandler parser xml-decl)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback xml-decl)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------

  (define dtd-utf8
    (string->utf8
     "<?xml version='1.0' encoding='utf-8'?>
      <!ELEMENT ball EMPTY>
      <!ATTLIST ball colour CDATA #REQUIRED>"))

  (define (external-entity-callback parser context base system-id public-id)
    (pretty-print
     (list 'external-entity
	   (or (ffi.pointer-null? context)     (ffi.cstring->string context))
	   (or (ffi.pointer-null? base)        (ffi.cstring->string base))
	   (ffi.cstring->string system-id)
	   (or (ffi.pointer-null? public-id)   (ffi.cstring->string public-id))))
    (let* ((parser	(XML_ExternalEntityParserCreate parser context 'UTF-8))
	   (xml-decl	(XML_XmlDeclHandler xml-decl-callback)))
      (XML_SetXmlDeclHandler parser xml-decl)
      (let ((rv (XML_Parse parser dtd-utf8 #f #t)))
	(ffi.free-c-callback xml-decl)
	rv)))

  (define (doit-with-external-entity xml-utf8)
    (let* ((parser	(XML_ParserCreate))
	   (xml-decl	(XML_XmlDeclHandler xml-decl-callback))
	   (ext-ent	(XML_ExternalEntityRefHandler external-entity-callback)))
      (XML_SetParamEntityParsing parser XML_PARAM_ENTITY_PARSING_ALWAYS)
      (XML_SetXmlDeclHandler parser xml-decl)
      (XML_SetExternalEntityRefHandler parser ext-ent)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback xml-decl)
      (ffi.free-c-callback ext-ent)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------
;;; XML declaration

  (when #f
    (doit (string->utf8 "<?xml version='1.0'?><toys><ball colour='red'/></toys>")))

  (when #f
    (doit (string->utf8 "<?xml version='1.0' encoding='utf-8'?><toys><ball colour='red'/></toys>")))

  (when #f
    (doit (string->utf8 "<?xml version='1.0' standalone='yes'?><toys><ball colour='red'/></toys>")))

  (when #f
    (doit (string->utf8 "<?xml version='1.0' standalone='no'?><toys><ball colour='red'/></toys>")))

;;; --------------------------------------------------------------------
;;; external entity

  (when #f
    (doit-with-external-entity
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
       <toys><ball colour='red'/></toys>")))

  #t)


;;;; DTD doctype handler

(let ()

  (define (start-doctype-callback data doctype-name sysid pubid has-internal-subset)
    (pretty-print
     (list 'doctype-start
	   (ffi.cstring->string doctype-name)
	   (or (ffi.pointer-null? sysid) (ffi.cstring->string sysid))
	   (or (ffi.pointer-null? pubid) (ffi.cstring->string pubid))
	   has-internal-subset)))

  (define (end-doctype-callback data)
    (pretty-print '(doctype-end)))

  (define (doit xml-utf8)
    (let ((parser	(XML_ParserCreate))
	  (start	(XML_StartDoctypeDeclHandler start-doctype-callback))
	  (end		(XML_EndDoctypeDeclHandler   end-doctype-callback)))
      (XML_SetDoctypeDeclHandler parser start end)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback start)
      (ffi.free-c-callback end)
      (flush-output-port (current-output-port))))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
       <toys><ball colour='yellow'/></toys>")))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
       <toys><ball colour='yellow'/></toys>")))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys [
         <!ELEMENT ball EMPTY>
         <!ATTLIST ball colour CDATA #REQUIRED>
       ]>
       <toys><ball colour='yellow'/></toys>")))

  #t)


;;;; DTD element declaration handler

(let ()

  (define (doit xml)
    (let ((xml-utf8	(string->utf8 xml))
	  (parser	(XML_ParserCreate))
	  (dtd-elm	(XML_ElementDeclHandler dtd-elm-callback)))
      (XML_UseParserAsHandlerArg parser)
      (XML_SetElementDeclHandler parser dtd-elm)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback dtd-elm)
      (flush-output-port (current-output-port))))

  (define (dtd-elm-callback data name model)
    (pretty-print
     (list 'dtd-element
	   (ffi.cstring->string name)
	   (pointer->XML_Content model)))
    (XML_FreeContentModel data model))

;;; --------------------------------------------------------------------

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT ball EMPTY>
           ]>
           <toys><ball/></toys>"))

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT ball ANY>
           ]>
           <toys><ball/></toys>"))

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT toys (ball)>
             <!ELEMENT ball EMPTY>
           ]>
           <toys><ball/></toys>"))

  (when #f
    (doit "<!DOCTYPE outer [
             <!ELEMENT outer (middle)>
             <!ELEMENT middle (inner)>
             <!ELEMENT inner EMPTY>
           ]>
           <outer><middle><inner/></middle></outer>"))

  (when #f
    (doit "<!DOCTYPE this [
             <!ELEMENT this (#PCDATA)>
           ]>
           <this>ciao</this>"))

  (when #f
    (doit "<!DOCTYPE this [
             <!ELEMENT this (#PCDATA|that)*>
             <!ELEMENT that EMPTY>
           ]>
           <this><that/></this>"))

;;; --------------------------------------------------------------------
;;; quantifiers

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT toys (ball)*>
             <!ELEMENT ball EMPTY>
           ]>
           <toys><ball/></toys>"))

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT toys (ball)?>
             <!ELEMENT ball EMPTY>
           ]>
           <toys><ball/></toys>"))

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT toys (ball)+>
             <!ELEMENT ball EMPTY>
           ]>
           <toys><ball/></toys>"))

  #t)


;;;; DTD attlist declaration handler

(let ()

  (define (doit xml)
    (let* ((xml-utf8	(string->utf8 xml))
	   (parser	(XML_ParserCreate))
	   (dtd-attlist	(XML_AttlistDeclHandler dtd-attlist-callback))
	   (elm-start	(XML_StartElementHandler elm-start-callback)))
      (XML_SetAttlistDeclHandler parser dtd-attlist)
      (XML_SetStartElementHandler parser elm-start)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback dtd-attlist)
      (ffi.free-c-callback elm-start)
      (flush-output-port (current-output-port))))

  (define (dtd-attlist-callback user-data element-name attribute-name
				attribute-type default-value required?)
    (pretty-print
     (list 'dtd-attlist
	   (ffi.cstring->string element-name)
	   (ffi.cstring->string attribute-name)
	   (ffi.cstring->string attribute-type)
	   (if (ffi.pointer-null? default-value)
	       'no-value
	     (ffi.cstring->string default-value))
	   (fxpositive? required?))))

  (define (elm-start-callback data element attributes)
    (pretty-print
     (list 'element-start
	   (ffi.cstring->string element)
	   (ffi.argv->strings attributes))))

;;; --------------------------------------------------------------------

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT ball EMPTY>
             <!ATTLIST ball colour CDATA #REQUIRED>
           ]>
           <toys><ball colour='red' /></toys>"))

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT ball EMPTY>
             <!ATTLIST ball colour CDATA #IMPLIED>
           ]>
           <toys><ball colour='red'/></toys>"))

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT ball EMPTY>
             <!ATTLIST ball colour CDATA #FIXED 'red'>
           ]>
           <toys><ball/></toys>"))

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT ball EMPTY>
             <!ATTLIST ball colour (red|blue|yellow) #REQUIRED>
           ]>
           <toys><ball colour='red' /></toys>"))

  (when #f
    (doit "<!DOCTYPE toys [
             <!ELEMENT ball EMPTY>
             <!ATTLIST ball colour CDATA 'red'>
           ]>
           <toys><ball/></toys>"))

  #t)


;;;; DTD notation handler

(let ()

  (define (%false-or-string thing)
    (if (ffi.pointer-null? thing)
	#f
      (ffi.cstring->string thing)))

  (define (notation-callback data notation-name base system-id public-id)
    (pretty-print
     (list 'notation
	   (%false-or-string notation-name)
	   (%false-or-string base)
	   (%false-or-string system-id)
	   (%false-or-string public-id))))

  (define (doit xml-utf8)
    (let* ((parser	(XML_ParserCreate))
	   (notation	(XML_NotationDeclHandler notation-callback)))
      (XML_SetNotationDeclHandler parser notation)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback notation)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------

  (when #f
    (doit (string->utf8
	   "<?xml version='1.0'?>
            <!DOCTYPE toys [
              <!NOTATION bouncing SYSTEM 'http://localhost/bouncer'>
              <!ELEMENT ball EMPTY>
              <!ATTLIST ball colour CDATA #REQUIRED>
            ]>
            <toys><ball colour='red' /></toys>")))

  (when #f
    (doit (string->utf8
	   "<?xml version='1.0'?>
            <!DOCTYPE toys [
              <!NOTATION bouncing PUBLIC 'The Bouncer'>
              <!ELEMENT ball EMPTY>
              <!ATTLIST ball colour CDATA #REQUIRED>
            ]>
            <toys><ball colour='red' /></toys>")))

  (when #f
    (doit (string->utf8
	   "<?xml version='1.0'?>
            <!DOCTYPE toys [
              <!NOTATION bouncing PUBLIC 'The Bouncer' 'http://localhost/bouncer'>
              <!ELEMENT ball EMPTY>
              <!ATTLIST ball colour CDATA #REQUIRED>
            ]>
            <toys><ball colour='red' /></toys>")))

  #t)


;;;; DTD entity handler

(let ()

  (define (%false-or-string thing)
    (if (ffi.pointer-null? thing)
	#f
      (ffi.cstring->string thing)))

  (define (doit xml)
    (let* ((xml-utf8	(string->utf8 xml))
	   (parser	(XML_ParserCreate))
	   (dtd-entity	(XML_EntityDeclHandler dtd-entity-callback))
	   (elm-start	(XML_StartElementHandler elm-start-callback)))
      (XML_SetBase parser (string->utf8 "http://localhost/"))
      (XML_SetParamEntityParsing parser XML_PARAM_ENTITY_PARSING_ALWAYS)
      (XML_SetEntityDeclHandler parser dtd-entity)
      (XML_SetStartElementHandler parser elm-start)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback dtd-entity)
      (ffi.free-c-callback elm-start)
      (flush-output-port (current-output-port))))

  (define (dtd-entity-callback data entity-name is-parameter-entity
			       value value-length
			       base system-id public-id
			       notation-name)
    (pretty-print
     (list 'dtd-entity
	   (%false-or-string entity-name)
	   (fxpositive? is-parameter-entity)
	   (if (ffi.pointer-null? value)
	       #f
	     (ffi.cstring->string value value-length))
	   (%false-or-string base)
	   (%false-or-string system-id)
	   (%false-or-string public-id)
	   (%false-or-string notation-name))))

  (define (elm-start-callback data element attributes)
    (pretty-print
     (list 'element-start
	   (ffi.cstring->string element)
	   (ffi.argv->strings attributes))))

;;; --------------------------------------------------------------------

  (when #f	;general internal entity
    (doit "<?xml version='1.0'?>
           <!DOCTYPE thing [
             <!ELEMENT thing EMPTY>
             <!ATTLIST thing frob (a|b|c) #REQUIRED>
             <!ENTITY stuff 'a'>
           ]>
           <thing frob='&stuff;'/>"))

  (when #f	;general external entity, SYSTEM
    (doit "<?xml version='1.0'?>
           <!DOCTYPE thing [
             <!ELEMENT thing EMPTY>
             <!ENTITY stuff SYSTEM 'http://localhost/stuff'>
           ]>
           <thing/>"))

  (when #f	;general external entity, PUBLIC
    (doit "<?xml version='1.0'?>
           <!DOCTYPE thing [
             <!ELEMENT thing EMPTY>
             <!ENTITY stuff PUBLIC 'The Stuff' 'http://localhost/stuff'>
           ]>
           <thing/>"))

  (when #f	;general external entity, SYSTEM with notation
    (doit "<?xml version='1.0'?>
           <!DOCTYPE thing [
             <!ELEMENT thing EMPTY>
             <!NOTATION stuffer SYSTEM 'http://localhost/stuffer'>
             <!ENTITY stuff SYSTEM 'http://localhost/stuff' NDATA stuffer>
           ]>
           <thing/>"))

  (when #f	;general external entity, PUBLIC with notation
    (doit "<?xml version='1.0'?>
           <!DOCTYPE thing [
             <!ELEMENT thing EMPTY>
             <!NOTATION stuffer SYSTEM 'http://localhost/stuffer'>
             <!ENTITY stuff PUBLIC 'The Stuff' 'http://localhost/stuff' NDATA stuffer>
           ]>
           <thing/>"))

;;; --------------------------------------------------------------------

  (when #f	;parameter internal entity
    (doit "<?xml version='1.0'?>
           <!DOCTYPE thing [
             <!ENTITY % stuff 'a'>
             <!ELEMENT thing EMPTY>
             <!ATTLIST thing>
           ]>
           <thing/>"))

  (when #f	;parameter external entity, SYSTEM
    (doit "<?xml version='1.0'?>
           <!DOCTYPE thing [
             <!ELEMENT thing EMPTY>
             <!ENTITY % stuff SYSTEM 'http://localhost/stuff'>
           ]>
           <thing/>"))

  (when #f	;parameter external entity, PUBLIC
    (doit "<?xml version='1.0'?>
           <!DOCTYPE thing [
             <!ELEMENT thing EMPTY>
             <!ENTITY % stuff PUBLIC 'The Stuff' 'http://localhost/stuff'>
           ]>
           <thing/>"))

  #t)


;;;; not standalone handler

(let ()

  (define (%process-standalone standalone)
    (case standalone
      ((-1)	'unspecified)
      ((0)	'non-standalone)
      ((1)	'standalone)
      (else	#f)))

  (define (xml-decl-callback user-data version encoding standalone)
    (pretty-print
     (list 'xml-decl (%process-standalone standalone))))

  (define (not-stand-callback user-data)
    (pretty-print '(not-standalone))
    XML_STATUS_OK)

  (define (start-doctype-callback data doctype-name sysid pubid has-internal-subset)
    (pretty-print
     (list 'doctype-start has-internal-subset)))

  (define (end-doctype-callback data)
    (pretty-print '(doctype-end)))

  (define (doit xml-utf8)
    (let ((parser	(XML_ParserCreate))
	  (xml-decl	(XML_XmlDeclHandler xml-decl-callback))
	  (not-stand	(XML_NotStandaloneHandler not-stand-callback))
	  (dt-start	(XML_StartDoctypeDeclHandler start-doctype-callback))
	  (dt-end	(XML_EndDoctypeDeclHandler end-doctype-callback)))
      (XML_SetXmlDeclHandler parser xml-decl)
      (XML_SetNotStandaloneHandler parser not-stand)
      (XML_SetStartDoctypeDeclHandler parser dt-start)
      (XML_SetEndDoctypeDeclHandler   parser dt-end)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback xml-decl)
      (ffi.free-c-callback not-stand)
      (ffi.free-c-callback dt-start)
      (ffi.free-c-callback dt-end)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <toys><ball colour='red'/></toys>")))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
       <toys><ball colour='red'/></toys>")))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
       <toys><ball colour='red'/></toys>")))

;;; --------------------------------------------------------------------

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0' standalone='no'?>
       <toys><ball colour='red'/></toys>")))


  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0' standalone='no'?>
       <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
       <toys><ball colour='red'/></toys>")))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0' standalone='no'?>
       <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
       <toys><ball colour='red'/></toys>")))

;;; --------------------------------------------------------------------

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0' standalone='yes'?>
       <toys><ball colour='red'/></toys>")))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0' standalone='yes'?>
       <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
       <toys><ball colour='red'/></toys>")))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0' standalone='yes'?>
       <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
       <toys><ball colour='red'/></toys>")))

  #t)


;;;; external entity parsing

(let ()

  (define dtd-utf8
    (string->utf8 "<!ELEMENT ball EMPTY>
                   <!ATTLIST ball colour CDATA #REQUIRED>"))

  (define (ext-callback parser context base system-id public-id)
    (let* ((parser (XML_ExternalEntityParserCreate parser context 'UTF-8))
	   (rv     (XML_Parse parser dtd-utf8 #f #t)))
      (pretty-print
       (list 'external-entity rv
	     (ffi.pointer-null? context)
	     (ffi.pointer-null? base)
	     (ffi.cstring->string system-id)
	     (ffi.pointer-null? public-id)))
      XML_STATUS_OK))

  (define (doit xml-utf8)
    (let* ((parser	(XML_ParserCreate))
	   (ext		(XML_ExternalEntityRefHandler ext-callback)))
      (XML_SetParamEntityParsing parser XML_PARAM_ENTITY_PARSING_ALWAYS)
      (XML_SetExternalEntityRefHandler parser ext)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback ext)
      (flush-output-port (current-output-port))))

  (when #f
    (doit (string->utf8
	   "<!DOCTYPE toys SYSTEM 'http://localhost/toys'>
            <toys><ball colour='red'/></toys>")))

  (when #f
    (doit (string->utf8
	   "<!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
            <toys><ball colour='red'/></toys>")))

  #t)


;;;; start and end element handlers

(when #f
  (let ()

    (define xml-utf8
      (string->utf8
       "<!-- this is a test document -->\
    <stuff>\
      <thing colour=\"yellow\">\
        <alpha>one</alpha>\
        <beta>two</beta>\
      </thing>\
      <thing>\
        <alpha>123</alpha>\
        <beta>456</beta>\
      </thing>\
    </stuff>"))

    (define (start-callback data element attributes)
      (pretty-print
       (list 'start
	     (ffi.cstring->string element)
	     (ffi.argv->strings attributes))))

    (define (end-callback data element)
      (pretty-print
       (list 'end
	     (ffi.cstring->string element))))

    (let ((parser  (XML_ParserCreate))
	  (start   (XML_StartElementHandler start-callback))
	  (end     (XML_EndElementHandler   end-callback)))
      (XML_SetElementHandler parser start end)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback start)
      (ffi.free-c-callback end)
      (flush-output-port (current-output-port)))

    #t))


;;;; character data handlers

(when #f
  (let ()

    (define xml-utf8
      (string->utf8
       "<!-- this is a test document -->\
    <stuff>\
      <thing colour=\"yellow\">\
        <alpha>one</alpha>\
        <beta>two</beta>\
      </thing>\
      <thing>\
        <alpha>123</alpha>\
        <beta>456</beta>\
      </thing>\
    </stuff>"))

    (define (text-callback data buf.ptr buf.len)
      (pretty-print
       (list 'text
	     (ffi.cstring->string buf.ptr buf.len))))

    (let ((parser  (XML_ParserCreate))
	  (text	(XML_CharacterDataHandler text-callback)))
      (XML_SetCharacterDataHandler parser text)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback text)
      (flush-output-port (current-output-port)))

    #t))


;;;; comment handlers

(when #f
  (let ()

    (define xml-utf8
      (string->utf8
       "<!-- this is a test document -->\
    <stuff>\
      <thing colour=\"yellow\">\
        <alpha>one</alpha>\
        <beta>two</beta>\
      </thing>\
      <thing>\
        <alpha>123</alpha>\
        <beta>456</beta>\
      </thing>\
    </stuff>"))

    (define (comment-callback data cstr)
      (pretty-print
       (list 'comment
	     (ffi.cstring->string cstr))))

    (let ((parser	(XML_ParserCreate))
	  (comment	(XML_CommentHandler comment-callback)))
      (XML_SetCommentHandler parser comment)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback comment)
      (flush-output-port (current-output-port)))

    #t))


;;;; cdata handlers

(when #f
  (let ()

    (define xml-utf8
      (string->utf8
       "<stuff><![CDATA[ <stuff> ]]></stuff>"))

    (define (start-cdata-callback data)
      (pretty-print '(start-cdata)))

    (define (end-cdata-callback data)
      (pretty-print '(end-cdata)))

    (define (text-callback data buf.ptr buf.len)
      (pretty-print
       (list 'text
	     (ffi.cstring->string buf.ptr buf.len))))

    (let ((parser	(XML_ParserCreate))
	  (start	(XML_StartCdataSectionHandler start-cdata-callback))
	  (end		(XML_EndCdataSectionHandler   end-cdata-callback))
	  (text		(XML_CharacterDataHandler     text-callback)))
      (XML_SetCdataSectionHandler parser start end)
      (XML_SetCharacterDataHandler parser text)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback start)
      (ffi.free-c-callback end)
      (ffi.free-c-callback text)
      (flush-output-port (current-output-port)))

    #t))


;;;; namespaces

(let ()

  (define (doit xml-utf8)
    (define (start-callback data element attributes)
      (pretty-print
       (list 'start
	     (ffi.cstring->string element)
	     (ffi.argv->strings attributes))))
    (define (end-callback data element)
      (pretty-print
       (list 'end
	     (ffi.cstring->string element))))
    (let ((parser  (XML_ParserCreateNS 'UTF-8 #\:))
	  (start   (XML_StartElementHandler start-callback))
	  (end     (XML_EndElementHandler   end-callback)))
      (XML_SetElementHandler parser start end)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback start)
      (ffi.free-c-callback end)
      (flush-output-port (current-output-port))))

  (define (doit-with-triplet xml-utf8)
    (define (start-callback data element attributes)
      (pretty-print
       (list 'start
	     (ffi.cstring->string element)
	     (ffi.argv->strings attributes))))
    (define (end-callback data element)
      (pretty-print
       (list 'end
	     (ffi.cstring->string element))))
    (let ((parser  (XML_ParserCreateNS 'UTF-8 #\:))
	  (start   (XML_StartElementHandler start-callback))
	  (end     (XML_EndElementHandler   end-callback)))
      (XML_SetReturnNSTriplet parser #t)
      (XML_SetElementHandler parser start end)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback start)
      (ffi.free-c-callback end)
      (flush-output-port (current-output-port))))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys [
         <!ELEMENT ball EMPTY>
         <!ATTLIST ball colour CDATA #REQUIRED>
       ]>
       <toys xmlns:blue='http://localhost/blue'
             xmlns:red='http://localhost/red'>
       <blue:ball colour='yellow'/>
       <red:ball  colour='purple'/>
       </toys>")))

  (when #f
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys [
         <!ELEMENT ball EMPTY>
         <!ATTLIST ball colour CDATA #REQUIRED>
       ]>
       <toys xmlns='http://localhost/blue'>
         <ball colour='yellow'/>
         <ball  colour='purple'/>
       </toys>")))

  (when #f
    (doit-with-triplet
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys [
         <!ELEMENT ball EMPTY>
         <!ATTLIST ball colour CDATA #REQUIRED>
       ]>
       <toys xmlns:blue='http://localhost/blue'
             xmlns:red='http://localhost/red'>
       <blue:ball colour='yellow'/>
       <red:ball  colour='purple'/>
       </toys>")))

  #f)


;;;; namespace handlers

(let ()
  (define (start-element-callback data element attributes)
    (pretty-print
     (list 'element-start
	   (ffi.cstring->string element)
	   (ffi.argv->strings attributes))))

  (define (end-element-callback data element)
    (pretty-print
     (list 'element-end
	   (ffi.cstring->string element))))

  (define (start-xmlns-callback data prefix uri)
    (pretty-print
     (list 'xmlns-start
	   (or (ffi.pointer-null? prefix) (ffi.cstring->string prefix))
	   (or (ffi.pointer-null? uri)    (ffi.cstring->string uri)))))

  (define (end-xmlns-callback data prefix)
    (pretty-print
     (list 'xmlns-end
	   (or (ffi.pointer-null? prefix) (ffi.cstring->string prefix)))))

  (define (doit xml-utf8)
    (let ((parser	(XML_ParserCreateNS 'UTF-8 #\:))
	  (start-elm	(XML_StartElementHandler	start-element-callback))
	  (end-elm	(XML_EndElementHandler		end-element-callback))
	  (start-ns	(XML_StartNamespaceDeclHandler	start-xmlns-callback))
	  (end-ns	(XML_EndNamespaceDeclHandler	end-xmlns-callback)))
      (XML_SetElementHandler		parser start-elm end-elm)
      (XML_SetNamespaceDeclHandler	parser start-ns  end-ns)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback start-elm)
      (ffi.free-c-callback end-elm)
      (ffi.free-c-callback start-ns)
      (ffi.free-c-callback end-ns)
      (flush-output-port (current-output-port))))

  (when #f	;some namespaces
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys [
         <!ELEMENT ball EMPTY>
         <!ATTLIST ball colour CDATA #REQUIRED>
       ]>
       <toys xmlns:blue='http://localhost/blue'
             xmlns:red='http://localhost/red'>\
         <blue:ball colour='yellow'/>\
         <red:ball  colour='purple'/>\
       </toys>")))

  (when #f	;default namespace
    (doit
     (string->utf8
      "<?xml version='1.0'?>
       <!DOCTYPE toys [
         <!ELEMENT ball EMPTY>
         <!ATTLIST ball colour CDATA #REQUIRED>
       ]>
       <toys xmlns='http://localhost/blue'>
         <ball colour='yellow'/>
         <ball  colour='purple'/>
       </toys>")))

  #t)


;;;; default handler

(let ()

  (define (doit xml-utf8)
    (define (start-callback data element attributes)
      (XML_DefaultCurrent data))
    (define (end-callback data element)
      (XML_DefaultCurrent data))
    (define (default-callback user-data buf.ptr buf.len)
      (pretty-print (list 'default (ffi.cstring->string buf.ptr buf.len))))
    (let ((parser	(XML_ParserCreateNS 'UTF-8 #\:))
	  (start	(XML_StartElementHandler start-callback))
	  (end		(XML_EndElementHandler   end-callback))
	  (default	(XML_DefaultHandler      default-callback)))
      (XML_UseParserAsHandlerArg parser)
      (XML_SetElementHandler parser start end)
      (XML_SetDefaultHandler parser default)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback start)
      (ffi.free-c-callback end)
      (ffi.free-c-callback default)
      (flush-output-port (current-output-port))))

  (when #f
    (doit (string->utf8 "<toys><ball colour='yellow'/></toys>")))

  #t)


;;;; skipped entity handler

(let ()

  (define (doit xml)
    (let ((xml-utf8	(string->utf8 xml))
	  (parser	(XML_ParserCreate))
	  (skip-ent	(XML_SkippedEntityHandler skipped-entity-callback)))
      (XML_SetSkippedEntityHandler parser skip-ent)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback skip-ent)
      (flush-output-port (current-output-port))))

  (define (skipped-entity-callback data entity-name is-parameter-entity)
    (pretty-print
     (list 'skipped-entity
	   (ffi.cstring->string entity-name)
	   (fxpositive? is-parameter-entity))))

;;; --------------------------------------------------------------------

  (when #f
    (doit "<?xml version='1.0' standalone='no'?>
           <!DOCTYPE thing SYSTEM 'http://localhost/thing'>
           <thing>&ciao;</thing>"))

  #t)


;;;; processing instruction handler

(let ()

  (define (doit xml)
    (let ((xml-utf8	(string->utf8 xml))
	  (parser	(XML_ParserCreate))
	  (proc-inst	(XML_ProcessingInstructionHandler processing-instruction-callback)))
      (XML_SetProcessingInstructionHandler parser proc-inst)
      (XML_Parse parser xml-utf8 #f #t)
      (ffi.free-c-callback proc-inst)
      (flush-output-port (current-output-port))))

  (define (processing-instruction-callback user-data target data)
    (pretty-print
     (list 'processing-instruction
	   (ffi.cstring->string target)
	   (ffi.cstring->string data))))

;;; --------------------------------------------------------------------

  (when #f
    (doit "<?xml version='1.0' standalone='no'?>
           <!DOCTYPE thing SYSTEM 'http://localhost/thing'>
           <thing><?scheme (display 123) ?></thing>"))

  #t)

;;; end of file
