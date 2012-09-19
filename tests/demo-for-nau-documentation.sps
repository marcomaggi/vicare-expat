;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: tests to be included in the documentation
;;;Date: Mon Feb 20, 2012
;;;
;;;Abstract
;;;
;;;	Documentation tests for the Nausicaa front end.
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
(import (nausicaa)
  (nausicaa xml expat)
  (prefix (vicare language-extensions)
	  ik.)
  (only (vicare expat)
	pointer->XML_Content
	XML_FreeContentModel))


;;;; simple document parsing

(let ()

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.start-element-handler  start-callback)
      (P.end-element-handler    end-callback)
      (P.character-data-handler cdata-callback)
      (P.comment-handler        comment-callback)
      (P.parse (string->utf8 xml) #f #t)))

  (define (start-callback data element attributes)
    (let ((element    (ik.cstring->string element))
	  (attributes (ik.argv->strings attributes)))
      (pretty-print (list 'start element attributes))))

  (define (end-callback data element)
    (let ((element (ik.cstring->string element)))
      (pretty-print (list 'end element))))

  (define (cdata-callback data buf.ptr buf.len)
    (let ((text (ik.cstring->string buf.ptr buf.len)))
      (pretty-print (list 'cdata text))))

  (define (comment-callback data cstr)
    (let ((text (ik.cstring->string cstr)))
      (pretty-print (list 'comment text))))

  (when #t
    (doit
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
      </stuff>")
    (flush-output-port (current-output-port)))

  #f)


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
	   (or (ik.pointer-null? version)  (ik.cstring->string version))
	   (or (ik.pointer-null? encoding) (ik.cstring->string encoding))
	   (%process-standalone standalone))))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.xml-decl-handler xml-decl-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------

  (define dtd
    "<?xml version='1.0' encoding='utf-8'?>
     <!ELEMENT ball EMPTY>
     <!ATTLIST ball colour CDATA #REQUIRED>")

  (define (external-entity-callback root-parser context base system-id public-id)
    (pretty-print
     (list 'external-entity
	   (or (ik.pointer-null? context)     (ik.cstring->string context))
	   (or (ik.pointer-null? base)        (ik.cstring->string base))
	   (ik.cstring->string system-id)
	   (or (ik.pointer-null? public-id)   (ik.cstring->string public-id))))
    (let (((E <expat-entity-parser>) (make <expat-entity-parser>
				       root-parser context)))
      (E.xml-decl-handler xml-decl-callback)
      (E.parse (string->utf8 dtd) #f #t)))

  (define (doit-with-external-entity xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.xml-decl-handler xml-decl-callback)
      (P.external-entity-ref-handler external-entity-callback)
      (P.set-param-entity-parsing XML_PARAM_ENTITY_PARSING_ALWAYS)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------
;;; XML declaration

  (when #f
    (doit "<?xml version='1.0'?><toys><ball colour='red'/></toys>"))

  (when #f
    (doit "<?xml version='1.0' encoding='utf-8'?><toys><ball colour='red'/></toys>"))

  (when #f
    (doit "<?xml version='1.0' standalone='yes'?><toys><ball colour='red'/></toys>"))

  (when #f
    (doit "<?xml version='1.0' standalone='no'?><toys><ball colour='red'/></toys>"))

;;; --------------------------------------------------------------------
;;; external entity

  (when #f
    (doit-with-external-entity
     "<?xml version='1.0'?>
      <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
      <toys><ball colour='red'/></toys>"))

  #t)


;;;; DTD doctype handler

(let ()

  (define (start-doctype-callback data doctype-name sysid pubid has-internal-subset)
    (pretty-print
     (list 'doctype-start
	   (ik.cstring->string doctype-name)
	   (or (ik.pointer-null? sysid) (ik.cstring->string sysid))
	   (or (ik.pointer-null? pubid) (ik.cstring->string pubid))
	   has-internal-subset)))

  (define (end-doctype-callback data)
    (pretty-print '(doctype-end)))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.start-doctype-decl-handler start-doctype-callback)
      (P.end-doctype-decl-handler   end-doctype-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (when #f
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
      <toys><ball colour='yellow'/></toys>"))

  (when #f
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
      <toys><ball colour='yellow'/></toys>"))

  (when #f
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys [
        <!ELEMENT ball EMPTY>
        <!ATTLIST ball colour CDATA #REQUIRED>
      ]>
      <toys><ball colour='yellow'/></toys>"))

  #t)


;;;; DTD element declaration handler

(let ()

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.element-decl-handler dtd-elm-callback)
      (P.use-parser-as-handler-arg)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (define (dtd-elm-callback data name model)
    (pretty-print
     (list 'dtd-element
	   (ik.cstring->string name)
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
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.attlist-decl-handler dtd-attlist-callback)
      (P.start-element-handler elm-start-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (define (dtd-attlist-callback user-data element-name attribute-name
				attribute-type default-value required?)
    (pretty-print
     (list 'dtd-attlist
	   (ik.cstring->string element-name)
	   (ik.cstring->string attribute-name)
	   (ik.cstring->string attribute-type)
	   (if (ik.pointer-null? default-value)
	       'no-value
	     (ik.cstring->string default-value))
	   (fxpositive? required?))))

  (define (elm-start-callback data element attributes)
    (pretty-print
     (list 'element-start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

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
    (if (ik.pointer-null? thing)
	#f
      (ik.cstring->string thing)))

  (define (notation-callback data notation-name base system-id public-id)
    (pretty-print
     (list 'notation
	   (%false-or-string notation-name)
	   (%false-or-string base)
	   (%false-or-string system-id)
	   (%false-or-string public-id))))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.notation-decl-handler notation-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------

  (when #f
    (doit "<?xml version='1.0'?>
           <!DOCTYPE toys [
             <!NOTATION bouncing SYSTEM 'http://localhost/bouncer'>
             <!ELEMENT ball EMPTY>
             <!ATTLIST ball colour CDATA #REQUIRED>
           ]>
           <toys><ball colour='red' /></toys>"))

  (when #f
    (doit "<?xml version='1.0'?>
           <!DOCTYPE toys [
             <!NOTATION bouncing PUBLIC 'The Bouncer'>
             <!ELEMENT ball EMPTY>
             <!ATTLIST ball colour CDATA #REQUIRED>
           ]>
           <toys><ball colour='red' /></toys>"))

  (when #f
    (doit "<?xml version='1.0'?>
           <!DOCTYPE toys [
             <!NOTATION bouncing PUBLIC 'The Bouncer' 'http://localhost/bouncer'>
             <!ELEMENT ball EMPTY>
             <!ATTLIST ball colour CDATA #REQUIRED>
           ]>
           <toys><ball colour='red' /></toys>"))

  #t)


;;;; DTD entity handler

(let ()

  (define (%false-or-string thing)
    (if (ik.pointer-null? thing)
	#f
      (ik.cstring->string thing)))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.entity-decl-handler dtd-entity-callback)
      (P.start-element-handler elm-start-callback)
      (P.set-base (string->utf8 "http://localhost/"))
      (P.set-param-entity-parsing XML_PARAM_ENTITY_PARSING_ALWAYS)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (define (dtd-entity-callback data entity-name is-parameter-entity
			       value value-length
			       base system-id public-id
			       notation-name)
    (pretty-print
     (list 'dtd-entity
	   (%false-or-string entity-name)
	   (fxpositive? is-parameter-entity)
	   (if (ik.pointer-null? value)
	       #f
	     (ik.cstring->string value value-length))
	   (%false-or-string base)
	   (%false-or-string system-id)
	   (%false-or-string public-id)
	   (%false-or-string notation-name))))

  (define (elm-start-callback data element attributes)
    (pretty-print
     (list 'element-start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

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

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.xml-decl-handler xml-decl-callback)
      (P.not-standalone-handler not-stand-callback)
      (P.start-doctype-decl-handler start-doctype-callback)
      (P.end-doctype-decl-handler end-doctype-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------

  (when #f
    (doit
     "<?xml version='1.0'?>
      <toys><ball colour='red'/></toys>"))

  (when #f
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
      <toys><ball colour='red'/></toys>"))

  (when #f
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
      <toys><ball colour='red'/></toys>"))

;;; --------------------------------------------------------------------

  (when #f
    (doit
     "<?xml version='1.0' standalone='no'?>
      <toys><ball colour='red'/></toys>"))

  (when #f
    (doit
     "<?xml version='1.0' standalone='no'?>
      <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
      <toys><ball colour='red'/></toys>"))

  (when #f
    (doit
     "<?xml version='1.0' standalone='no'?>
      <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
      <toys><ball colour='red'/></toys>"))

;;; --------------------------------------------------------------------

  (when #f
    (doit
     "<?xml version='1.0' standalone='yes'?>
      <toys><ball colour='red'/></toys>"))

  (when #f
    (doit
     "<?xml version='1.0' standalone='yes'?>
      <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
      <toys><ball colour='red'/></toys>"))

  (when #f
    (doit
     "<?xml version='1.0' standalone='yes'?>
      <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
      <toys><ball colour='red'/></toys>"))

  #t)


;;;; external entity parsing

(let ()

  (define (ext-callback root-parser context base system-id public-id)
    (let* (((E <expat-entity-parser>) (make <expat-entity-parser>
					root-parser context))
	   (rv     (E.parse (string->utf8 dtd) #f #t)))
      (pretty-print
       (list 'external-entity rv
	     (ik.pointer-null? context)
	     (ik.pointer-null? base)
	     (ik.cstring->string system-id)
	     (ik.pointer-null? public-id)))
      XML_STATUS_OK))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.external-entity-ref-handler ext-callback)
      (P.set-param-entity-parsing XML_PARAM_ENTITY_PARSING_ALWAYS)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

;;; --------------------------------------------------------------------

  (define dtd
    "<!ELEMENT ball EMPTY>
     <!ATTLIST ball colour CDATA #REQUIRED>")

  (when #f
    (doit "<!DOCTYPE toys SYSTEM 'http://localhost/toys'>
           <toys><ball colour='red'/></toys>"))

  (when #f
    (doit "<!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
           <toys><ball colour='red'/></toys>"))

  #t)


;;;; start and end element handlers

(let ()

  (define xml
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
     </stuff>")

  (define (start-callback data element attributes)
    (pretty-print
     (list 'start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

  (define (end-callback data element)
    (pretty-print
     (list 'end
	   (ik.cstring->string element))))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.start-element-handler start-callback)
      (P.end-element-handler   end-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (when #f
    (doit xml))

  #t)


;;;; character data handlers

(let ()

  (define xml
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
     </stuff>")

  (define (text-callback data buf.ptr buf.len)
    (pretty-print
     (list 'text
	   (ik.cstring->string buf.ptr buf.len))))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.character-data-handler text-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (when #f
    (doit xml))

  #t)


;;;; comment handlers

(let ()

  (define xml
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
     </stuff>")

  (define (comment-callback data cstr)
    (pretty-print
     (list 'comment
	   (ik.cstring->string cstr))))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.comment-handler comment-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (when #f
    (doit xml))

  #t)


;;;; cdata handlers

(let ()

  (define xml
    "<stuff><![CDATA[ <stuff> ]]></stuff>")

  (define (start-cdata-callback data)
    (pretty-print '(start-cdata)))

  (define (end-cdata-callback data)
    (pretty-print '(end-cdata)))

  (define (text-callback data buf.ptr buf.len)
    (pretty-print
     (list 'text
	   (ik.cstring->string buf.ptr buf.len))))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.start-cdata-section-handler start-cdata-callback)
      (P.end-cdata-section-handler   end-cdata-callback)
      (P.character-data-handler      text-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (when #f
    (doit xml))

  #t)


;;;; namespaces

(let ()

  (define (start-callback data element attributes)
    (pretty-print
     (list 'start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

  (define (end-callback data element)
    (pretty-print
     (list 'end
	   (ik.cstring->string element))))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-ns-parser>)))
      (P.start-element-handler start-callback)
      (P.end-element-handler   end-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (when #f
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys [
        <!ELEMENT ball EMPTY>
        <!ATTLIST ball colour CDATA #REQUIRED>
      ]>
      <toys xmlns:blue='http://localhost/blue'
            xmlns:red='http://localhost/red'>
      <blue:ball colour='yellow'/>
      <red:ball  colour='purple'/>
      </toys>"))

  (when #f
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys [
        <!ELEMENT ball EMPTY>
        <!ATTLIST ball colour CDATA #REQUIRED>
      ]>
      <toys xmlns='http://localhost/blue'>
        <ball colour='yellow'/>
         <ball  colour='purple'/>
      </toys>"))

  #f)

;;; --------------------------------------------------------------------

(let ()

  (define (start-callback data element attributes)
    (pretty-print
     (list 'start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

  (define (end-callback data element)
    (pretty-print
     (list 'end
	   (ik.cstring->string element))))

  (define (doit-with-triplet xml)
    (let (((P <expat-ns-parser>) (make <expat-ns-parser>)))
      (P.start-element-handler start-callback)
      (P.end-element-handler   end-callback)
      (P.set-return-ns-triplet #t)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (when #f
    (doit-with-triplet
     "<?xml version='1.0'?>
      <!DOCTYPE toys [
        <!ELEMENT ball EMPTY>
        <!ATTLIST ball colour CDATA #REQUIRED>
      ]>
      <toys xmlns:blue='http://localhost/blue'
            xmlns:red='http://localhost/red'>
      <blue:ball colour='yellow'/>
      <red:ball  colour='purple'/>
      </toys>"))

  #f)


;;;; namespace handlers

(let ()
  (define (start-element-callback data element attributes)
    (pretty-print
     (list 'element-start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

  (define (end-element-callback data element)
    (pretty-print
     (list 'element-end
	   (ik.cstring->string element))))

  (define (start-xmlns-callback data prefix uri)
    (pretty-print
     (list 'xmlns-start
	   (or (ik.pointer-null? prefix) (ik.cstring->string prefix))
	   (or (ik.pointer-null? uri)    (ik.cstring->string uri)))))

  (define (end-xmlns-callback data prefix)
    (pretty-print
     (list 'xmlns-end
	   (or (ik.pointer-null? prefix) (ik.cstring->string prefix)))))

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-ns-parser>)))
      (P.start-element-handler	start-element-callback)
      (P.end-element-handler	end-element-callback)
      (P.start-namespace-decl-handler	start-xmlns-callback)
      (P.end-namespace-decl-handler	end-xmlns-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (when #f	;some namespaces
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys [
        <!ELEMENT ball EMPTY>
        <!ATTLIST ball colour CDATA #REQUIRED>
      ]>
      <toys xmlns:blue='http://localhost/blue'
            xmlns:red='http://localhost/red'>\
        <blue:ball colour='yellow'/>\
        <red:ball  colour='purple'/>\
      </toys>"))

  (when #f	;default namespace
    (doit
     "<?xml version='1.0'?>
      <!DOCTYPE toys [
        <!ELEMENT ball EMPTY>
        <!ATTLIST ball colour CDATA #REQUIRED>
      ]>
      <toys xmlns='http://localhost/blue'>
        <ball colour='yellow'/>
        <ball  colour='purple'/>
      </toys>"))

  #t)


;;;; skipped entity handler

(let ()

  (define (doit xml)
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.skipped-entity-handler skipped-entity-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (define (skipped-entity-callback data entity-name is-parameter-entity)
    (pretty-print
     (list 'skipped-entity
	   (ik.cstring->string entity-name)
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
    (let (((P <expat-parser>) (make <expat-parser>)))
      (P.processing-instruction-handler processing-instruction-callback)
      (P.parse (string->utf8 xml) #f #t)
      (flush-output-port (current-output-port))))

  (define (processing-instruction-callback user-data target data)
    (pretty-print
     (list 'processing-instruction
	   (ik.cstring->string target)
	   (ik.cstring->string data))))

;;; --------------------------------------------------------------------

  (when #f
    (doit "<?xml version='1.0' standalone='no'?>
           <!DOCTYPE thing SYSTEM 'http://localhost/thing'>
           <thing><?scheme (display 123) ?></thing>"))

  #t)

;;; end of file
