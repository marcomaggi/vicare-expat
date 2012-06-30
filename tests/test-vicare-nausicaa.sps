;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: nausicaa front end
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
(import (nausicaa)
  (nausicaa xml expat)
  (prefix (vicare language-extensions)
	  ik.)
;;;  (prefix (vicare expat) expat.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa front end\n")


(parametrise ((check-test-name	'parsing-basic))

  (define xml-1
    (string->utf8 "<!-- this is a test document -->\
     <stuff>\
     <thing>\
     <alpha>one</alpha>\
     <beta>two</beta>\
     </thing>\
     <thing>\
     <alpha>123</alpha>\
     <beta>456</beta>\
     </thing>\
     </stuff>"))

  (check
      (with-result
       (define (start-callback data element attributes)
	 (add-result (list 'start
			   (ik.cstring->string element)
			   (ik.argv->strings attributes))))
       (define (end-callback data element)
	 (add-result (list 'end (ik.cstring->string element))))
       (define (chdata-callback data buf.ptr buf.len)
	 (add-result (list 'character-data (ik.cstring->string buf.ptr buf.len))))
       (define (comment-callback data cstr)
	 (add-result (list 'comment (ik.cstring->string cstr))))
       (let (((P <expat-parser>) (make <expat-parser>
				   (encoding: 'UTF-8))))
	 (P.start-element-handler  start-callback)
	 (P.end-element-handler    end-callback)
	 (P.character-data-handler chdata-callback)
	 (P.comment-handler        comment-callback)
	 (P.parse xml-1 #f #t)))
    => (list XML_STATUS_OK
	     '((comment " this is a test document ")
	       (start "stuff" ())
	       (start "thing" ())
	       (start "alpha" ()) (character-data "one") (end "alpha")
	       (start "beta" ()) (character-data "two")(end "beta")
	       (end "thing")
	       (start "thing" ())
	       (start "alpha" ()) (character-data "123") (end "alpha")
	       (start "beta" ()) (character-data "456") (end "beta")
	       (end "thing")
	       (end "stuff"))))

  (check	;<expat-ns-parser>
      (with-result
       (define (start-callback data element attributes)
	 (add-result (list 'start
			   (ik.cstring->string element)
			   (ik.argv->strings attributes))))
       (define (end-callback data element)
	 (add-result (list 'end (ik.cstring->string element))))
       (define (chdata-callback data buf.ptr buf.len)
	 (add-result (list 'character-data (ik.cstring->string buf.ptr buf.len))))
       (define (comment-callback data cstr)
	 (add-result (list 'comment (ik.cstring->string cstr))))
       (let (((P <expat-ns-parser>) (make <expat-ns-parser>
				      (encoding: 'UTF-8)
				      (namespace-separator: #\,))))
	 (P.start-element-handler  start-callback)
	 (P.end-element-handler    end-callback)
	 (P.character-data-handler chdata-callback)
	 (P.comment-handler        comment-callback)
	 (P.parse xml-1 #f #t)))
    => (list XML_STATUS_OK
	     '((comment " this is a test document ")
	       (start "stuff" ())
	       (start "thing" ())
	       (start "alpha" ()) (character-data "one") (end "alpha")
	       (start "beta" ()) (character-data "two")(end "beta")
	       (end "thing")
	       (start "thing" ())
	       (start "alpha" ()) (character-data "123") (end "alpha")
	       (start "beta" ()) (character-data "456") (end "beta")
	       (end "thing")
	       (end "stuff"))))

  (check	;parsing with internal buffer
      (with-result
       (define (start-callback data element attributes)
	 (add-result (list 'start
			   (ik.cstring->string element)
			   (ik.argv->strings attributes))))
       (define (end-callback data element)
	 (add-result (list 'end (ik.cstring->string element))))
       (define (chdata-callback data buf.ptr buf.len)
	 (add-result (list 'character-data (ik.cstring->string buf.ptr buf.len))))
       (define (comment-callback data cstr)
	 (add-result (list 'comment (ik.cstring->string cstr))))
       (let (((P <expat-parser>) (make <expat-parser>)))
	 (P.start-element-handler  start-callback)
	 (P.end-element-handler    end-callback)
	 (P.character-data-handler chdata-callback)
	 (P.comment-handler        comment-callback)
	 (let* ((buflen		(bytevector-length xml-1))
		(buffer		(P.get-buffer buflen)))
	   (ik.memory-copy buffer 0 xml-1 0 buflen)
	   (P.parse-buffer buflen #t))))
    => (list XML_STATUS_OK
	     '((comment " this is a test document ")
	       (start "stuff" ())
	       (start "thing" ())
	       (start "alpha" ()) (character-data "one") (end "alpha")
	       (start "beta" ()) (character-data "two")(end "beta")
	       (end "thing")
	       (start "thing" ())
	       (start "alpha" ()) (character-data "123") (end "alpha")
	       (start "beta" ()) (character-data "456") (end "beta")
	       (end "thing")
	       (end "stuff"))))

  (ik.collect))


(parametrise ((check-test-name	'namespaces))

  (define (doit xml-utf8)
    (with-result
     (define (start-callback data element attributes)
       (add-result (list 'start
			 (ik.cstring->string element)
			 (ik.argv->strings attributes))))
     (define (end-callback data element)
       (add-result (list 'end (ik.cstring->string element))))
     (let (((P <expat-ns-parser>) (make <expat-ns-parser>)))
       (P.start-element-handler  start-callback)
       (P.end-element-handler    end-callback)
       (P.parse xml-utf8 #f #t))))

  (check	;some namespaces
      (doit (string->utf8
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
    => (list XML_STATUS_OK
	     '((start "toys" ())
	       (start "http://localhost/blue:ball" ("colour" "yellow"))
	       (end "http://localhost/blue:ball")
	       (start "http://localhost/red:ball" ("colour" "purple"))
	       (end "http://localhost/red:ball")
	       (end "toys"))))

  (check	;default namespace
      (doit (string->utf8
	     "<?xml version='1.0'?>
              <!DOCTYPE toys [
                <!ELEMENT ball EMPTY>
                <!ATTLIST ball colour CDATA #REQUIRED>
              ]>
             <toys xmlns='http://localhost/blue'>
               <ball colour='yellow'/>
               <ball  colour='purple'/>
             </toys>"))
    => (list XML_STATUS_OK
	     '((start "http://localhost/blue:toys" ())
	       (start "http://localhost/blue:ball" ("colour" "yellow"))
	       (end "http://localhost/blue:ball")
	       (start "http://localhost/blue:ball" ("colour" "purple"))
	       (end "http://localhost/blue:ball")
	       (end "http://localhost/blue:toys"))))

  #t)


(parametrise ((check-test-name	'stop-and-resume))

  (define xml-utf8
    (string->utf8
     "<?xml version='1.0'?>
      <!DOCTYPE toys [
        <!ELEMENT ball EMPTY>
        <!ATTLIST ball colour CDATA #REQUIRED>
      ]>
      <toys><ball colour='red'/></toys>"))

  (check	;stopping with NO resuming
      (with-result
       (define (start-callback dummy element attributes)
	 (add-result (list 'start
			   (ik.cstring->string element)
			   (ik.argv->strings attributes)))
	 (let* (((P <expat-parser>) (current-expat-parser))
		((status <expat-parsing-status>) (P.get-parsing-status)))
	   (unless (= XML_STATUS_SUSPENDED status.parsing)
	     (add-result (list 'stop-rv (P.stop-parser #f))))))
       (let (((P <expat-parser>) (make <expat-parser>)))
	 (P.start-element-handler start-callback)
	 (let ((rv (P.parse xml-utf8 #f #t)))
	   (add-result (P.get-error-code))
	   rv)))
    => `(,XML_STATUS_ERROR ((start "toys" ())
			    (stop-rv ,XML_STATUS_OK)
			    ,XML_ERROR_ABORTED)))

  (check	;stopping with resuming
      (with-result
       (define (start-callback-and-suspend dummy element attributes)
	 (add-result (list 'start
			   (ik.cstring->string element)
			   (ik.argv->strings attributes)))
	 (let (((P <expat-parser>) (current-expat-parser)))
	   (add-result (list 'stop-rv (P.stop-parser #t)))))
       (define (start-callback parser element attributes)
	 (add-result (list 'start
			   (ik.cstring->string element)
			   (ik.argv->strings attributes))))
       (let (((P <expat-parser>) (make <expat-parser>)))
	 (P.start-element-handler start-callback-and-suspend)
	 (let ((rv (P.parse xml-utf8 #f #t)))
	   (add-result (list 'suspended rv))
	   (P.start-element-handler start-callback)
	   (P.resume-parser))))
    => `(,XML_STATUS_OK ((start "toys" ())
			 (stop-rv ,XML_STATUS_OK)
			 (suspended ,XML_STATUS_SUSPENDED)
			 (start "ball" ("colour" "red")))))

  (ik.collect))


(parametrise ((check-test-name	'parser-misc))

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.reset))
    => #t)

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.reset 'UTF-8))
    => #t)

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.set-user-data P.parser)
	(ik.pointer=? P.parser (P.get-user-data)))
    => #t)

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.use-parser-as-handler-arg)
	#t)
    => #t)

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.set-base '#vu8(1 2 3 4))
	(P.get-base))
    => '#vu8(1 2 3 4))

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.set-base #f)
	(P.get-base))
    => #f)

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.set-encoding 'UTF-8))
    => XML_STATUS_OK)

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.parse (string->utf8 "<alpha>") #f #f)
	(guard (E ((is-a? &expat-error (with-class &who))
		   E.who)
		  (else E))
	  (P.set-encoding 'US-ASCII)))
    => '<expat-parser>.set-encoding)

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.use-foreign-dtd #t))
    => XML_ERROR_NONE)

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
	(P.use-foreign-dtd #f))
    => XML_ERROR_NONE)

  (ik.collect))


(parametrise ((check-test-name	'xml-decl-handler))

  (define (%process-standalone standalone)
    (case standalone
      ((-1)	'unspecified)
      ((0)	'non-standalone)
      ((1)	'standalone)
      (else	#f)))

  (define (xml-decl-callback user-data version encoding standalone)
    (add-result
     (list 'xml-decl
	   (or (ik.pointer-null? version)  (ik.cstring->string version))
	   (or (ik.pointer-null? encoding) (ik.cstring->string encoding))
	   (%process-standalone standalone))))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.xml-decl-handler xml-decl-callback)
       (P.parse (string->utf8 xml) #f #t))))

;;; --------------------------------------------------------------------

  (define dtd-utf8
    (string->utf8
     "<?xml version='1.0' encoding='utf-8'?>
      <!ELEMENT ball EMPTY>
      <!ATTLIST ball colour CDATA #REQUIRED>"))

  (define (external-entity-callback root-parser context base system-id public-id)
    (add-result
     (list 'external-entity
	   (or (ik.pointer-null? context)     (ik.cstring->string context))
	   (or (ik.pointer-null? base)        (ik.cstring->string base))
	   (ik.cstring->string system-id)
	   (or (ik.pointer-null? public-id)   (ik.cstring->string public-id))))
    (let (((E <expat-entity-parser>) (make <expat-entity-parser>
				       root-parser context)))
      (E.xml-decl-handler xml-decl-callback)
      (E.parse dtd-utf8 #f #t)))

  (define (doit-with-external-entity xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.xml-decl-handler xml-decl-callback)
       (P.external-entity-ref-handler external-entity-callback)
       (P.set-param-entity-parsing XML_PARAM_ENTITY_PARSING_ALWAYS)
       (P.parse (string->utf8 xml) #f #t))))

;;; --------------------------------------------------------------------

  (check
      (doit "<?xml version='1.0'?><toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" #t unspecified))))

  (check
      (doit "<?xml version='1.0' encoding='utf-8'?><toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" "utf-8" unspecified))))

  (check
      (doit "<?xml version='1.0' standalone='yes'?><toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" #t standalone))))

  (check
      (doit "<?xml version='1.0' standalone='no'?><toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" #t non-standalone))))

;;; --------------------------------------------------------------------

  (check
      (doit-with-external-entity
       "<?xml version='1.0'?>
        <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" #t unspecified)
	  (external-entity #t #t "http://localhost/toys" #t)
	  (xml-decl "1.0" "utf-8" unspecified))))

  (ik.collect))


(parametrise ((check-test-name	'non-standalone-handler))

  (define (%process-standalone standalone)
    (case standalone
      ((-1)	'unspecified)
      ((0)	'non-standalone)
      ((1)	'standalone)
      (else	#f)))

  (define (xml-decl-callback user-data version encoding standalone)
    (add-result
     (list 'xml-decl (%process-standalone standalone))))

  (define (not-stand-callback user-data)
    (add-result '(not-standalone))
    XML_STATUS_OK)

  (define (start-doctype-callback data doctype-name sysid pubid has-internal-subset)
    (add-result
     (list 'doctype-start has-internal-subset)))

  (define (end-doctype-callback data)
    (add-result '(doctype-end)))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.xml-decl-handler xml-decl-callback)
       (P.not-standalone-handler not-stand-callback)
       (P.start-doctype-decl-handler start-doctype-callback)
       (P.end-doctype-decl-handler end-doctype-callback)
       (P.parse (string->utf8 xml) #f #t))))

;;; --------------------------------------------------------------------

  (check
      (doit
       "<?xml version='1.0'?>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl unspecified))))

  (check
      (doit
       "<?xml version='1.0'?>
        <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl unspecified)
	  (not-standalone)
	  (doctype-start 0)
	  (doctype-end))))

  (check
      (doit
       "<?xml version='1.0'?>
        <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl unspecified)
	  (not-standalone)
	  (doctype-start 0)
	  (doctype-end))))

;;; --------------------------------------------------------------------

  (check
      (doit
       "<?xml version='1.0' standalone='no'?>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl non-standalone))))

  (check
      (doit
       "<?xml version='1.0' standalone='no'?>
        <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl non-standalone)
	  (not-standalone)
	  (doctype-start 0)
	  (doctype-end))))

  (check
      (doit
       "<?xml version='1.0' standalone='no'?>
        <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl non-standalone)
	  (not-standalone)
	  (doctype-start 0)
	  (doctype-end))))

;;; --------------------------------------------------------------------

  (check
      (doit
       "<?xml version='1.0' standalone='yes'?>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl standalone))))

  (check
      (doit
       "<?xml version='1.0' standalone='yes'?>
        <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl standalone)
	  (doctype-start 0)
	  (doctype-end))))

  (check
      (doit
       "<?xml version='1.0' standalone='yes'?>
        <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
        <toys><ball colour='red'/></toys>")
    => `(,XML_STATUS_OK
	 ((xml-decl standalone)
	  (doctype-start 0)
	  (doctype-end))))

  (ik.collect))


(parametrise ((check-test-name	'dtd-doctype-handler))

  (define (start-doctype-callback data doctype-name sysid pubid has-internal-subset)
    (add-result
     (list 'doctype-start
	   (ik.cstring->string doctype-name)
	   (or (ik.pointer-null? sysid) (ik.cstring->string sysid))
	   (or (ik.pointer-null? pubid) (ik.cstring->string pubid))
	   has-internal-subset)))

  (define (end-doctype-callback data)
    (add-result '(doctype-end)))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.start-doctype-decl-handler start-doctype-callback)
       (P.end-doctype-decl-handler   end-doctype-callback)
       (P.parse (string->utf8 xml) #f #t))))

  (check
      (doit
       "<?xml version='1.0'?>
        <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
        <toys><ball colour='yellow'/></toys>")
    => (list XML_STATUS_OK
	     '((doctype-start "toys" "http://localhost/toys" #t 0)
	       (doctype-end))))

  (check
      (doit
       "<?xml version='1.0'?>
        <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
        <toys><ball colour='yellow'/></toys>")
    => (list XML_STATUS_OK
	     '((doctype-start "toys" "http://localhost/toys" "The Toys" 0)
	       (doctype-end))))

  (check
      (doit
       "<?xml version='1.0'?>
        <!DOCTYPE toys [
          <!ELEMENT ball EMPTY>
          <!ATTLIST ball colour CDATA #REQUIRED>
        ]>
        <toys><ball colour='yellow'/></toys>")
    => (list XML_STATUS_OK
	     '((doctype-start "toys" #t #t 1)
	       (doctype-end))))

  (ik.collect))


(parametrise ((check-test-name	'external-entity-parser))

  (define xml
    "<!DOCTYPE toys SYSTEM 'http://localhost/toys'>
     <toys><ball colour='red'/></toys>")

  (define dtd
    "<!ELEMENT ball EMPTY>
     <!ATTLIST ball colour CDATA #REQUIRED>")

  (define (scheme-callback root-parser context base system-id public-id)
    (let* (((E <expat-entity-parser>) (make <expat-entity-parser>
					root-parser context))
	   (rv     (E.parse (string->utf8 dtd) #f #t)))
      (add-result (list 'external-entity rv
			(ik.pointer-null? context)
			(ik.pointer-null? base)
			(ik.cstring->string system-id)
			(ik.pointer-null? public-id)))
      XML_STATUS_OK))

  (define (doit)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.external-entity-ref-handler scheme-callback)
       (P.set-param-entity-parsing XML_PARAM_ENTITY_PARSING_ALWAYS)
       (P.parse (string->utf8 xml) #f #t))))

;;; --------------------------------------------------------------------

  (check
      (doit)
    => `(,XML_STATUS_OK
	 ((external-entity ,XML_STATUS_OK #t #t "http://localhost/toys" #t))))

  (ik.collect))


#;(parametrise ((check-test-name	'dtd-element-handler))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.element-decl-handler dtd-elm-callback)
       (P.use-parser-as-handler-arg)
       (P.parse (string->utf8 xml) #f #t))))

  (define (dtd-elm-callback data name model)
    (add-result
     (list 'dtd-element
	   (ik.cstring->string name)
	   (expat.XML_Content->list (expat.pointer->XML_Content model))))
    (expat.XML_FreeContentModel data model))

;;; --------------------------------------------------------------------

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball>
             ]>
             <toys><ball/></toys>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "ball" (,XML_CTYPE_EMPTY ,XML_CQUANT_NONE #f 0 #f)))))

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball ANY>
               <!ATTLIST ball>
             ]>
             <toys><ball/></toys>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "ball" (,XML_CTYPE_ANY ,XML_CQUANT_NONE #f 0 #f)))))

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT toys (ball)>
               <!ATTLIST toys>
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball>
             ]>
             <toys><ball/></toys>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "toys" ( ;;
			       ,XML_CTYPE_SEQ ,XML_CQUANT_NONE #f 1
			       #((,XML_CTYPE_NAME ,XML_CQUANT_NONE "ball" 0 #f))))
	  (dtd-element "ball" (,XML_CTYPE_EMPTY ,XML_CQUANT_NONE #f 0 #f)))))

  (check
      (doit "<!DOCTYPE outer [
               <!ELEMENT outer (middle)>
               <!ATTLIST outer>
               <!ELEMENT middle (inner)>
               <!ATTLIST middle>
               <!ELEMENT inner EMPTY>
               <!ATTLIST inner>
             ]>
             <outer><middle><inner/></middle></outer>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "outer" ( ;;
				,XML_CTYPE_SEQ ,XML_CQUANT_NONE #f 1
				#((,XML_CTYPE_NAME ,XML_CQUANT_NONE "middle" 0 #f))))
	  (dtd-element "middle" ( ;;
				 ,XML_CTYPE_SEQ ,XML_CQUANT_NONE #f 1
				 #((,XML_CTYPE_NAME ,XML_CQUANT_NONE "inner" 0 #f))))
	  (dtd-element "inner" (,XML_CTYPE_EMPTY ,XML_CQUANT_NONE #f 0 #f)))))

  (check
      (doit "<!DOCTYPE this [
               <!ELEMENT this (#PCDATA)>
               <!ATTLIST this>
             ]>
             <this>ciao</this>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "this" (,XML_CTYPE_MIXED ,XML_CQUANT_NONE #f 0 #f)))))

  (check
      (doit "<!DOCTYPE this [
               <!ELEMENT this (#PCDATA|that)*>
               <!ATTLIST this>
               <!ELEMENT that EMPTY>
               <!ATTLIST that>
             ]>
             <this><that/></this>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "this" ( ;;
			       ,XML_CTYPE_MIXED ,XML_CQUANT_REP #f 1
						#((,XML_CTYPE_NAME ,XML_CQUANT_NONE "that" 0 #f))))
	  (dtd-element "that" (,XML_CTYPE_EMPTY ,XML_CQUANT_NONE #f 0 #f)))))

;;; --------------------------------------------------------------------
;;; quantifiers

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT toys (ball)>  <!ATTLIST toys>
               <!ELEMENT ball EMPTY>   <!ATTLIST ball>
             ]>
             <toys><ball/></toys>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "toys" ( ;;
			       ,XML_CTYPE_SEQ ,XML_CQUANT_NONE #f 1
			       #((,XML_CTYPE_NAME ,XML_CQUANT_NONE "ball" 0 #f))))
	  (dtd-element "ball" (,XML_CTYPE_EMPTY ,XML_CQUANT_NONE #f 0 #f)))))

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT toys (ball)*> <!ATTLIST toys>
               <!ELEMENT ball EMPTY>   <!ATTLIST ball>
             ]>
             <toys><ball/></toys>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "toys" ( ;;
			       ,XML_CTYPE_SEQ ,XML_CQUANT_REP #f 1
			       #((,XML_CTYPE_NAME ,XML_CQUANT_NONE "ball" 0 #f))))
	  (dtd-element "ball" (,XML_CTYPE_EMPTY ,XML_CQUANT_NONE #f 0 #f)))))

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT toys (ball)?> <!ATTLIST toys>
               <!ELEMENT ball EMPTY>   <!ATTLIST ball>
             ]>
             <toys><ball/></toys>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "toys" ( ;;
			       ,XML_CTYPE_SEQ ,XML_CQUANT_OPT #f 1
			       #((,XML_CTYPE_NAME ,XML_CQUANT_NONE "ball" 0 #f))))
	  (dtd-element "ball" (,XML_CTYPE_EMPTY ,XML_CQUANT_NONE #f 0 #f)))))

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT toys (ball)+> <!ATTLIST toys>
               <!ELEMENT ball EMPTY>   <!ATTLIST ball>
             ]>
             <toys><ball/></toys>")
    => `(,XML_STATUS_OK
  	 ((dtd-element "toys" ( ;;
			       ,XML_CTYPE_SEQ ,XML_CQUANT_PLUS #f 1
			       #((,XML_CTYPE_NAME ,XML_CQUANT_NONE "ball" 0 #f))))
	  (dtd-element "ball" (,XML_CTYPE_EMPTY ,XML_CQUANT_NONE #f 0 #f)))))

  #t)


(parametrise ((check-test-name	'dtd-attlist-handler))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.attlist-decl-handler dtd-attlist-callback)
       (P.start-element-handler elm-start-callback)
       (P.parse (string->utf8 xml) #f #t))))

  (define (dtd-attlist-callback user-data element-name attribute-name
				attribute-type default-value required?)
    (add-result
     (list 'dtd-attlist
	   (ik.cstring->string element-name)
	   (ik.cstring->string attribute-name)
	   (ik.cstring->string attribute-type)
	   (if (ik.pointer-null? default-value)
	       'no-value
	     (ik.cstring->string default-value))
	   (fxpositive? required?))))

  (define (elm-start-callback data element attributes)
    (add-result
     (list 'element-start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

;;; --------------------------------------------------------------------

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #REQUIRED>
             ]>
             <toys><ball colour='red' /></toys>")
    => (list XML_STATUS_OK
	     '((dtd-attlist "ball" "colour" "CDATA" no-value #t)
	       (element-start "toys" ())
	       (element-start "ball" ("colour" "red")))))

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #IMPLIED>
             ]>
             <toys><ball colour='red'/></toys>")
    => (list XML_STATUS_OK
	     '((dtd-attlist "ball" "colour" "CDATA" no-value #f)
	       (element-start "toys" ())
	       (element-start "ball" ("colour" "red")))))

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #FIXED 'red'>
             ]>
             <toys><ball/></toys>")
    => (list XML_STATUS_OK
	     '((dtd-attlist "ball" "colour" "CDATA" "red" #t)
	       (element-start "toys" ())
	       (element-start "ball" ("colour" "red")))))

  (check	;enumeration type
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour (red|blue|yellow) #REQUIRED>
             ]>
             <toys><ball colour='red' /></toys>")
    => (list XML_STATUS_OK
	     '((dtd-attlist "ball" "colour" "(red|blue|yellow)" no-value #t)
	       (element-start "toys" ())
	       (element-start "ball" ("colour" "red")))))

  (ik.collect))


(parametrise ((check-test-name	'dtd-notation-handler))

  (define (%false-or-string thing)
    (if (ik.pointer-null? thing)
	#f
      (ik.cstring->string thing)))

  (define (notation-callback data notation-name base system-id public-id)
    (add-result
     (list 'notation
	   (%false-or-string notation-name)
	   (%false-or-string base)
	   (%false-or-string system-id)
	   (%false-or-string public-id))))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.notation-decl-handler notation-callback)
       (P.parse (string->utf8 xml) #f #t))))

;;; --------------------------------------------------------------------

  (check
      (doit "<?xml version='1.0'?>
             <!DOCTYPE toys [
              <!NOTATION bouncing SYSTEM 'http://localhost/bouncer'>
              <!ELEMENT ball EMPTY>
              <!ATTLIST ball colour CDATA #REQUIRED>
            ]>
            <toys><ball colour='red' /></toys>")
    => `(,XML_STATUS_OK
	 ((notation "bouncing" #f "http://localhost/bouncer" #f))))

  (check
      (doit "<?xml version='1.0'?>
             <!DOCTYPE toys [
              <!NOTATION bouncing PUBLIC 'The Bouncer'>
              <!ELEMENT ball EMPTY>
              <!ATTLIST ball colour CDATA #REQUIRED>
            ]>
            <toys><ball colour='red' /></toys>")
    => `(,XML_STATUS_OK
	 ((notation "bouncing" #f #f "The Bouncer"))))

  (check
      (doit "<?xml version='1.0'?>
             <!DOCTYPE toys [
              <!NOTATION bouncing PUBLIC 'The Bouncer' 'http://localhost/bouncer'>
              <!ELEMENT ball EMPTY>
              <!ATTLIST ball colour CDATA #REQUIRED>
            ]>
            <toys><ball colour='red' /></toys>")
    => `(,XML_STATUS_OK
	 ((notation "bouncing" #f "http://localhost/bouncer" "The Bouncer"))))

  (ik.collect))


(parametrise ((check-test-name	'dtd-entity-handler))

  (define (%false-or-string thing)
    (if (ik.pointer-null? thing)
	#f
      (ik.cstring->string thing)))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.entity-decl-handler dtd-entity-callback)
       (P.start-element-handler elm-start-callback)
       (P.set-base (string->utf8 "http://localhost/"))
       (P.set-param-entity-parsing XML_PARAM_ENTITY_PARSING_ALWAYS)
       (P.parse (string->utf8 xml) #f #t))))

  (define (dtd-entity-callback data entity-name is-parameter-entity
			       value value-length
			       base system-id public-id
			       notation-name)
    (add-result
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
    (add-result
     (list 'element-start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

;;; --------------------------------------------------------------------

  (check	;general internal entity
      (doit "<?xml version='1.0'?>
             <!DOCTYPE thing [
               <!ELEMENT thing EMPTY>
               <!ATTLIST thing frob (a|b|c) #REQUIRED>
               <!ENTITY stuff 'a'>
             ]>
             <thing frob='&stuff;'/>")
    => `(,XML_STATUS_OK
	 ((dtd-entity "stuff" #f "a" "http://localhost/" #f #f #f)
	  (element-start "thing" ("frob" "a")))))

  (check	;general external entity, SYSTEM
      (doit "<?xml version='1.0'?>
             <!DOCTYPE thing [
               <!ELEMENT thing EMPTY>
               <!ENTITY stuff SYSTEM 'http://localhost/stuff'>
             ]>
             <thing/>")
    => `(,XML_STATUS_OK
	 ((dtd-entity "stuff" #f #f "http://localhost/" "http://localhost/stuff" #f #f)
	  (element-start "thing" ()))))

  (check	;general external entity, PUBLIC
      (doit "<?xml version='1.0'?>
             <!DOCTYPE thing [
               <!ELEMENT thing EMPTY>
               <!ENTITY stuff PUBLIC 'The Stuff' 'http://localhost/stuff'>
             ]>
             <thing/>")
    => `(,XML_STATUS_OK
	 ((dtd-entity "stuff" #f #f "http://localhost/" "http://localhost/stuff" "The Stuff" #f)
	  (element-start "thing" ()))))

  (check	;general external entity, SYSTEM with notation
      (doit "<?xml version='1.0'?>
             <!DOCTYPE thing [
               <!ELEMENT thing EMPTY>
               <!NOTATION stuffer SYSTEM 'http://localhost/stuffer'>
               <!ENTITY stuff SYSTEM 'http://localhost/stuff' NDATA stuffer>
             ]>
             <thing/>")
    => `(,XML_STATUS_OK
	 ((dtd-entity "stuff" #f #f "http://localhost/" "http://localhost/stuff" #f "stuffer")
	  (element-start "thing" ()))))

  (check	;general external entity, PUBLIC with notation
      (doit "<?xml version='1.0'?>
             <!DOCTYPE thing [
               <!ELEMENT thing EMPTY>
               <!NOTATION stuffer SYSTEM 'http://localhost/stuffer'>
               <!ENTITY stuff PUBLIC 'The Stuff' 'http://localhost/stuff' NDATA stuffer>
             ]>
             <thing/>")
    => `(,XML_STATUS_OK
	 ((dtd-entity "stuff" #f #f "http://localhost/" "http://localhost/stuff" "The Stuff" "stuffer")
	  (element-start "thing" ()))))

;;; --------------------------------------------------------------------

  (check	;parameter internal entity
      (doit "<?xml version='1.0'?>
             <!DOCTYPE thing [
               <!ENTITY % stuff 'a'>
               <!ELEMENT thing EMPTY>
               <!ATTLIST thing>
             ]>
             <thing/>")
    => `(,XML_STATUS_OK
	 ((dtd-entity "stuff" #t "a" "http://localhost/" #f #f #f)
	  (element-start "thing" ()))))

  (check	;parameter external entity, SYSTEM
      (doit "<?xml version='1.0'?>
             <!DOCTYPE thing [
               <!ELEMENT thing EMPTY>
               <!ENTITY % stuff SYSTEM 'http://localhost/stuff'>
             ]>
             <thing/>")
    => `(,XML_STATUS_OK
	 ((dtd-entity "stuff" #t #f "http://localhost/" "http://localhost/stuff" #f #f)
	  (element-start "thing" ()))))

  (check	;parameter external entity, PUBLIC
      (doit "<?xml version='1.0'?>
             <!DOCTYPE thing [
               <!ELEMENT thing EMPTY>
               <!ENTITY % stuff PUBLIC 'The Stuff' 'http://localhost/stuff'>
             ]>
             <thing/>")
    => `(,XML_STATUS_OK
	 ((dtd-entity "stuff" #t #f "http://localhost/" "http://localhost/stuff" "The Stuff" #f)
	  (element-start "thing" ()))))

  (ik.collect))


(parametrise ((check-test-name	'comment-handler))

  (define xml
    "<!-- this is a test document --><stuff></stuff>")

  (define (comment-callback data cstr)
    (add-result
     (list 'comment
	   (ik.cstring->string cstr))))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.comment-handler comment-callback)
       (P.parse (string->utf8 xml) #f #t))))

  (check
      (doit xml)
    => `(,XML_STATUS_OK
	 ((comment " this is a test document "))))

  (ik.collect))


(parametrise ((check-test-name	'cdata-handler))

  (define (start-cdata-callback data)
    (add-result '(start-cdata)))

  (define (end-cdata-callback data)
    (add-result '(end-cdata)))

  (define (text-callback data buf.ptr buf.len)
    (add-result
     (list 'text
	   (ik.cstring->string buf.ptr buf.len))))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.start-cdata-section-handler start-cdata-callback)
       (P.end-cdata-section-handler   end-cdata-callback)
       (P.character-data-handler      text-callback)
       (P.parse (string->utf8 xml) #f #t))))

  (define (doit-2 xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.start-cdata-section-handler start-cdata-callback)
       (P.end-cdata-section-handler   end-cdata-callback)
       (P.character-data-handler      text-callback)
       (P.parse (string->utf8 xml) #f #t))))

;;; --------------------------------------------------------------------

  (define xml
    "<stuff><![CDATA[ <stuff> ]]></stuff>")

  (check
      (doit xml)
    => `(,XML_STATUS_OK
	 ((start-cdata)
	  (text " <stuff> ")
	  (end-cdata))))

  (check
      (doit-2 xml)
    => `(,XML_STATUS_OK
	 ((start-cdata)
	  (text " <stuff> ")
	  (end-cdata))))

  (ik.collect))


(parametrise ((check-test-name	'namespace-handler))

  (define (start-element-callback data element attributes)
    (add-result
     (list 'element-start
	   (ik.cstring->string element)
	   (ik.argv->strings attributes))))

  (define (end-element-callback data element)
    (add-result
     (list 'element-end
	   (ik.cstring->string element))))

  (define (start-xmlns-callback data prefix uri)
    (add-result
     (list 'xmlns-start
	   (or (ik.pointer-null? prefix) (ik.cstring->string prefix))
	   (or (ik.pointer-null? uri)    (ik.cstring->string uri)))))

  (define (end-xmlns-callback data prefix)
    (add-result
     (list 'xmlns-end
	   (or (ik.pointer-null? prefix) (ik.cstring->string prefix)))))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-ns-parser>)))
       (P.start-element-handler  start-element-callback)
       (P.end-element-handler    end-element-callback)
       (P.start-namespace-decl-handler  start-xmlns-callback)
       (P.end-namespace-decl-handler    end-xmlns-callback)
       (P.parse (string->utf8 xml) #f #t))))

  (check	;some namespaces
      (doit "<?xml version='1.0'?>
             <!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #REQUIRED>
             ]>
            <toys xmlns:blue='http://localhost/blue'
                  xmlns:red='http://localhost/red'>\
              <blue:ball colour='yellow'/>\
              <red:ball  colour='purple'/>\
            </toys>")
    => (list XML_STATUS_OK
	     '((xmlns-start "blue" "http://localhost/blue")
	       (xmlns-start "red" "http://localhost/red")
	       (element-start "toys" ())
	       (element-start "http://localhost/blue:ball" ("colour" "yellow"))
	       (element-end "http://localhost/blue:ball")
	       (element-start "http://localhost/red:ball" ("colour" "purple"))
	       (element-end "http://localhost/red:ball")
	       (element-end "toys")
	       (xmlns-end "red")
	       (xmlns-end "blue"))))

  (check	;default namespace
      (doit "<?xml version='1.0'?>
             <!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #REQUIRED>
             ]>
            <toys xmlns='http://localhost/blue'>
              <ball colour='yellow'/>
              <ball  colour='purple'/>
            </toys>")
    => (list XML_STATUS_OK
	     '((xmlns-start #t "http://localhost/blue")
	       (element-start "http://localhost/blue:toys" ())
	       (element-start "http://localhost/blue:ball" ("colour" "yellow"))
	       (element-end "http://localhost/blue:ball")
	       (element-start "http://localhost/blue:ball" ("colour" "purple"))
	       (element-end "http://localhost/blue:ball")
	       (element-end "http://localhost/blue:toys")
	       (xmlns-end #t))))

  (ik.collect))


(parametrise ((check-test-name	'skipped-entity-handler))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.skipped-entity-handler skipped-entity-callback)
       (P.parse (string->utf8 xml) #f #t))))

  (define (skipped-entity-callback data entity-name is-parameter-entity)
    (add-result
     (list 'skipped-entity
	   (ik.cstring->string entity-name)
	   (fxpositive? is-parameter-entity))))

;;; --------------------------------------------------------------------

  (check
      (doit "<?xml version='1.0' standalone='no'?>
             <!DOCTYPE thing SYSTEM 'http://localhost/thing'>
             <thing>&ciao;</thing>")
    => `(,XML_STATUS_OK
	 ((skipped-entity "ciao" #f))))

  (ik.collect))


(parametrise ((check-test-name	'processing-instruction-handler))

  (define (doit xml)
    (with-result
     (let (((P <expat-parser>) (make <expat-parser>)))
       (P.processing-instruction-handler processing-instruction-callback)
       (P.parse (string->utf8 xml) #f #t))))

  (define (processing-instruction-callback user-data target data)
    (add-result
     (list 'processing-instruction
	   (ik.cstring->string target)
	   (ik.cstring->string data))))

;;; --------------------------------------------------------------------

  (check
      (doit "<?xml version='1.0' standalone='no'?>
             <!DOCTYPE thing SYSTEM 'http://localhost/thing'>
             <thing><?scheme (display 123) ?></thing>")
    => `(,XML_STATUS_OK
	 ((processing-instruction "scheme" "(display 123) "))))

  #t)


;;;; done

(check-report)

;;; end of file
