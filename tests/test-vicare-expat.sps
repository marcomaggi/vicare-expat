;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: tests for Expat bindings
;;;Date: Tue Jan 24, 2012
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
(import (vicare)
  (vicare expat)
  (vicare expat constants)
  (prefix (vicare ffi) ffi.)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare Expat bindings\n")


;;;; helpers

(define (%print-parser-error-maybe parser rv)
  (unless (= XML_STATUS_OK rv)
    (fprintf (current-error-port) "error: ~a\n" (XML_ErrorString (XML_GetErrorCode parser)))))


(parametrise ((check-test-name	'parsing-basic))

  (define xml-1 "<!-- this is a test document -->\
     <stuff>\
     <thing>\
     <alpha>one</alpha>\
     <beta>two</beta>\
     </thing>\
     <thing>\
     <alpha>123</alpha>\
     <beta>456</beta>\
     </thing>\
     </stuff>")

  (check
      (with-result
       (define (start-callback data element attributes)
	 (add-result (list 'start
			   (ffi.cstring->string element)
			   (ffi.argv->strings attributes))))
       (define (end-callback data element)
	 (add-result (list 'end (ffi.cstring->string element))))
       (define (chdata-callback data buf.ptr buf.len)
	 (add-result (list 'character-data (ffi.cstring->string buf.ptr buf.len))))
       (define (comment-callback data cstr)
	 (add-result (list 'comment (ffi.cstring->string cstr))))
       (let ((parser	(XML_ParserCreate 'UTF-8))
	     (start	(XML_StartElementHandler  start-callback))
	     (end	(XML_EndElementHandler    end-callback))
	     (chdata	(XML_CharacterDataHandler chdata-callback))
	     (comment	(XML_CommentHandler       comment-callback)))
	 (XML_SetElementHandler		parser start end)
	 (XML_SetCharacterDataHandler	parser chdata)
	 (XML_SetCommentHandler		parser comment)
	 (let* ((buffer	(string->utf8 xml-1))
		(finished?	#t)
		(rv		(XML_Parse parser buffer #f finished?)))
	   (ffi.free-c-callback start)
	   (ffi.free-c-callback end)
	   (ffi.free-c-callback chdata)
	   (ffi.free-c-callback comment)
	   rv)))
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

  (check	;XML_ParserCreateNS
      (with-result
       (define (start-callback data element attributes)
	 (add-result (list 'start
			   (ffi.cstring->string element)
			   (ffi.argv->strings attributes))))
       (define (end-callback data element)
	 (add-result (list 'end (ffi.cstring->string element))))
       (define (chdata-callback data buf.ptr buf.len)
	 (add-result (list 'character-data (ffi.cstring->string buf.ptr buf.len))))
       (define (comment-callback data cstr)
	 (add-result (list 'comment (ffi.cstring->string cstr))))
       (let ((parser	(XML_ParserCreateNS 'UTF-8 #\,))
	     (start	(XML_StartElementHandler  start-callback))
	     (end	(XML_EndElementHandler    end-callback))
	     (chdata	(XML_CharacterDataHandler chdata-callback))
	     (comment	(XML_CommentHandler       comment-callback)))
	 (XML_SetElementHandler		parser start end)
	 (XML_SetCharacterDataHandler	parser chdata)
	 (XML_SetCommentHandler		parser comment)
	 (let* ((buffer	(string->utf8 xml-1))
		(finished?	#t)
		(rv		(XML_Parse parser buffer #f finished?)))
	   (ffi.free-c-callback start)
	   (ffi.free-c-callback end)
	   (ffi.free-c-callback chdata)
	   (ffi.free-c-callback comment)
	   rv)))
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
			   (ffi.cstring->string element)
			   (ffi.argv->strings attributes))))
       (define (end-callback data element)
	 (add-result (list 'end (ffi.cstring->string element))))
       (define (chdata-callback data buf.ptr buf.len)
	 (add-result (list 'character-data (ffi.cstring->string buf.ptr buf.len))))
       (define (comment-callback data cstr)
	 (add-result (list 'comment (ffi.cstring->string cstr))))
       (let ((parser	(XML_ParserCreate 'UTF-8))
	     (start	(XML_StartElementHandler  start-callback))
	     (end	(XML_EndElementHandler    end-callback))
	     (chdata	(XML_CharacterDataHandler chdata-callback))
	     (comment	(XML_CommentHandler       comment-callback)))
	 (XML_SetElementHandler		parser start end)
	 (XML_SetCharacterDataHandler	parser chdata)
	 (XML_SetCommentHandler		parser comment)
	 (let* ((data		(string->utf8 xml-1))
		(buflen		(bytevector-length data))
		(buffer		(XML_GetBuffer parser buflen)))
	   (ffi.memory-copy buffer 0 data 0 buflen)
	   (let* ((finished?	#t)
		  (rv		(XML_ParseBuffer parser buflen finished?)))
	     (ffi.free-c-callback start)
	     (ffi.free-c-callback end)
	     (ffi.free-c-callback chdata)
	     (ffi.free-c-callback comment)
	     rv))))
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

  #t)


(parametrise ((check-test-name	'namespaces))

  (define (doit xml-utf8)
    (with-result
     (define (start-callback data element attributes)
       (add-result (list 'start
			 (ffi.cstring->string element)
			 (ffi.argv->strings attributes))))
     (define (end-callback data element)
       (add-result (list 'end (ffi.cstring->string element))))
     (let ((parser	(XML_ParserCreateNS 'UTF-8 #\:))
	   (start	(XML_StartElementHandler  start-callback))
	   (end		(XML_EndElementHandler    end-callback)))
       (XML_SetElementHandler		parser start end)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback start)
	 (ffi.free-c-callback end)
	 rv))))

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


(parametrise ((check-test-name	'creation))

  (check
      (let ((parser (XML_ParserCreate 'UTF-8)))
	(XML_ParserFree parser)
	(ffi.pointer-null? parser))
    => #t)

  (check
      (let ((parser (XML_ParserCreate 'UTF-8)))
	(XML_ParserFree parser)
	(XML_ParserFree parser)
	(XML_ParserFree parser)
	(ffi.pointer-null? parser))
    => #t)

  (check
      (let ((parser (XML_ParserCreate 'UTF-8)))
	(XML_ParserReset parser)
	(XML_ParserFree parser)
	(ffi.pointer-null? parser))
    => #t)


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
       (define (start-callback parser element attributes)
	 (add-result (list 'start
			   (ffi.cstring->string element)
			   (ffi.argv->strings attributes)))
	 (let ((status (XML_GetParsingStatus parser)))
	   (unless (= XML_STATUS_SUSPENDED
		      (XML_ParsingStatus-parsing status))
	     (add-result (list 'stop-rv (XML_StopParser parser #f))))))
       (let* ((parser	(XML_ParserCreate))
	      (start	(XML_StartElementHandler start-callback)))
	 (XML_SetStartElementHandler parser start)
	 (XML_SetUserData parser parser)
	 (let ((rv	(XML_Parse parser xml-utf8 #f #t)))
	   (ffi.free-c-callback start)
	   (add-result (XML_GetErrorCode parser))
	   rv)))
    => `(,XML_STATUS_ERROR ((start "toys" ())
			    (stop-rv ,XML_STATUS_OK)
			    ,XML_ERROR_ABORTED)))

  (check	;stopping with resuming
      (with-result
       (define (start-callback-and-suspend parser element attributes)
	 (add-result (list 'start
			   (ffi.cstring->string element)
			   (ffi.argv->strings attributes)))
	 (add-result (list 'stop-rv (XML_StopParser parser #t))))
       (define (start-callback parser element attributes)
	 (add-result (list 'start
			   (ffi.cstring->string element)
			   (ffi.argv->strings attributes))))
       (let* ((parser	(XML_ParserCreate))
	      (suspend	(XML_StartElementHandler start-callback-and-suspend)))
	 (XML_SetStartElementHandler parser suspend)
	 (XML_SetUserData parser parser)
	 (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	   (ffi.free-c-callback suspend)
	   (add-result (list 'suspended rv))
	   (let ((start (XML_StartElementHandler start-callback)))
	     (XML_SetStartElementHandler parser start)
	     (let ((rv (XML_ResumeParser parser)))
	       (ffi.free-c-callback start)
	       rv)))))
    => `(,XML_STATUS_OK ((start "toys" ())
			 (stop-rv ,XML_STATUS_OK)
			 (suspended ,XML_STATUS_SUSPENDED)
			 (start "ball" ("colour" "red")))))


  #t)


(parametrise ((check-test-name	'parser-misc))

  (check
      (let ((parser (XML_ParserCreate)))
	(XML_SetUserData parser parser)
	(ffi.pointer=? parser (XML_GetUserData parser)))
    => #t)

  (check
      (let ((parser (XML_ParserCreate)))
	(XML_UseParserAsHandlerArg parser)
	#t)
    => #t)

  (check
      (let ((parser (XML_ParserCreate)))
	(XML_SetBase parser '#vu8(1 2 3 4))
	(XML_GetBase parser))
    => '#vu8(1 2 3 4))

  (check
      (let ((parser (XML_ParserCreate)))
	(XML_SetBase parser #f)
	(XML_GetBase parser))
    => #f)

  (check
      (let ((parser (XML_ParserCreate)))
	(XML_SetEncoding parser 'UTF-8))
    => XML_STATUS_OK)

  (check
      (let ((parser (XML_ParserCreate)))
	(XML_UseForeignDTD parser #t))
    => XML_ERROR_NONE)

  (check
      (let ((parser (XML_ParserCreate)))
	(XML_UseForeignDTD parser #f))
    => XML_ERROR_NONE)



  #t)


(parametrise ((check-test-name	'error-reporting))

  (check
      (XML_ErrorString XML_ERROR_NO_ELEMENTS)
    => "no element found")

  #t)


(parametrise ((check-test-name	'version))

  (check
      (XML_ExpatVersion)
    => "expat_2.0.1")

  (check
      (XML_ExpatVersionInfo)
    => '#(2 0 1))

;;;(check-pretty-print (XML_GetFeatureList))

  #t)


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
	   (or (ffi.pointer-null? version)  (ffi.cstring->string version))
	   (or (ffi.pointer-null? encoding) (ffi.cstring->string encoding))
	   (%process-standalone standalone))))

  (define (doit xml-utf8)
    (with-result
     (let ((parser	(XML_ParserCreate))
	   (xml-decl	(XML_XmlDeclHandler xml-decl-callback)))
       (XML_SetXmlDeclHandler parser xml-decl)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback xml-decl)
	 rv))))

;;; --------------------------------------------------------------------

  (define dtd-utf8
    (string->utf8
     "<?xml version='1.0' encoding='utf-8'?>
      <!ELEMENT ball EMPTY>
      <!ATTLIST ball colour CDATA #REQUIRED>"))

  (define (external-entity-callback parser context base system-id public-id)
    (add-result
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
	(%print-parser-error-maybe parser rv)
	rv)))

  (define (doit-with-external-entity xml-utf8)
    (with-result
     (let* ((parser	(XML_ParserCreate))
	    (xml-decl	(XML_XmlDeclHandler xml-decl-callback))
	    (ext-ent	(XML_ExternalEntityRefHandler external-entity-callback)))
       (XML_SetParamEntityParsing parser XML_PARAM_ENTITY_PARSING_ALWAYS)
       (XML_SetXmlDeclHandler parser xml-decl)
       (XML_SetExternalEntityRefHandler parser ext-ent)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback xml-decl)
	 (ffi.free-c-callback ext-ent)
	 rv))))

;;; --------------------------------------------------------------------

  (check
      (doit (string->utf8 "<?xml version='1.0'?><toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" #t unspecified))))

  (check
      (doit (string->utf8 "<?xml version='1.0' encoding='utf-8'?><toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" "utf-8" unspecified))))

  (check
      (doit (string->utf8 "<?xml version='1.0' standalone='yes'?><toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" #t standalone))))

  (check
      (doit (string->utf8 "<?xml version='1.0' standalone='no'?><toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" #t non-standalone))))

;;; --------------------------------------------------------------------

  (check
      (doit-with-external-entity
       (string->utf8
	"<?xml version='1.0'?>
         <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl "1.0" #t unspecified)
	  (external-entity #t #t "http://localhost/toys" #t)
	  (xml-decl "1.0" "utf-8" unspecified))))

  #t)


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

  (define (doit xml-utf8)
    (with-result
     (let ((parser	(XML_ParserCreate))
	   (xml-decl	(XML_XmlDeclHandler xml-decl-callback))
	   (not-stand	(XML_NotStandaloneHandler not-stand-callback))
	   (dt-start	(XML_StartDoctypeDeclHandler start-doctype-callback))
	   (dt-end	(XML_EndDoctypeDeclHandler end-doctype-callback)))
       (XML_SetXmlDeclHandler parser xml-decl)
       (XML_SetNotStandaloneHandler parser not-stand)
       (XML_SetStartDoctypeDeclHandler parser dt-start)
       (XML_SetEndDoctypeDeclHandler   parser dt-end)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback xml-decl)
	 (ffi.free-c-callback not-stand)
	 (ffi.free-c-callback dt-start)
	 (ffi.free-c-callback dt-end)
	 rv))))

;;; --------------------------------------------------------------------

  (check
      (doit
       (string->utf8
	"<?xml version='1.0'?>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl unspecified))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0'?>
         <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl unspecified)
	  (not-standalone)
	  (doctype-start 0)
	  (doctype-end))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0'?>
         <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl unspecified)
	  (not-standalone)
	  (doctype-start 0)
	  (doctype-end))))

;;; --------------------------------------------------------------------

  (check
      (doit
       (string->utf8
	"<?xml version='1.0' standalone='no'?>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl non-standalone))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0' standalone='no'?>
         <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl non-standalone)
	  (not-standalone)
	  (doctype-start 0)
	  (doctype-end))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0' standalone='no'?>
         <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl non-standalone)
	  (not-standalone)
	  (doctype-start 0)
	  (doctype-end))))

;;; --------------------------------------------------------------------

  (check
      (doit
       (string->utf8
	"<?xml version='1.0' standalone='yes'?>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl standalone))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0' standalone='yes'?>
         <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl standalone)
	  (doctype-start 0)
	  (doctype-end))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0' standalone='yes'?>
         <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
         <toys><ball colour='red'/></toys>"))
    => `(,XML_STATUS_OK
	 ((xml-decl standalone)
	  (doctype-start 0)
	  (doctype-end))))

  #t)


(parametrise ((check-test-name	'dtd-doctype-handler))

  (define (doit xml-utf8)
    (with-result
     (define (start-doctype-callback data doctype-name sysid pubid has-internal-subset)
       (add-result
	(list 'doctype-start
	      (ffi.cstring->string doctype-name)
	      (or (ffi.pointer-null? sysid) (ffi.cstring->string sysid))
	      (or (ffi.pointer-null? pubid) (ffi.cstring->string pubid))
	      has-internal-subset)))
     (define (end-doctype-callback data)
       (add-result '(doctype-end)))
     (let ((parser	(XML_ParserCreate))
	   (start	(XML_StartDoctypeDeclHandler start-doctype-callback))
	   (end		(XML_EndDoctypeDeclHandler   end-doctype-callback)))
       (XML_SetDoctypeDeclHandler parser start end)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
;;;	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback start)
	 (ffi.free-c-callback end)
	 rv))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0'?>
         <!DOCTYPE toys SYSTEM 'http://localhost/toys'>
         <toys><ball colour='yellow'/></toys>"))
    => (list XML_STATUS_OK
	     '((doctype-start "toys" "http://localhost/toys" #t 0)
	       (doctype-end))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0'?>
         <!DOCTYPE toys PUBLIC 'The Toys' 'http://localhost/toys'>
         <toys><ball colour='yellow'/></toys>"))
    => (list XML_STATUS_OK
	     '((doctype-start "toys" "http://localhost/toys" "The Toys" 0)
	       (doctype-end))))

  (check
      (doit
       (string->utf8
	"<?xml version='1.0'?>
         <!DOCTYPE toys [
           <!ELEMENT ball EMPTY>
           <!ATTLIST ball colour CDATA #REQUIRED>
         ]>
         <toys><ball colour='yellow'/></toys>"))
    => (list XML_STATUS_OK
	     '((doctype-start "toys" #t #t 1)
	       (doctype-end))))

  #t)


(parametrise ((check-test-name	'default-handler))

  (define (doit xml-utf8)
    (with-result
     (define (start-callback data element attributes)
       (XML_DefaultCurrent data))
     (define (end-callback data element)
       (XML_DefaultCurrent data))
     (define (default-callback user-data buf.ptr buf.len)
       (add-result (list 'default (ffi.cstring->string buf.ptr buf.len))))
     (let ((parser	(XML_ParserCreateNS 'UTF-8 #\:))
	   (start	(XML_StartElementHandler start-callback))
	   (end		(XML_EndElementHandler   end-callback))
	   (default	(XML_DefaultHandler      default-callback)))
       (XML_UseParserAsHandlerArg parser)
       (XML_SetElementHandler parser start end)
       (XML_SetDefaultHandler parser default)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback start)
	 (ffi.free-c-callback end)
	 (ffi.free-c-callback default)
	 rv))))

  (check
      (doit (string->utf8 "<toys><ball colour='yellow'/></toys>"))
    => (list XML_STATUS_OK
	     '((default "<toys>")
	       (default "<ball colour='yellow'/>")
	       (default "")
	       (default "</toys>"))))

  #t)


(parametrise ((check-test-name	'external-entity-parser))

  (define xml-utf8
    (string->utf8 "<!DOCTYPE toys SYSTEM 'http://localhost/toys'>
                   <toys><ball colour='red'/></toys>"))

  (define dtd-utf8
    (string->utf8 "<!ELEMENT ball EMPTY>
                   <!ATTLIST ball colour CDATA #REQUIRED>"))

  (define (scheme-callback parser context base system-id public-id)
    (let* ((parser (XML_ExternalEntityParserCreate parser context 'UTF-8))
	   (rv     (XML_Parse parser dtd-utf8 #f #t)))
      (add-result (list 'external-entity rv
			(ffi.pointer-null? context)
			(ffi.pointer-null? base)
			(ffi.cstring->string system-id)
			(ffi.pointer-null? public-id)))
      XML_STATUS_OK))

  (define (doit)
    (with-result
     (let* ((parser	(XML_ParserCreate 'UTF-8))
	    (cb		(XML_ExternalEntityRefHandler scheme-callback)))
       (XML_SetParamEntityParsing parser XML_PARAM_ENTITY_PARSING_ALWAYS)
       (XML_SetExternalEntityRefHandler parser cb)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback cb)
	 rv))))

;;; --------------------------------------------------------------------

  (check
      (doit)
    => `(,XML_STATUS_OK
	 ((external-entity ,XML_STATUS_OK #t #t "http://localhost/toys" #t))))

  #t)


(parametrise ((check-test-name	'dtd-element-handler))

  (define (doit xml)
    (with-result
     (let ((xml-utf8	(string->utf8 xml))
	   (parser	(XML_ParserCreate))
	   (dtd-elm	(XML_ElementDeclHandler dtd-elm-callback)))
       (XML_UseParserAsHandlerArg parser)
       (XML_SetElementDeclHandler parser dtd-elm)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback dtd-elm)
	 rv))))

  (define (dtd-elm-callback data name model)
    (add-result
     (list 'dtd-element
	   (ffi.cstring->string name)
	   (XML_Content->list (pointer->XML_Content model))))
    (XML_FreeContentModel data model))

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
     (let* ((xml-utf8		(string->utf8 xml))
	    (parser		(XML_ParserCreate))
	    (dtd-attlist	(XML_AttlistDeclHandler dtd-attlist-callback))
	    (elm-start		(XML_StartElementHandler elm-start-callback)))
       (XML_SetAttlistDeclHandler parser dtd-attlist)
       (XML_SetStartElementHandler parser elm-start)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback dtd-attlist)
	 (ffi.free-c-callback elm-start)
	 rv))))

  (define (dtd-attlist-callback user-data element-name attribute-name
				attribute-type default-value required?)
    (add-result
     (list 'dtd-attlist
	   (ffi.cstring->string element-name)
	   (ffi.cstring->string attribute-name)
	   (ffi.cstring->string attribute-type)
	   (if (ffi.pointer-null? default-value)
	       'no-value
	     (ffi.cstring->string default-value))
	   (fxpositive? required?))))

  (define (elm-start-callback data element attributes)
    (add-result
     (list 'element-start
	   (ffi.cstring->string element)
	   (ffi.argv->strings attributes))))

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

  #t)


(parametrise ((check-test-name	'dtd-notation-handler))

  (define (%false-or-string thing)
    (if (ffi.pointer-null? thing)
	#f
      (ffi.cstring->string thing)))

  (define (notation-callback data notation-name base system-id public-id)
    (add-result
     (list 'notation
	   (%false-or-string notation-name)
	   (%false-or-string base)
	   (%false-or-string system-id)
	   (%false-or-string public-id))))

  (define (doit xml-utf8)
    (with-result
     (let* ((parser	(XML_ParserCreate))
	    (notation	(XML_NotationDeclHandler notation-callback)))
       (XML_SetNotationDeclHandler parser notation)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback notation)
	 rv))))

;;; --------------------------------------------------------------------

  (check
      (doit (string->utf8
	     "<?xml version='1.0'?>
              <!DOCTYPE toys [
               <!NOTATION bouncing SYSTEM 'http://localhost/bouncer'>
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #REQUIRED>
             ]>
             <toys><ball colour='red' /></toys>"))
    => `(,XML_STATUS_OK
	 ((notation "bouncing" #f "http://localhost/bouncer" #f))))

  (check
      (doit (string->utf8
	     "<?xml version='1.0'?>
              <!DOCTYPE toys [
               <!NOTATION bouncing PUBLIC 'The Bouncer'>
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #REQUIRED>
             ]>
             <toys><ball colour='red' /></toys>"))
    => `(,XML_STATUS_OK
	 ((notation "bouncing" #f #f "The Bouncer"))))

  (check
      (doit (string->utf8
	     "<?xml version='1.0'?>
              <!DOCTYPE toys [
               <!NOTATION bouncing PUBLIC 'The Bouncer' 'http://localhost/bouncer'>
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #REQUIRED>
             ]>
             <toys><ball colour='red' /></toys>"))
    => `(,XML_STATUS_OK
	 ((notation "bouncing" #f "http://localhost/bouncer" "The Bouncer"))))

  #t)


(parametrise ((check-test-name	'dtd-entity-handler))

  (define (%false-or-string thing)
    (if (ffi.pointer-null? thing)
	#f
      (ffi.cstring->string thing)))

  (define (doit xml)
    (with-result
     (let* ((xml-utf8	(string->utf8 xml))
	    (parser	(XML_ParserCreate))
	    (dtd-entity	(XML_EntityDeclHandler dtd-entity-callback))
	    (elm-start	(XML_StartElementHandler elm-start-callback)))
       (XML_SetBase parser (string->utf8 "http://localhost/"))
       (XML_SetParamEntityParsing parser XML_PARAM_ENTITY_PARSING_ALWAYS)
       (XML_SetEntityDeclHandler parser dtd-entity)
       (XML_SetStartElementHandler parser elm-start)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback dtd-entity)
	 (ffi.free-c-callback elm-start)
	 rv))))

  (define (dtd-entity-callback data entity-name is-parameter-entity
			       value value-length
			       base system-id public-id
			       notation-name)
    (add-result
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
    (add-result
     (list 'element-start
	   (ffi.cstring->string element)
	   (ffi.argv->strings attributes))))

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

  #t)


(parametrise ((check-test-name	'comment-handler))

  (define xml-utf8
    (string->utf8
     "<!-- this is a test document --><stuff></stuff>"))

  (define (comment-callback data cstr)
    (add-result
     (list 'comment
	   (ffi.cstring->string cstr))))

  (define (doit xml-utf8)
    (with-result
     (let ((parser	(XML_ParserCreate))
	   (comment	(XML_CommentHandler comment-callback)))
       (XML_SetCommentHandler parser comment)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback comment)
	 rv))))

  (check
      (doit xml-utf8)
    => `(,XML_STATUS_OK
	 ((comment " this is a test document "))))

  #t)


(parametrise ((check-test-name	'cdata-handler))

  (define xml-utf8
    (string->utf8
     "<stuff><![CDATA[ <stuff> ]]></stuff>"))

  (define (start-cdata-callback data)
    (add-result '(start-cdata)))

  (define (end-cdata-callback data)
    (add-result '(end-cdata)))

  (define (text-callback data buf.ptr buf.len)
    (add-result
     (list 'text
	   (ffi.cstring->string buf.ptr buf.len))))

  (define (doit xml-utf8)
    (with-result
     (let ((parser	(XML_ParserCreate))
	   (start	(XML_StartCdataSectionHandler start-cdata-callback))
	   (end		(XML_EndCdataSectionHandler   end-cdata-callback))
	   (text	(XML_CharacterDataHandler     text-callback)))
       (XML_SetCdataSectionHandler parser start end)
       (XML_SetCharacterDataHandler parser text)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback start)
	 (ffi.free-c-callback end)
	 (ffi.free-c-callback text)
	 rv))))

  (define (doit-2 xml-utf8)
    (with-result
     (let ((parser	(XML_ParserCreate))
	   (start	(XML_StartCdataSectionHandler start-cdata-callback))
	   (end		(XML_EndCdataSectionHandler   end-cdata-callback))
	   (text	(XML_CharacterDataHandler     text-callback)))
       (XML_SetStartCdataSectionHandler parser start)
       (XML_SetEndCdataSectionHandler   parser end)
       (XML_SetCharacterDataHandler     parser text)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback start)
	 (ffi.free-c-callback end)
	 (ffi.free-c-callback text)
	 rv))))

  (check
      (doit xml-utf8)
    => `(,XML_STATUS_OK
	 ((start-cdata)
	  (text " <stuff> ")
	  (end-cdata))))

  (check
      (doit-2 xml-utf8)
    => `(,XML_STATUS_OK
	 ((start-cdata)
	  (text " <stuff> ")
	  (end-cdata))))

  #t)


(parametrise ((check-test-name	'namespace-handler))

  (define (start-element-callback data element attributes)
    (add-result
     (list 'element-start
	   (ffi.cstring->string element)
	   (ffi.argv->strings attributes))))

  (define (end-element-callback data element)
    (add-result
     (list 'element-end
	   (ffi.cstring->string element))))

  (define (start-xmlns-callback data prefix uri)
    (add-result
     (list 'xmlns-start
	   (or (ffi.pointer-null? prefix) (ffi.cstring->string prefix))
	   (or (ffi.pointer-null? uri)    (ffi.cstring->string uri)))))

  (define (end-xmlns-callback data prefix)
    (add-result
     (list 'xmlns-end
	   (or (ffi.pointer-null? prefix) (ffi.cstring->string prefix)))))

  (define (doit xml-utf8)
    (with-result
     (let ((parser	(XML_ParserCreateNS 'UTF-8 #\:))
	   (start-elm	(XML_StartElementHandler  start-element-callback))
	   (end-elm	(XML_EndElementHandler    end-element-callback))
	   (start-ns	(XML_StartNamespaceDeclHandler  start-xmlns-callback))
	   (end-ns	(XML_EndNamespaceDeclHandler    end-xmlns-callback)))
       (XML_SetElementHandler		parser start-elm end-elm)
       (XML_SetNamespaceDeclHandler	parser start-ns  end-ns)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback start-elm)
	 (ffi.free-c-callback end-elm)
	 (ffi.free-c-callback start-ns)
	 (ffi.free-c-callback end-ns)
	 rv))))

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
	     '((xmlns-start #t "http://localhost/blue")
	       (element-start "http://localhost/blue:toys" ())
	       (element-start "http://localhost/blue:ball" ("colour" "yellow"))
	       (element-end "http://localhost/blue:ball")
	       (element-start "http://localhost/blue:ball" ("colour" "purple"))
	       (element-end "http://localhost/blue:ball")
	       (element-end "http://localhost/blue:toys")
	       (xmlns-end #t))))

  #t)


(parametrise ((check-test-name	'skipped-entity-handler))

  (define (doit xml)
    (with-result
     (let ((xml-utf8	(string->utf8 xml))
	   (parser	(XML_ParserCreate))
	   (skip-ent	(XML_SkippedEntityHandler skipped-entity-callback)))
       (XML_SetSkippedEntityHandler parser skip-ent)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback skip-ent)
	 rv))))

  (define (skipped-entity-callback data entity-name is-parameter-entity)
    (add-result
     (list 'skipped-entity
	   (ffi.cstring->string entity-name)
	   (fxpositive? is-parameter-entity))))

;;; --------------------------------------------------------------------

  (check
      (doit "<?xml version='1.0' standalone='no'?>
             <!DOCTYPE thing SYSTEM 'http://localhost/thing'>
             <thing>&ciao;</thing>")
    => `(,XML_STATUS_OK
	 ((skipped-entity "ciao" #f))))

  #t)


(parametrise ((check-test-name	'processing-instruction-handler))

  (define (doit xml)
    (with-result
     (let ((xml-utf8	(string->utf8 xml))
	   (parser	(XML_ParserCreate))
	   (proc-inst	(XML_ProcessingInstructionHandler processing-instruction-callback)))
       (XML_SetProcessingInstructionHandler parser proc-inst)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (%print-parser-error-maybe parser rv)
	 (ffi.free-c-callback proc-inst)
	 rv))))

  (define (processing-instruction-callback user-data target data)
    (add-result
     (list 'processing-instruction
	   (ffi.cstring->string target)
	   (ffi.cstring->string data))))

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
