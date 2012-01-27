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
(import (rename (vicare)
		(parameterize	parametrise))
  (vicare expat)
  (vicare expat constants)
  (prefix (vicare ffi) ffi.)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare Expat bindings\n")


;;;; helpers

(define (%print-parser-error-maybe parser rv)
  (unless (= XML_STATUS_OK rv)
    (printf "error: ~a\n" (XML_ErrorString (XML_GetErrorCode parser)))))


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
		      (xml-parser-status-parsing status))
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



(parametrise ((check-test-name	'dtd-attlist-handler))

  (define (scheme-callback user-data element-name attribute-name
			   attribute-type default-value required?)
    (add-result (list 'attlist
		      (ffi.cstring->string element-name)
		      (ffi.cstring->string attribute-name)
		      (ffi.cstring->string attribute-type)
		      (if (ffi.pointer-null? default-value)
			  'no-value
			(ffi.cstring->string default-value))
		      required?)))

  (define (doit xml)
    (with-result
     (let* ((xml-utf8	(string->utf8 xml))
	    (parser	(XML_ParserCreate 'UTF-8))
	    (cb		(XML_AttlistDeclHandler scheme-callback)))
       (XML_SetAttlistDeclHandler parser cb)
       (let ((rv (XML_Parse parser xml-utf8 #f #t)))
	 (ffi.free-c-callback cb)
	 rv))))

;;; --------------------------------------------------------------------

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #REQUIRED>
             ]>
             <toys><ball colour='red' /></toys>")
    => (list XML_STATUS_OK
	     '((attlist "ball" "colour" "CDATA" no-value 1))))

  (check
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour CDATA #IMPLIED>
             ]>
             <toys><ball colour='red' /></toys>")
    => (list XML_STATUS_OK
	     '((attlist "ball" "colour" "CDATA" no-value 0))))

  (check	;enumeration type
      (doit "<!DOCTYPE toys [
               <!ELEMENT ball EMPTY>
               <!ATTLIST ball colour (red|blue|yellow) #REQUIRED>
             ]>
             <toys><ball colour='red' /></toys>")
    => (list XML_STATUS_OK
	     '((attlist "ball" "colour" "(red|blue|yellow)" no-value 1))))

  #t)


;;;; done

(check-report)

;;; end of file
