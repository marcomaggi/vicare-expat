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
