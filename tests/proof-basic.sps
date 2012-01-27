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

    (let ((parser		(XML_ParserCreate 'UTF-8))
	  (start		(XML_StartElementHandler  start-callback))
	  (end		(XML_EndElementHandler    end-callback))
	  (cdata		(XML_CharacterDataHandler cdata-callback))
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


;;;; DTD attributes list

(when #f
  (let ()

    (define xml-utf8
      (string->utf8
       "<!DOCTYPE spiffy [
         <!ELEMENT ball EMPTY>
         <!ATTLIST ball colour CDATA #REQUIRED>
       ]>
       <spiffy><ball colour='red' /></spiffy>"))

    (define (scheme-callback user-data element-name attribute-name attribute-type default-value required?)
      (let ((element-name	(ffi.cstring->string element-name))
	    (attribute-name	(ffi.cstring->string attribute-name))
	    (attribute-type	(ffi.cstring->string attribute-type))
	    (default-value	(if (ffi.pointer-null? default-value)
				    'NULL
				  (ffi.cstring->string default-value))))
	(pretty-print (list element-name attribute-name attribute-type default-value required?))))

    (let ((parser	(XML_ParserCreate 'UTF-8))
	  (cb		(XML_AttlistDeclHandler scheme-callback)))
      (XML_SetAttlistDeclHandler parser cb)
      (if (= XML_STATUS_OK (XML_Parse parser xml-utf8 #f #t))
	  (display "success\n")
	(let ((code (XML_GetErrorCode parser)))
	  (printf "error: ~a\n" (latin1->string (XML_ErrorString code)))))
      (ffi.free-c-callback cb))

    (flush-output-port (current-output-port))

    #f))

;;; end of file
