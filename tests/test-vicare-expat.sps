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
	 (let ((element    (ffi.cstring->string element))
	       (attributes (ffi.argv->strings attributes)))
	   (add-result (list 'start element attributes)))
	 (void))
       (define (end-callback data element)
	 (let ((element (ffi.cstring->string element)))
	   (add-result (list 'end element)))
	 (void))
       (define (cdata-callback data buf.ptr buf.len)
	 (let ((text (ffi.cstring->string buf.ptr buf.len)))
	   (add-result (list 'cdata text))))
       (define (comment-callback data cstr)
	 (let ((text (ffi.cstring->string cstr)))
	   (add-result (list 'comment text))))
       (let ((parser	(XML_ParserCreate 'UTF-8))
	     (start	(XML_StartElementHandler  start-callback))
	     (end	(XML_EndElementHandler    end-callback))
	     (cdata	(XML_CharacterDataHandler cdata-callback))
	     (comment	(XML_CommentHandler       comment-callback)))
	 (XML_SetElementHandler		parser start end)
	 (XML_SetCharacterDataHandler	parser cdata)
	 (XML_SetCommentHandler		parser comment)
	 (let* ((buffer	(string->utf8 xml-1))
		(finished?	#t)
		(rv		(XML_Parse parser buffer #f finished?)))
	   (ffi.free-c-callback start)
	   (ffi.free-c-callback end)
	   (ffi.free-c-callback cdata)
	   (ffi.free-c-callback comment)
	   rv)))
    => (list XML_STATUS_OK
	     '((comment " this is a test document ")
	       (start "stuff" ())
	       (start "thing" ())
	       (start "alpha" ()) (cdata "one") (end "alpha")
	       (start "beta" ()) (cdata "two")(end "beta")
	       (end "thing")
	       (start "thing" ())
	       (start "alpha" ()) (cdata "123") (end "alpha")
	       (start "beta" ()) (cdata "456") (end "beta")
	       (end "thing")
	       (end "stuff"))))

  #t)


;;;; done

(check-report)

;;; end of file
