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

  (define xml-1 "<stuff>\
     <thing>\
     <alpha>one</alpha>\
     <beta>two</beta>\
     </thing>\
     <thing>\
     <alpha>123</alpha>\
     <beta>456</beta>\
     </thing>\
     </stuff>")

  (define make-start-cb
    (ffi.make-c-callback-maker 'void '(pointer pointer pointer)))

  (define make-end-cb
    (ffi.make-c-callback-maker 'void '(pointer pointer)))

  (define make-cdata-cb
    (ffi.make-c-callback-maker 'void '(pointer pointer signed-int)))

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
       (let ((parser	(XML_ParserCreate 'UTF-8))
	     (start	(make-start-cb start-callback))
	     (end	(make-end-cb   end-callback))
	     (cdata	(make-cdata-cb cdata-callback)))
	 (XML_SetElementHandler parser start end)
	 (XML_SetCharacterDataHandler parser cdata)
	 (let* ((buffer	(string->utf8 xml-1))
		(finished?	#t)
		(rv		(XML_Parse parser buffer #f finished?)))
	   (ffi.free-c-callback start)
	   (ffi.free-c-callback end)
	   rv)))
    => (list XML_STATUS_OK
	     '((start "stuff" ())
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
