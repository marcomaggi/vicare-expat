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
  (prefix (vicare ffi) ffi.)
  (only (vicare) collect)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Nausicaa front end\n")


(parametrise ((check-test-name	'base))

  (check
      (let (((P <expat-parser>) (make <expat-parser>)))
        #f)
    => #f)

  (collect)

  #t)


;;;; done

(check-report)

;;; end of file
