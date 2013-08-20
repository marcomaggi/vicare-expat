;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: tests for Expat bindings: version functions
;;;Date: Tue Aug 20, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare Expat bindings: version functions\n")


(parametrise ((check-test-name	'version))

  (check
      (fixnum? (vicare-expat-version-interface-current))
    => #t)

  (check
      (fixnum? (vicare-expat-version-interface-revision))
    => #t)

  (check
      (fixnum? (vicare-expat-version-interface-age))
    => #t)

  (check
      (string? (vicare-expat-version))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
