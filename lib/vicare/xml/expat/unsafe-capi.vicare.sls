;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: unsafe interface to the C language API
;;;Date: Wed Jun 10, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare xml expat unsafe-capi)
  (export

    ;; version functions
    vicare-expat-version-interface-current
    vicare-expat-version-interface-revision
    vicare-expat-version-interface-age
    vicare-expat-version

    )
  (import (vicare))


;;;; version functions

(define-inline (vicare-expat-version-interface-current)
  (foreign-call "ikrt_expat_version_interface_current"))

(define-inline (vicare-expat-version-interface-revision)
  (foreign-call "ikrt_expat_version_interface_revision"))

(define-inline (vicare-expat-version-interface-age)
  (foreign-call "ikrt_expat_version_interface_age"))

(define-inline (vicare-expat-version)
  (foreign-call "ikrt_expat_version"))


;;;; done

#| end of library |# )

;;; end of file
