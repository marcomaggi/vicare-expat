;; libraries.scm --
;;
;;This file is meant  to be included by the build dependencies script,  so it must be
;;in the sources search path.

(define-constant INCLUDE-LIBRARY-BUILD-HIERARCHIES
  '((vicare xml expat)
    #;(nausicaa xml expat)
    ))

(define-constant INCLUDE-LIBRARY-DEPENDENCIES-HIERARCHIES
  '())

(define-constant INCLUDE-INSTALL-RULES
  #t)

;;These are the library files generated by the "configure" script starting from ".in"
;;templates.
;;
(define-constant FROM-TEMPLATES-SOURCE-FILES
  '("lib/vicare/xml/expat/constants.vicare.sls"))

;;These are the  library files generated by some automated  process; for example, the
;;"features"  program  that inspects  the  availability  of host  facilities  through
;;preprocessor constant generated by the GNU Autoconf infrastructure.
;;
(define-constant BUILT-SOURCE-FILES
  '())

;;This is the table of libraries to compile.  The table is a list of entries:
;;
;;   (?entry ...)
;;
;;each ?ENTRY having one of the formats:
;;
;;   ((?want-feature ...) ?library-name ...)
;;
;;where: each ?WANT-FEATURE is a symbol  defined in the "configure.ac" template using
;;AM_CONDITIONAL  from the  GNU  Automake infrastructure;  ?LIBRARY-NAME  is an  R6RS
;;library name specification.   If no ?WANT-FEATURE is present: the  libraries are to
;;be always processed.
;;
(define-constant LIBRARIES-SPECS
  '((()
     (vicare xml expat)
     (vicare xml expat constants))
    ;; ((WANT_NAUSICAA)
    ;;  (nausicaa xml expat))
    ))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
