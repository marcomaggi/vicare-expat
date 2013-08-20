/*
  Part of: Vicare/Expat
  Contents: print platform features library
  Date: Fri Apr 12, 2013

  Abstract



  Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>


int
main (int argc, const char *const argv[])
{
  printf(";;; -*- coding: utf-8-unix -*-\n\
;;;\n\
;;;Part of: Vicare/Expat\n\
;;;Contents: static platform inspection\n\
;;;Date: Fri Apr 12, 2013\n\
;;;\n\
;;;Abstract\n\
;;;\n\
;;;\n\
;;;\n\
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>\n\
;;;\n\
;;;This program is free software:  you can redistribute it and/or modify\n\
;;;it under the terms of the  GNU General Public License as published by\n\
;;;the Free Software Foundation, either version 3 of the License, or (at\n\
;;;your option) any later version.\n\
;;;\n\
;;;This program is  distributed in the hope that it  will be useful, but\n\
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of\n\
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU\n\
;;;General Public License for more details.\n\
;;;\n\
;;;You should  have received a  copy of  the GNU General  Public License\n\
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
;;;\n\
\n\
\n\
#!r6rs\n\
(library (vicare xml expat features)\n\
  (export\n\
    HAVE_XML_DEFAULTCURRENT\n\
    HAVE_XML_ERRORSTRING\n\
    HAVE_XML_EXPATVERSIONINFO\n\
    HAVE_XML_EXPATVERSION\n\
    HAVE_XML_EXTERNALENTITYPARSERCREATE\n\
    HAVE_XML_FREECONTENTMODEL\n\
    HAVE_XML_GETBASE\n\
    HAVE_XML_GETBUFFER\n\
    HAVE_XML_GETCURRENTBYTECOUNT\n\
    HAVE_XML_GETCURRENTBYTEINDEX\n\
    HAVE_XML_GETCURRENTCOLUMNNUMBER\n\
    HAVE_XML_GETCURRENTLINENUMBER\n\
    HAVE_XML_GETERRORCODE\n\
    HAVE_XML_GETFEATURELIST\n\
    HAVE_XML_GETIDATTRIBUTEINDEX\n\
    HAVE_XML_GETINPUTCONTEXT\n\
    HAVE_XML_GETPARSINGSTATUS\n\
    HAVE_XML_GETSPECIFIEDATTRIBUTECOUNT\n\
    HAVE_XML_GETUSERDATA\n\
    HAVE_XML_PARSEBUFFER\n\
    HAVE_XML_PARSERCREATENS\n\
    HAVE_XML_PARSERCREATE\n\
    HAVE_XML_PARSERCREATE_MM\n\
    HAVE_XML_PARSERFREE\n\
    HAVE_XML_PARSERRESET\n\
    HAVE_XML_PARSE\n\
    HAVE_XML_RESUMEPARSER\n\
    HAVE_XML_SETATTLISTDECLHANDLER\n\
    HAVE_XML_SETBASE\n\
    HAVE_XML_SETCDATASECTIONHANDLER\n\
    HAVE_XML_SETCHARACTERDATAHANDLER\n\
    HAVE_XML_SETCOMMENTHANDLER\n\
    HAVE_XML_SETDEFAULTHANDLEREXPAND\n\
    HAVE_XML_SETDEFAULTHANDLER\n\
    HAVE_XML_SETDOCTYPEDECLHANDLER\n\
    HAVE_XML_SETELEMENTDECLHANDLER\n\
    HAVE_XML_SETELEMENTHANDLER\n\
    HAVE_XML_SETENCODING\n\
    HAVE_XML_SETENDCDATASECTIONHANDLER\n\
    HAVE_XML_SETENDDOCTYPEDECLHANDLER\n\
    HAVE_XML_SETENDELEMENTHANDLER\n\
    HAVE_XML_SETENDNAMESPACEDECLHANDLER\n\
    HAVE_XML_SETENTITYDECLHANDLER\n\
    HAVE_XML_SETEXTERNALENTITYREFHANDLERARG\n\
    HAVE_XML_SETEXTERNALENTITYREFHANDLER\n\
    HAVE_XML_SETNAMESPACEDECLHANDLER\n\
    HAVE_XML_SETNOTATIONDECLHANDLER\n\
    HAVE_XML_SETNOTSTANDALONEHANDLER\n\
    HAVE_XML_SETPARAMENTITYPARSING\n\
    HAVE_XML_SETPROCESSINGINSTRUCTIONHANDLER\n\
    HAVE_XML_SETRETURNNSTRIPLET\n\
    HAVE_XML_SETSKIPPEDENTITYHANDLER\n\
    HAVE_XML_SETSTARTCDATASECTIONHANDLER\n\
    HAVE_XML_SETSTARTDOCTYPEDECLHANDLER\n\
    HAVE_XML_SETSTARTELEMENTHANDLER\n\
    HAVE_XML_SETSTARTNAMESPACEDECLHANDLER\n\
    HAVE_XML_SETUNKNOWNENCODINGHANDLER\n\
    HAVE_XML_SETUNPARSEDENTITYDECLHANDLER\n\
    HAVE_XML_SETUSERDATA\n\
    HAVE_XML_SETXMLDECLHANDLER\n\
    HAVE_XML_STOPPARSER\n\
    HAVE_XML_USEFOREIGNDTD\n\
    HAVE_XML_USEPARSERASHANDLERARG\n\
    )\n\
  (import (rnrs))\n\
\n\
;;;; helpers\n\
\n\
(define-syntax define-inline-constant\n\
  (syntax-rules ()\n\
    ((_ ?name ?value)\n\
     (define-syntax ?name (identifier-syntax ?value)))))\n\
\n\
\n\
;;;; code\n\n");


printf("(define-inline-constant HAVE_XML_DEFAULTCURRENT %s)\n",
#ifdef HAVE_XML_DEFAULTCURRENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_ERRORSTRING %s)\n",
#ifdef HAVE_XML_ERRORSTRING
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_EXPATVERSION %s)\n",
#ifdef HAVE_XML_EXPATVERSION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_EXPATVERSIONINFO %s)\n",
#ifdef HAVE_XML_EXPATVERSIONINFO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_EXTERNALENTITYPARSERCREATE %s)\n",
#ifdef HAVE_XML_EXTERNALENTITYPARSERCREATE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_FREECONTENTMODEL %s)\n",
#ifdef HAVE_XML_FREECONTENTMODEL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETBASE %s)\n",
#ifdef HAVE_XML_GETBASE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETBUFFER %s)\n",
#ifdef HAVE_XML_GETBUFFER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETCURRENTBYTECOUNT %s)\n",
#ifdef HAVE_XML_GETCURRENTBYTECOUNT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETCURRENTBYTEINDEX %s)\n",
#ifdef HAVE_XML_GETCURRENTBYTEINDEX
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETCURRENTCOLUMNNUMBER %s)\n",
#ifdef HAVE_XML_GETCURRENTCOLUMNNUMBER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETCURRENTLINENUMBER %s)\n",
#ifdef HAVE_XML_GETCURRENTLINENUMBER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETERRORCODE %s)\n",
#ifdef HAVE_XML_GETERRORCODE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETFEATURELIST %s)\n",
#ifdef HAVE_XML_GETFEATURELIST
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETIDATTRIBUTEINDEX %s)\n",
#ifdef HAVE_XML_GETIDATTRIBUTEINDEX
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETINPUTCONTEXT %s)\n",
#ifdef HAVE_XML_GETINPUTCONTEXT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETPARSINGSTATUS %s)\n",
#ifdef HAVE_XML_GETPARSINGSTATUS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETSPECIFIEDATTRIBUTECOUNT %s)\n",
#ifdef HAVE_XML_GETSPECIFIEDATTRIBUTECOUNT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_GETUSERDATA %s)\n",
#ifdef HAVE_XML_GETUSERDATA
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_PARSE %s)\n",
#ifdef HAVE_XML_PARSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_PARSEBUFFER %s)\n",
#ifdef HAVE_XML_PARSEBUFFER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_PARSERCREATE %s)\n",
#ifdef HAVE_XML_PARSERCREATE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_PARSERCREATENS %s)\n",
#ifdef HAVE_XML_PARSERCREATENS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_PARSERCREATE_MM %s)\n",
#ifdef HAVE_XML_PARSERCREATE_MM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_PARSERFREE %s)\n",
#ifdef HAVE_XML_PARSERFREE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_PARSERRESET %s)\n",
#ifdef HAVE_XML_PARSERRESET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_RESUMEPARSER %s)\n",
#ifdef HAVE_XML_RESUMEPARSER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETATTLISTDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETATTLISTDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETBASE %s)\n",
#ifdef HAVE_XML_SETBASE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETCDATASECTIONHANDLER %s)\n",
#ifdef HAVE_XML_SETCDATASECTIONHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETCHARACTERDATAHANDLER %s)\n",
#ifdef HAVE_XML_SETCHARACTERDATAHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETCOMMENTHANDLER %s)\n",
#ifdef HAVE_XML_SETCOMMENTHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETDEFAULTHANDLER %s)\n",
#ifdef HAVE_XML_SETDEFAULTHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETDEFAULTHANDLEREXPAND %s)\n",
#ifdef HAVE_XML_SETDEFAULTHANDLEREXPAND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETDOCTYPEDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETDOCTYPEDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETELEMENTDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETELEMENTDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETELEMENTHANDLER %s)\n",
#ifdef HAVE_XML_SETELEMENTHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETENCODING %s)\n",
#ifdef HAVE_XML_SETENCODING
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETENDCDATASECTIONHANDLER %s)\n",
#ifdef HAVE_XML_SETENDCDATASECTIONHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETENDDOCTYPEDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETENDDOCTYPEDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETENDELEMENTHANDLER %s)\n",
#ifdef HAVE_XML_SETENDELEMENTHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETENDNAMESPACEDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETENDNAMESPACEDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETENTITYDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETENTITYDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETEXTERNALENTITYREFHANDLER %s)\n",
#ifdef HAVE_XML_SETEXTERNALENTITYREFHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETEXTERNALENTITYREFHANDLERARG %s)\n",
#ifdef HAVE_XML_SETEXTERNALENTITYREFHANDLERARG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETNAMESPACEDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETNAMESPACEDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETNOTSTANDALONEHANDLER %s)\n",
#ifdef HAVE_XML_SETNOTSTANDALONEHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETNOTATIONDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETNOTATIONDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETPARAMENTITYPARSING %s)\n",
#ifdef HAVE_XML_SETPARAMENTITYPARSING
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETPROCESSINGINSTRUCTIONHANDLER %s)\n",
#ifdef HAVE_XML_SETPROCESSINGINSTRUCTIONHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETRETURNNSTRIPLET %s)\n",
#ifdef HAVE_XML_SETRETURNNSTRIPLET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETSKIPPEDENTITYHANDLER %s)\n",
#ifdef HAVE_XML_SETSKIPPEDENTITYHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETSTARTCDATASECTIONHANDLER %s)\n",
#ifdef HAVE_XML_SETSTARTCDATASECTIONHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETSTARTDOCTYPEDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETSTARTDOCTYPEDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETSTARTELEMENTHANDLER %s)\n",
#ifdef HAVE_XML_SETSTARTELEMENTHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETSTARTNAMESPACEDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETSTARTNAMESPACEDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETUNKNOWNENCODINGHANDLER %s)\n",
#ifdef HAVE_XML_SETUNKNOWNENCODINGHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETUNPARSEDENTITYDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETUNPARSEDENTITYDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETUSERDATA %s)\n",
#ifdef HAVE_XML_SETUSERDATA
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_SETXMLDECLHANDLER %s)\n",
#ifdef HAVE_XML_SETXMLDECLHANDLER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_STOPPARSER %s)\n",
#ifdef HAVE_XML_STOPPARSER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_USEFOREIGNDTD %s)\n",
#ifdef HAVE_XML_USEFOREIGNDTD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_XML_USEPARSERASHANDLERARG %s)\n",
#ifdef HAVE_XML_USEPARSERASHANDLERARG
  "#t"
#else
  "#f"
#endif
  );


  printf("\n\
;;;; done\n\
\n\
)\n\
\n\
;;; end of file\n");
  exit(EXIT_SUCCESS);
}

/* end of file */
