/*
  Part of: Vicare/Expat
  Contents: internal header file
  Date: Wed Jun 10, 2015

  Abstract

	Internal header file.

  Copyright (C) 2012, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>

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

#ifndef VICARE_EXPAT_INTERNALS_H
#define VICARE_EXPAT_INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <expat.h>

#define EX_PARSER_POINTER_OBJECT(PARSER)	(PARSER)
#define EX_PARSER(PARSER)			\
  IK_POINTER_DATA_VOIDP(EX_PARSER_POINTER_OBJECT(PARSER))

#define EX_CALLBACK(CALLBACK)	IK_POINTER_DATA_VOIDP(CALLBACK)

#define EX_BOOLEAN(BOOL)	((IK_FALSE == (BOOL))? XML_FALSE : XML_TRUE)

#define BOOLEAN_TO_INT(BOOL)	((IK_FALSE == (BOOL))? 0 : 1)


/** --------------------------------------------------------------------
 ** Support for missing functions.
 ** ----------------------------------------------------------------- */

static IK_UNUSED void
feature_failure_ (const char * funcname)
{
  ik_abort("called unavailable Expat specific function, %s\n", funcname);
}

#define feature_failure(FN)     { feature_failure_(FN); return IK_VOID; }


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#endif /* VICARE_EXPAT_INTERNALS_H */

/* end of file */
