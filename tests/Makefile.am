## Process this file with automake to produce Makefile.in

TESTS	= \
	test-vicare-expat.sps

if WANT_NAUSICAA
TESTS	+= test-vicare-nausicaa.sps
endif

EXTRA_DIST = \
	test-vicare-template.sps	\
	test-vicare-nausicaa.sps	\
	\
	demo-for-documentation.sps	\
	demo-for-nau-documentation.sps

include $(top_srcdir)/meta/makefiles/tests.am

### end of file
