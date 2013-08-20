## Process this file with automake to produce Makefile.in

VICARE_EXPAT_ALL_TESTS	= \
	test-vicare-expat-version.sps		\
	test-vicare-expat-core.sps

if WANT_NAUSICAA
VICARE_EXPAT_ALL_TESTS	+= test-vicare-nausicaa.sps
endif

TESTS	= $(VICARE_EXPAT_ALL_TESTS)

EXTRA_DIST = \
	$(VICARE_EXPAT_ALL_TESTS)	\
	\
	demo-for-documentation.sps	\
	demo-for-nau-documentation.sps

include $(top_srcdir)/meta/makefiles/tests.am

### end of file
