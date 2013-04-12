## Process this file with automake to produce Makefile.in

EXTRA_DIST	= \
	vicare/xml/expat/constants.sls.in

# These files are hand-written.
nobase_dist_libvicare_DATA = \
	vicare/xml/expat.sls

# These files are created at configuration or make time.
nobase_nodist_libvicare2_DATA = \
	vicare/xml/expat/constants.sls		\
	vicare/xml/expat/features.sls

if WANT_NAUSICAA
nobase_dist_libvicare_DATA	+= nausicaa/xml/expat.vicare.sls
endif

VICARE_LIB_INFIX	= xml/expat

include $(top_srcdir)/meta/makefiles/lib.am

### end of file
