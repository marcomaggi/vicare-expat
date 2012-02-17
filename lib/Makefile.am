## Process this file with automake to produce Makefile.in

libvicaredir	= $(libdir)/scheme

EXTRA_DIST	=			\
	compile-all.sps			\
	compile-nausicaa.sps		\
	vicare/expat/constants.sls.in

nobase_dist_libvicare_DATA =		\
	vicare/expat.sls		\
	vicare/expat/constants.sls

if WANT_NAUSICAA
nobase_dist_libvicare_DATA	+= nausicaa/xml/expat.vicare.sls
endif

## --------------------------------------------------------------------

VICARE_FLAGS	= $(user_flags)

VICARE		= @VICARE@
VICARE_ENV	= VICARE_LIBRARY_PATH=$(builddir):$(srcdir)	\
		  LD_LIBRARY_PATH=$(builddir)/../src/.libs
VICARE_RUN	= $(VICARE_ENV) $(VICARE) $(VICARE_FLAGS)

VICARE_INST	= $(bindir)/vicare
VICARE_INST_ENV	= VICARE_LIBRARY_PATH=$(VICARE_LIBRARY_PATH)
VICARE_INST_RUN	= $(VICARE_INST_ENV) $(VICARE_INST)

.PHONY: vfasl vfasl-installed

vfasl:
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-all.sps
if WANT_NAUSICAA
	$(VICARE_RUN) --compile-dependencies $(srcdir)/compile-nausicaa.sps
endif

vfasl-installed:
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-all.sps
if WANT_NAUSICAA
	$(VICARE_INST_RUN) --compile-dependencies $(srcdir)/compile-nausicaa.sps
endif

### end of file
