## dependencies.make --
#
# Automatically built.

EXTRA_DIST +=  \
	lib/vicare/xml/expat/constants.vicare.sls.in

lib/vicare/xml/expat.fasl: \
		lib/vicare/xml/expat.vicare.sls \
		lib/vicare/xml/expat/constants.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_xml_expat_fasldir = $(bundledlibsdir)/vicare/xml
lib_vicare_xml_expat_vicare_slsdir  = $(bundledlibsdir)/vicare/xml
nodist_lib_vicare_xml_expat_fasl_DATA = lib/vicare/xml/expat.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_xml_expat_vicare_sls_DATA = lib/vicare/xml/expat.vicare.sls
endif
EXTRA_DIST += lib/vicare/xml/expat.vicare.sls
CLEANFILES += lib/vicare/xml/expat.fasl

lib/vicare/xml/expat/constants.fasl: \
		lib/vicare/xml/expat/constants.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_xml_expat_constants_fasldir = $(bundledlibsdir)/vicare/xml/expat
lib_vicare_xml_expat_constants_vicare_slsdir  = $(bundledlibsdir)/vicare/xml/expat
nodist_lib_vicare_xml_expat_constants_fasl_DATA = lib/vicare/xml/expat/constants.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_xml_expat_constants_vicare_sls_DATA = lib/vicare/xml/expat/constants.vicare.sls
endif
CLEANFILES += lib/vicare/xml/expat/constants.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:
