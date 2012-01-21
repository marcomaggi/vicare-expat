## Process this file with automake to produce Makefile.in

vicare_expat_CURRENT	= @vicare_expat_VERSION_INTERFACE_CURRENT@
vicare_expat_REVISION	= @vicare_expat_VERSION_INTERFACE_REVISION@
vicare_expat_AGE	= @vicare_expat_VERSION_INTERFACE_AGE@
vicare_expat_VERSION	= $(vicare_expat_CURRENT):$(vicare_expat_REVISION):$(vicare_expat_AGE)

lib_LTLIBRARIES			= libvicare-expat.la
libvicare_expat_la_LDFLAGS	= -version-info $(vicare_expat_VERSION)
libvicare_expat_la_SOURCES	= vicare.c

### end of file
