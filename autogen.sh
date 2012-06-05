# autogen.sh --
#
# Run this in the top source directory to rebuild the infrastructure.

set -xe

autoreconf --warnings=all --install --verbose
# --force

# libtoolize
# aclocal
# autoheader
# automake --foreign --add-missing
# autoconf

### end of file
