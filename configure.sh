# configure.sh --
#
# Run this to configure.

set -xe

prefix=/usr/local

../configure \
    --enable-maintainer-mode                    \
    --config-cache                              \
    --cache-file=../config.cache                \
    --prefix="${prefix}"                        \
    CFLAGS='-Wall -O3 -march=i686 -mtune=i686'  \
    "$@"

### end of file
