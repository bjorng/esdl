#! /bin/sh
# Note that this script expects the cygwin version of echo etc
OS=`uname -o`
if [ X$OS != XCygwin ]; then
    echo "This script is supposed to be run in the" \
	 "cygwin environement on windows only." >&2
    exit 1
fi
echo -n "Checking basic sanity... " 
WORKDIR=`pwd`

if [ ! -f "$WORKDIR/README-SDL.txt" ]; then
    echo "no."
    echo "You should have the esdl root directory as current when running" 
	 "this script." >&2
    exit 1
fi

echo "ok."

echo -n "Trying to locate MinGW gcc... "
MINGW32GCC=`which mingw32-gcc 2>/dev/null`

if [ -z "$MINGW32GCC" ]; then
    echo "no."
    echo "You need MinGW in your path." >&2 
    echo "Alter your PATH either globally or in cygwin to include MinGW" >&2
    exit 1
fi
echo $MINGW32GCC 

echo -n "Checking some more about MinGW... "
MINGWROOT=`echo $MINGW32GCC | sed 's,/bin/mingw32-gcc$,,g'`
if [ -z "$MINGWROOT" ] ; then
    echo "failure."
    echo "The MinGW root directory cannot be located (!)" >&2
    exit 1
fi

GLINCLUDE=$MINGWROOT/include/GL

if [ ! -f $GLINCLUDE/gl.h ]; then
    echo "failure."
    echo "gl.h not found in" $GLINCLUDE >&2
    exit 1
fi

GLLIB=$MINGWROOT/lib
if [ ! -f $GLLIB/libopengl32.a ]; then
    echo "failure."
    echo "No libopengl32.a found in" $GLLIB >&2
    exit 1
fi

WMINGWROOT=`cygpath -m $MINGWROOT`
WGLINCLUDE=`cygpath -m $GLINCLUDE`
WGLLIB=`cygpath -m $GLLIB`

echo "ok."

echo -n "Checking for SDL..."
SDLDLL=`which SDL.dll 2>/dev/null`

if [ -z "$SDLDLL" ]; then
    echo "no."
    echo "The SDL.dll shared library is not in PATH" >&2
    echo "The development version of the SDL libraries is needed" >&2
    echo "and the DLL itself should be in the PATH, alter PATH" \
	 "globally or in cygwin" >&2
    exit 1
fi

echo $SDLDLL
SDLROOT=`echo $SDLDLL | sed 's,/lib/SDL.dll$,,g'`
SDLROOT=`echo $SDLROOT | sed 's,/bin/SDL.dll$,,g'`

echo SDLROOT is $SDLROOT
echo -n "checking some more on SDL..."

if [ -z "$SDLROOT" ]; then
    echo "failed."
    echo "Could not locate root of SDL installation." >&2
    echo "You probably do not have the development version of the" \
	"SDL libraries" >&2
    echo "early enough in your PATH. Alter the PATH to include the LIB of " >&2
    echo "the *development libraries* before any other SDL.dll." >&2
    exit 1
fi

WSDLROOT=`cygpath -m $SDLROOT`
SDLLIB=$SDLROOT/lib
if [ -f $SDLLIB/SDL.lib ]; then
    SDL_LIBTYPE="msvc"
    SDL_LIBFLAGS=$WSDLROOT/lib/SDL.lib
    WSDLLIB=$WSDLROOT/lib
elif [ -f $SDLLIB/libSDL.a ]; then
    SDL_LIBTYPE="mingw"
    SDL_LIBFLAGS="-L$WSDLROOT/lib -lSDL"
    WSDLLIB=$WSDLROOT/lib
else
    echo "failed."
    echo "Could not find any import library for SDL." >&2
    echo "Is this *really* the *development libraries* of SDL in the PATH?" >&2
    exit 1
fi

SDLINCLUDE=$SDLROOT/include

if [ -f $SDLINCLUDE/SDL.h ]; then
    echo "ok."
elif [ -f $SDLINCLUDE/SDL/SDL.h ]; then
    SDLINCLUDE=$SDLINCLUDE/SDL
    echo "ok."
else
    echo "failed."
    echo "The header files are missing from SDL." >&2
    echo "Is this *really* the *development libraries* of SDL in the PATH?" >&2
    exit 1
fi

WSDLINCLUDE=`cygpath -m $SDLINCLUDE`

echo -n "Checking make... "
CYGMAKE=`which make 2>/dev/null`

if [ -z "$CYGMAKE" ]; then
    echo "no."
    echo "Unable to find make. It seems you need to install more cygwin" \
	"packages..." >&2
    exit 1
fi

gnutest=`make --version | head -1 | awk '{print $1}'`

if [ X$gnutest != XGNU ]; then
    echo "no."
    echo "The make you have in your path is not a GNU make." >&2
    echo "Make sure Cygwin's GNU make is installed and prior to any other" \
         "make in your path." >&2
    exit 1
fi

echo "ok."

echo -n "Looking for working erlang... "

ERLC=`which erlc 2>/dev/null`
ERL=`which erl 2>/dev/null`

require_major=5
require_minor=3

if [ -z "$ERLC" -o -z "$ERL" ]; then
    echo "no."
    echo "You need an erlang version of $require_major.$require_minor" \
	 "or higher in your PATH." >&2
    exit 1
fi

ERLTOP=`echo $ERL | sed 's,/bin/erl$,,g'`
WERLTOP=`cygpath -d "$ERLTOP"`
ERLTOP=`cygpath -u $WERLTOP`
ERLC=$ERLTOP/bin/erlc
ERL=$ERLTOP/bin/erl
ERLINC=$ERLTOP/usr/include
if [ ! -f $ERL -o ! -f $ERLC -o ! -d $ERLINC ]; then
    echo "no."
    echo "Path conversion failed. Unable to locate ERL_TOP correctly" \
	 "(bug?)." >&2
    exit 1
fi

major=0
minor=0
for x in $ERLTOP/erts-*; do
    ver=`echo $x | sed "s,^$ERLTOP/erts-,,g"`
    maj=`echo $ver | awk -F. '{print $1}'`
    min=`echo $ver | awk -F. '{print $2}'`
    if [ $maj -gt $major ]; then
	major=$maj
    fi
    if [ $min -gt $minor ]; then
	minor=$min
    fi
done

if [ $major -lt $require_major -o \
     \( $major = $require_major -a $minor -lt $require_minor \) ]; then
     echo "no."
     echo "Your erlang version in PATH is too old." >&2
     echo "At least $require_major.$require_minor is required." >&2
     exit 1
fi

echo "ok."

echo
echo "Found out:"
echo "MinGW is in $MINGWROOT"
echo "MinGWGCC is $MINGW32GCC"
echo "OpenGL include dir $GLINCLUDE"
echo "OpenGL lib dir $GLLIB"
echo "SDL in $SDLROOT"
echo "SDL style is $SDL_LIBTYPE"
echo "SDL linkflags $SDL_LIBFLAGS"
echo "SDL Include dir $SDLINCLUDE"
echo "Cygwin Make is in $CYGMAKE"
echo "Erlang is in $ERLTOP"
echo
echo -n "Creating some directories if non-existing... "

CONFDIR="$WORKDIR/win32_conf"
EBIN="$WORKDIR/ebin"
PRIV="$WORKDIR/priv"
for x in "$CONFDIR" "$EBIN" "$PRIV"; do
    if [ ! -d "$x" ]; then
	echo -n "$x"
	mkdir "$x"
    fi
done

WCONFDIR=`cygpath -m "$CONFDIR"`

echo "ok."

echo -n "Converting erlang headers for MinGW... "
sed 's,##Params, Params,g' "$ERLINC/erl_win_dyn_driver.h" \
    > "$CONFDIR/erl_win_dyn_driver.h"
cp "$ERLINC/erl_driver.h" "$CONFDIR/erl_driver.h"

echo "ok."

MINGW_MAKEVARS="$CONFDIR/mingw_vars.mk"

echo -n "Creating $MINGW_MAKEVARS... "
echo '# Auto-generated -- DO NOT EDIT.' > "$MINGW_MAKEVARS"
cat >> "$MINGW_MAKEVARS" <<EOF
GL_LIBS = -L$WGLLIB -lopengl32 -lglu32
GL_INCS = -I$WGLINCLUDE
SDL_LIBS = $SDL_LIBFLAGS
SDL_INCS = -I$WSDLINCLUDE
ERL_INCS = "-I$WCONFDIR"
CC = $MINGW32GCC
CFLAGS = -g -O2 -funroll-loops -Wall -ffast-math -fomit-frame-pointer \\
         -DWIN32 -D_WIN32 -D__WIN32__ \$(GL_INCS) \$(SDL_INCS) \$(ERL_INCS)
CLINKFLAGS = -shared
SOEXT = dll
LIBS = \$(GL_LIBS) \$(SDL_LIBS) -lm
EOF

echo "ok."

BUILDSCRIPT="$WORKDIR/mingw_build.sh"
echo -n "Creating build script $BUILDSCRIPT... "
echo '#! /bin/sh' > "$BUILDSCRIPT"
echo '# Auto-generated, do not edit.' >> "$BUILDSCRIPT"
cat >> "$BUILDSCRIPT" <<EOF
SDLROOT="$SDLROOT"
PATH=$MINGWROOT/bin:$SDLROOT/lib:$ERLTOP/bin:\$PATH
export SDLROOT
export PATH
make OS_FLAG=mingw \$*
EOF

chmod u+x $BUILDSCRIPT

echo "ok."
echo
echo "Done, run $BUILDSCRIPT to compile."
echo
