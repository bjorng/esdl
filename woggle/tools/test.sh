#!/bin/sh
if [ -z "$WOGGLE_TOP" ]; then
    echo set WOGGLE_TOP please.
    exit 1
fi
WWOGGLE_TOP=`cygpath -d $WOGGLE_TOP`

cmd.exe /c $WWOGGLE_TOP\\c_src\\woggle_test.exe
