#! /bin/sh
if [ -z "$WOGGLE_TOP" ]; then
    echo set WOGGLE_TOP please.
    exit 1
fi
ESDL_TOP=$WOGGLE_TOP/..

MWOGGLE_TOP=`cygpath -m $WOGGLE_TOP`
MESDL_TOP=`cygpath -m $ESDL_TOP`

werl -pa $MESDL_TOP/ebin -pa $MWOGGLE_TOP/test -s woggle_test go