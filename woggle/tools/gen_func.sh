#! /bin/sh

ERL_PLACEHOLDER='%% PLACEHOLDER_FOR_GENERATED_FUNCTIONS'
C_PLACEHOLDER='PLACEHOLDER_FOR_GENERATED_FUNCTIONS'
TEMPFILE_TEMPLATE=/tmp/gen_func
FINAL_COMMAND=""

usage ()
{
    echo "Usage: $0 {woggle|compat} {call|cast} <function name in erlang>"
    exit 1
}

erlang_source ()
{
    echo "adding to $2:"
    TEMPFILE=${TEMPFILE_TEMPLATE}_1.tmp
    cp $2 $TEMPFILE
    ed $TEMPFILE > /dev/null 2>&1 <<EOF
/$ERL_PLACEHOLDER/
i
$1() ->
    $CALLTYPE($ERLOPNAME,<<>>).

.
w
EOF
    diff -c $2 $TEMPFILE
    echo -n 'edit OK? [<RET> = continue,^C = quit]'
    read dummy
    FINAL_COMMAND="$FINAL_COMMAND cp $TEMPFILE $2;"
}

erlang_opcode ()
{
    last=`grep "$ERL_PLACEHOLDER" $2 | sed "s,$ERL_PLACEHOLDER:\([^ ]*\).*$,\1,g"`
    echo "adding to $2:"
    TEMPFILE=${TEMPFILE_TEMPLATE}_2.tmp
    cp $2 $TEMPFILE
    ed $TEMPFILE > /dev/null 2>&1 <<EOF
/$ERL_PLACEHOLDER/
s/$last/$1/g
i
-define($1, ($last + 1)).

.
w
EOF
    diff -c $2 $TEMPFILE
    echo -n 'edit OK? [<RET> = continue,^C = quit]'
    read dummy
    FINAL_COMMAND="$FINAL_COMMAND cp $TEMPFILE $2;"
}

c_opcode ()
{
    last=`grep "$C_PLACEHOLDER" $3 | sed "s,/\*$C_PLACEHOLDER:\([^ ]*\).*\*/$,\1,g"`
    echo "adding to $3:"
    TEMPFILE=${TEMPFILE_TEMPLATE}_3.tmp
    cp $3 $TEMPFILE
    ed $TEMPFILE > /dev/null 2>&1 <<EOF
/$C_PLACEHOLDER/
s/$last/$1/g
i
#define $1 ($last + 1)
void $2(sdl_data *sd, int len, char *buff);

.
w
EOF
    diff -c $3 $TEMPFILE
    echo -n 'edit OK? [<RET> = continue,^C = quit]'
    read dummy
    FINAL_COMMAND="$FINAL_COMMAND cp $TEMPFILE $3;"
}

c_opcode_compat ()
{
    echo "adding to $2:"
    TEMPFILE=${TEMPFILE_TEMPLATE}_3.tmp
    cp $2 $TEMPFILE
    ed $TEMPFILE > /dev/null 2>&1 <<EOF
/$C_PLACEHOLDER/
i
void $1(sdl_data *sd, int len, char *buff);

.
w
EOF
    diff -c $2 $TEMPFILE
    echo -n 'edit OK? [<RET> = continue,^C = quit]'
    read dummy
    FINAL_COMMAND="$FINAL_COMMAND cp $TEMPFILE $2;"
}

c_funtab ()
{
    echo "adding to $3:"
    TEMPFILE=${TEMPFILE_TEMPLATE}_4.tmp
    cp $3 $TEMPFILE
    ed $TEMPFILE > /dev/null 2>&1 <<EOF
/$C_PLACEHOLDER/
i
{ $1, "$1", $2},
.
w
EOF
    diff -c $3 $TEMPFILE
    echo -n 'edit OK? [<RET> = continue,^C = quit]'
    read dummy
    FINAL_COMMAND="$FINAL_COMMAND cp $TEMPFILE $3;"
}

c_source ()
{
    echo "adding to $2:"
    TEMPFILE=${TEMPFILE_TEMPLATE}_5.tmp
    cp $2 $TEMPFILE
    ed $TEMPFILE > /dev/null 2>&1 <<EOF
/$C_PLACEHOLDER/
i
void $1(sdl_data *sd, int len, char *buff)
{
}

.
w
EOF
    diff -c $2 $TEMPFILE
    echo -n 'edit OK? [<RET> = continue,^C = quit]'
    read dummy
    FINAL_COMMAND="$FINAL_COMMAND cp $TEMPFILE $2;"
}

if [ -z "$WOGGLE_TOP" ]; then
    echo set WOGGLE_TOP please.
    exit 1
fi

if [ $# -lt 3 ]; then
    usage
fi

if [ \( $1 != "woggle" \) -a \( $1 != "compat" \) ]; then
    usage
fi
if [ \( $2 != "call" \) -a \( $2 != "cast" \) ]; then
    usage
fi

GENTYPE=$1
CALLTYPE=$2
ERLNAME=$3
UNDERSCORED=`echo $ERLNAME | sed 's,[A-Z],_&,g' | tr "[:upper:]" "[:lower:]"`
initial=`echo $ERLNAME | sed 's,\(.\).*,\1,g'`
rest=`echo $ERLNAME | sed 's,.\(.*\),\1,g'`
cinitial=`echo $initial | tr "[:lower:]" "[:upper:]"`
if [ $cinitial = $initial ]; then
    echo "initial character of erlang name should be lowercase."
    usage
fi
CAPERLNAME=${cinitial}${rest}
if [ $GENTYPE = woggle ]; then
    ERLOPNAME=WOGGLE_$CAPERLNAME
    COPNAME=WOGGLE_${CAPERLNAME}Func
    CFUNNAME="wog_if_$UNDERSCORED"
    erlang_source $ERLNAME $WOGGLE_TOP/src/woggle.erl
    erlang_opcode $ERLOPNAME $WOGGLE_TOP/src/woggle_ops.hrl
    c_opcode $COPNAME $CFUNNAME $WOGGLE_TOP/c_src/woggle_if.h
    c_funtab $COPNAME $CFUNNAME $WOGGLE_TOP/c_src/woggle_if_fp.h
    c_source $CFUNNAME $WOGGLE_TOP/c_src/woggle_if.c
    eval "$FINAL_COMMAND echo Changes stored."
else
    matches=`grep _${CAPERLNAME}Func ../../c_src/esdl_video.h | grep -v "(.*_${CAPERLNAME}Func *+ *1 *)" | wc -l`
    
    if [ $matches -ne 1 ]; then
	echo Could not resolve opcode for $ERLNAME
	exit 1
    fi
    COPNAME=`grep _${CAPERLNAME}Func ../../c_src/esdl_video.h | grep -v "(.*_${CAPERLNAME}Func *+ *1 *)" | awk '{print $2}'`
    CFUNNAME="wog_compat_if_$UNDERSCORED"
    c_opcode_compat $CFUNNAME $WOGGLE_TOP/c_src/woggle_compat_if.h
    c_funtab $COPNAME $CFUNNAME $WOGGLE_TOP/c_src/woggle_compat_if_fp.h
    c_source $CFUNNAME $WOGGLE_TOP/c_src/woggle_compat_if.c
    eval "$FINAL_COMMAND echo Changes stored."
fi




