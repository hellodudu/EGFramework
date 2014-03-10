#!/bin/sh

function compile_all() {
    cd proto
    erlc dev.erl
    erl -detached -s dev compile_all
    rm -f dev.beam
    cd ..
    find src -name "*.erl" | xargs erlc -I "include" -o "ebin";
    echo "Compiled"
}

function compile() {
    cd proto
    erlc dev.erl
    erl -detached -s dev compile
    rm -f dev.beam
    cd ..
    for SOURCE_FILE in $(find src -type f -name "*.erl");
    do
	SOURCE_FILE_LAST_MODIFIED_TIME=`stat --format='%Y' ${SOURCE_FILE}`
	FILE=$(basename ${SOURCE_FILE})
	TARGET_FILE_EXTENSION=`echo ${FILE} | sed 's/\(.*\.\)erl/\1beam/' `
	TARGET_FILE="ebin/${TARGET_FILE_EXTENSION}"
	TARGET_FILE_LAST_MODIFIED_TIME=0
	test -f ${TARGET_FILE} && TARGET_FILE_LAST_MODIFIED_TIME=`stat --format='%Y' ${TARGET_FILE}`
	if test ${SOURCE_FILE_LAST_MODIFIED_TIME} -gt ${TARGET_FILE_LAST_MODIFIED_TIME};then
	    erlc -I "include" -o "ebin" ${SOURCE_FILE}
	    echo "Compile ${SOURCE_FILE} into ${TARGET_FILE}"
	fi;
    done
    echo "Compiled"
}

function usage() {
    echo "Wrong Command."
}

COMMAND=$1
shift

case ${COMMAND} in
    compile) compile;;
    compile_all) compile_all;;
    *) usage;;
esac


    
