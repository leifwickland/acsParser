#! /bin/bash
export COLUMNS=$COLUMNS
java -jar `dirname $0`/acs.jar $* | sort -n 
