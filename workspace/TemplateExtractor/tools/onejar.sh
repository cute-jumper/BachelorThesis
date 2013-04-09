#!/bin/bash
# Author: qjp
# Date: <2013-04-09 Tue>

if [ $# -ne 1 ]; then
    echo "Error! You must supply the main class!"
    exit
fi

sed -i "s/\(.*Some(\"\)\(.*\)\(\")\)/\1$1\3/g" build.sbt && sbt one-jar 
