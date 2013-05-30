#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2013-05-21 Tue>

SERVER_ADDRESS="qiujunpeng@118.192.65.79"
SERVER_HOME="/home/qiujunpeng"
SERVER_PROJECT_DIR="$SERVER_HOME/workspace/TemplateExtractor"
SERVER_ONE_JAR_PATH="$SERVER_PROJECT_DIR/target/scala-2.10/templateextractor_2.10-0.1-SNAPSHOT-one-jar.jar"
CLIENT_PROJECT_DIR="$HOME/Programs/BachelorThesis/workspace/TemplateExtractor"
CLIENT_RSYNC_EXCLUDE="bin
project
target
scratch.sc
.*"

if [ $# -lt 1 ]; then
    echo "Error! You must supply the main class!"
    exit
fi

mainClass=$1

rsync_source() {
    exclude=""
    for i in $CLIENT_RSYNC_EXCLUDE
    do
        exclude="$exclude --exclude=$i"
    done
    rsync -azv --delete -e ssh $exclude $CLIENT_PROJECT_DIR/ $SERVER_ADDRESS:$SERVER_PROJECT_DIR/
}

server_compile_and_run() {
    ssh $SERVER_ADDRESS -t "$SERVER_PROJECT_DIR/tools/make_jar.sh $mainClass &&\
java -jar $SERVER_ONE_JAR_PATH"
}

rsync_source
server_compile_and_run
