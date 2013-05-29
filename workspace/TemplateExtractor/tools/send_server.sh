#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2013-05-21 Tue>

SERVER_ADDRESS="qiujunpeng@118.192.65.79"
SERVER_HOME="/home/qiujunpeng"
SERVER_PROJECT_DIR="$SERVER_HOME/workspace/TemplateExtractor"
SERVER_ONE_JAR_PATH="$SERVER_PROJECT_DIR/target/scala-2.10/templateextractor_2.10-0.1-SNAPSHOT-one-jar.jar"
CLIENT_PROJECT_DIR="$HOME/Programs/BachelorThesis/workspace/TemplateExtractor"
CLIENT_RSYNC_SRCS="src/
conf/
lib/
tools/
build.sbt"

if [ $# -lt 1 ]; then
    echo "Error! You must supply the main class!"
    exit
fi

mainClass=$1

rsync_source() {
    for i in $CLIENT_RSYNC_SRCS
    do
        rsync -azv --delete -e ssh $CLIENT_PROJECT_DIR/$i $SERVER_ADDRESS:$SERVER_PROJECT_DIR/$i 
    done
}

server_compile() {
    ssh $SERVER_ADDRESS -t "$SERVER_PROJECT_DIR/tools/make_jar.sh $mainClass"
}

server_run() {
    ssh $SERVER_ADDRESS -t "java -jar $SERVER_ONE_JAR_PATH"
}

rsync_source
server_compile

if [ $# -eq 2 ]; then
    server_run
fi
