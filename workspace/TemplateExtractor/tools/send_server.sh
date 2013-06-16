#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2013-05-21 Tue>

SERVER_ADDRESS="qiujunpeng@192.168.201.118"
SERVER_HOME="/home/qiujunpeng"
SERVER_PROJECT_DIR="$SERVER_HOME/workspace/TemplateExtractor"
SERVER_ONE_JAR_PATH="$SERVER_PROJECT_DIR/target/scala-2.10/templateextractor_2.10-0.1-SNAPSHOT-one-jar.jar"
if [ $# -lt 1 ]; then
    echo "Error! You must supply the main class!"
    exit
fi

working_dir=$(pwd)/$(dirname $0)
mainClass=$1

rsync_source() {
    $working_dir/rsync_source.sh
}

server_compile_and_run() {
    ssh $SERVER_ADDRESS -t "$SERVER_PROJECT_DIR/tools/make_jar.sh $mainClass &&\
java -jar $SERVER_ONE_JAR_PATH"
}

rsync_source
server_compile_and_run
