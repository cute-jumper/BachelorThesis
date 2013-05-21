#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2013-05-21 Tue>

PROJECT_DIR=$(dirname $0 | sed 's|/tools||')
CONF_DIR=$PROJECT_DIR/conf
JAR_PATH="$PROJECT_DIR/target/scala-2.9.2/templateextractor_2.9.2-0.1-SNAPSHOT.jar"
ONE_JAR_PATH="$PROJECT_DIR/target/scala-2.9.2/templateextractor_2.9.2-0.1-SNAPSHOT-one-jar.jar"

if [ $# -lt 1 ]; then
    echo "Error! You must supply the main class!"
    exit
fi

mainClass=$1

change_conf() {
    cd $CONF_DIR
    mv application.conf application.conf.client
    mv application.conf.server application.conf
}

change_back() {
    cd $CONF_DIR
    mv application.conf application.conf.server
    mv application.conf.client application.conf
}

make_jar() {
    if [ -x $JAR_PATH ]; then
        rm  $JAR_PATH
    fi
    if [ -x $ONE_JAR_PATH ]; then
        rm $ONE_JAR_PATH
    fi
    cd $PROJECT_DIR
    tools/onejar.sh $mainClass
}

change_conf
make_jar
change_back
