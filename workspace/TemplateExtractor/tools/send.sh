#! /bin/bash
# Author: qjp
# Date: <2013-04-17 Wed>

PROJECT_DIR="$HOME/Programs/BachelorThesis/workspace/TemplateExtractor"
JAR_PATH="$PROJECT_DIR/target/scala-2.9.2/templateextractor_2.9.2-0.1-SNAPSHOT.jar"
ONE_JAR_PATH="$PROJECT_DIR/target/scala-2.9.2/templateextractor_2.9.2-0.1-SNAPSHOT-one-jar.jar"
CLASSES_DIR="$PROJECT_DIR/target/scala-2.9.2/classes"

if [ $# -ne 1 ]; then
    echo "Error! You must supply the main class!"
    exit
fi

mainClass=$1

change_conf() {
    cd $CLASSES_DIR
    mv application.conf $PROJECT_DIR/conf/application.conf.client
    mv $PROJECT_DIR/conf/application.conf.server application.conf
}

change_back() {
    cd $CLASSES_DIR
    mv application.conf $PROJECT_DIR/conf/application.conf.server
    mv $PROJECT_DIR/conf/application.conf.client application.conf
}

make_jar() {
    rm $ONE_JAR_PATH $JAR_PATH
    cd $PROJECT_DIR
    tools/onejar.sh $mainClass
}

copy_to_server() {
    scp $ONE_JAR_PATH qiujunpeng@166.111.138.18:/home/qiujunpeng/prog/
}

change_conf
make_jar
change_back
copy_to_server
