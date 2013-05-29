#! /bin/bash
# Author: qjp
# Date: <2013-04-17 Wed>

PROJECT_DIR="$HOME/Programs/BachelorThesis/workspace/TemplateExtractor"
ONE_JAR_PATH="$PROJECT_DIR/target/scala-2.9.2/templateextractor_2.9.2-0.1-SNAPSHOT-one-jar.jar"
REMOTE_HOST="qiujunpeng@118.192.65.79"

if [ $# -lt 1 ]; then
    echo "Error! You must supply the main class!"
    exit
fi

mainClass=$1

copy_to_server() {
    scp $ONE_JAR_PATH $REMOTE_HOST:/home/qiujunpeng/tmp/
}

client_compile() {
    $PROJECT_DIR/tools/make_jar.sh $mainClass
}

server_run() {
    ssh $REMOTE_HOST -t "cd tmp && java -jar templateextractor_2.9.2-0.1-SNAPSHOT-one-jar.jar"
}

client_compile
copy_to_server

if [ $# -eq 2 ]; then
    server_run
fi
