#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2013-06-17 Mon>

SERVER_ADDRESS="qiujunpeng@192.168.201.214"
SERVER_HOME="/home/qiujunpeng"
SERVER_PROJECT_DIR="$SERVER_HOME/workspace/TemplateExtractor"
SERVER_ONE_JAR_PATH="$SERVER_PROJECT_DIR/target/scala-2.10/templateextractor_2.10-0.1-SNAPSHOT-one-jar.jar"
PROJECT_DIR="$HOME/Programs/BachelorThesis/workspace/TemplateExtractorDemo"

scp $SERVER_ADDRESS:$SERVER_ONE_JAR_PATH $PROJECT_DIR/lib
