#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2013-06-03 Mon>

SERVER_ADDRESS="qiujunpeng@118.192.65.79"
SERVER_PROJECT_DIR="$SERVER_HOME/workspace/TemplateExtractor"
CLIENT_PROJECT_DIR="$HOME/Programs/BachelorThesis/workspace/TemplateExtractor"
CLIENT_RSYNC_EXCLUDE="bin
project
target
scratch.sc
.*"

exclude=""
for i in $CLIENT_RSYNC_EXCLUDE
do
    exclude="$exclude --exclude=$i"
done
rsync -azv --delete -e ssh $exclude $CLIENT_PROJECT_DIR/ $SERVER_ADDRESS:$SERVER_PROJECT_DIR/

