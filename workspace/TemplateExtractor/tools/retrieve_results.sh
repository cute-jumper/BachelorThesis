#! /bin/sh

remote_host="qiujunpeng@118.192.65.79"
remote_user_dir="/home/qiujunpeng"
scp $remote_host:$remote_user_dir/tmp/{distFile$1,clusterFile$1,id2filename$1,templateFile$1} $HOME/tmp/

