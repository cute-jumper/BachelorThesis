#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2013-06-17 Mon>

server_address="qiujunpeng@192.168.201.214"
local_dir="$HOME/Programs/BachelorThesis/Data/templates"
res=$(sed -n 's/.*centerFile="\(.*\)">/\1/p' $1)

for i in $res;
do
    name=$local_dir/$(echo $i | awk -F \/ '{print $NF}')
    scp $server_address:$i $name
done

sed 's~/home/qiujunpeng/Data/blog_detail~'$local_dir'~' $1 > $1.template
