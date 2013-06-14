#! /bin/bash
#-*- coding: utf-8 -*-
# Author: qjp
# Date: <2013-06-14 Fri>

project_dir="$HOME/Programs/BachelorThesis"
figure_dir="$project_dir/Thesis/figures"
material_dir="$project_dir/Data/material"
for dir in `ls $figure_dir`;
do
    for fig in `ls $figure_dir/$dir`;
    do
        if [ $material_dir/$fig -nt $fig ]; then
            cp $material_dir/$fig $figure_dir/$dir
        fi
    done
done
