#!/bin/bash
cd d:/data
rm -rf dest/*
cd src
export LC_ALL=C
for f in *.txt
do
sed 's/^M//g' $f | sed '1,2d;$d' >../dest/$f
done

cd ../dest
for f in *.txt
do
if [ ! -s $f ]; then rm $f;fi
done


