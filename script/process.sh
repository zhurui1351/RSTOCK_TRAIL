#!/bin/bash
rm -rf dest/*
cd src
export LC_ALL=C
for f in *.TXT
do
sed 's/^M//g' $f | sed '1,2d;$d' >../dest/$f
done


