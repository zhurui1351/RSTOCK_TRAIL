#!/bin/bash
rm ../../data/dest/*
cd ../../data/stock
export LC_ALL=C
for f in *.TXT
do
dos2unix $f
done

for f in *.TXT
do
sed '1,2d;$d' $f > ../dest/$f;
done

