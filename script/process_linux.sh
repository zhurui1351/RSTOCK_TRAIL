#!/bin/bash
rm ../../data/dest/*
cd ../../data/stock
export LC_ALL=C
for f in *.TXT
do
if [ ! -s $f ]; then rm $f; else dos2unix $f ; fi
done

for f in *.TXT
do
sed '1,2d;$d' $f > ../dest/$f;
done

