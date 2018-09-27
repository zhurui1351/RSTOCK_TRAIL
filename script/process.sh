#!/bin/bash

cd D:/tradingSystem/Rcode/code/RSTOCK_TRAIL/script
R CMD BATCH hyProcess.R

cd d:/data/stock
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


cd d:/data/stock/index
rm -rf dest/*
cd src
export LC_ALL=C
for f in *.txt
do
sed 's/^M//g' $f | sed '1,2d;$d' >../dest/$f
done

cd d:/data/stock/hyindex
rm -rf dest/*
cd src
export LC_ALL=C
for f in *.txt
do
sed 's/^M//g' $f | sed '1,2d;$d' >../dest/$f
done