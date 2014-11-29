#!/bin/bash
cd dest
for f in *.TXT
do
txt="/Users/ruizhu/Desktop/stock/dest/$f"
table=${f:0:8}
insert="load data local infile '$txt' into table $table"
sql="drop table if exists $table ; create table $table (date varchar(255),high varchar(255),low varchar(255),open varchar(255),close varchar(255),volum varchar(255),amount varchar(255)); $insert ;"
mysql -uroot -proot -Dstock -e "$sql"
done
