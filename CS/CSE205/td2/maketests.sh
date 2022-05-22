#!/bin/sh
# Peixin You peixin.you
mkdir ./good

for i in `seq 2 2 200`
do
    touch ./good/$i
done

mkdir ./bad

for i in `seq 1 2 200`
do
    touch ./bad/$i
done