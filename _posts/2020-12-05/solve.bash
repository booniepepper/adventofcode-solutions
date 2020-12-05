#!/bin/bash

max_id=0

for line in $(cat input); do
    row=$(echo ${line:0:7} | tr FB 01)
    col=$(echo ${line:7:11} | tr LR 01)
    row=$((2#$row))
    col=$((2#$col))
    id=$((row * 8 + col))
    if [[ $id -gt $max_id ]];then max_id=$id; fi
done

echo Max ID: $max_id

