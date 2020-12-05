#!/bin/bash

max_id=0
declare -a ids

for line in $(cat input); do
    row=$(echo ${line:0:7} | tr FB 01)
    col=$(echo ${line:7:11} | tr LR 01)
    row=$((2#$row))
    col=$((2#$col))
    id=$((row * 8 + col))
    ids+=($id)
    if [[ $id -gt $max_id ]];then max_id=$id; fi
done

echo "(part 1) Max ID: $max_id"

ids=($(echo "${ids[@]}" | sed 's/ /\n/g' | sort -n))
first_id=${ids[0]}
prev_id=$((first_id - 1))

for id in ${ids[@]}; do
    if [[ $prev_id -ne $(($id - 1)) ]]; then
        echo "(part 2) First empty chair: $(($id - 1))"
        break
    fi
    prev_id=$id
done

