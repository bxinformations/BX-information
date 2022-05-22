#!/bin/sh
succ=0
fail=0

for test in $(ls './good')
do
    x=$(sh is_even $test)
    if [ $? -eq 0 ] ;
    then
        let succ=succ+1
    else
        if [ $? -eq 1 ] ;
        then
            echo $test' should be odd'
            let fail=fail+1
        else
            let fail=fail+1
        fi
    fi
done

for test in $(ls './bad')
do
    x=$(sh is_even $test)
    if [ $? -eq 0 ] ;
    then
        echo $test' should be even'
        let fail=fail+1
    else
        if [ $? -eq 1 ] ;
        then
            let succ=succ+1
        else
            let fail=fail+1
        fi
    fi
done

echo 'Success = '$succ
echo 'Failure = '$fail