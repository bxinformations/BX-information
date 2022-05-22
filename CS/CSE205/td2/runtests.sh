#!/bin/sh
# Peixin You peixin.you
succ=0
fail=0

work()
{
    for test in $(ls $1)
    do
        x=$(sh is_even $test)
        flag=$?
        if [ $flag -eq $2 ] ;
        then
            succ=$((succ + 1))
        else
            if [ $flag -eq $3 ] ;
            then
                if [ $3 -eq 1 ] ;
                then
                    echo "$test should be odd"
                else
                    echo "$test should be even"
                fi
                fail=$((fail + 1))
            else
                fail=$((fail + 1))
            fi
        fi
    done
}

work "./good" 0 1
work "./bad" 1 0

echo 'Success = '$succ
echo 'Failure = '$fail

