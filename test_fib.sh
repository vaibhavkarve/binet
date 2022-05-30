#!/usr/bin/bash
. ./fib.sh --source-only


test_fib ()
{
    fib_function=$1
    echo "Testing fib function: $fib_function"
    answers=(1 1 2 3 5 8 13 21 34 55 89)
    n_values=(1 2 3 4 5 6 7 8 9 10 11)
    for index in ${!answers[@]}
    do
	fib_answer=`$fib_function ${n_values[index]}`
	if ((fib_answer == answers[index]))
	then
	    echo -n "."
	else
	    echo "F"
	    echo $fib_function ${n_values[index]} = $fib_answer does not equal ${answers[index]} >&2
	    exit 1
	fi
    done
    echo

}

test_fib fib_naive
test_fib fib_case
test_fib fib_accumulator
