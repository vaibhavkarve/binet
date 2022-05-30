#!/usr/bin/bash


fib_naive ()
# Attempt naive calculation of the nth fibonacci number.
{
    n=$1
    if (($n < 2))
    then
	echo $n
    else
	echo $((`fib_naive $(($n - 1))` + `fib_naive $(($n - 2))`))
    fi
}


fib_case ()
# Attempt naive but using case-syntax.
{
    n=$1
    case $n in
	0 | 1)
	    echo $n;;
	*)
	    echo $((`fib_case $((n - 1))` + `fib_case $((n - 2))`));;
    esac
}


fib_memoized ()
# Memoization not possible in a bash script.
# Atleast not out-of-the-box.
{
 exit 1
}


fib_accumulator ()
# Recursively compute the fibonacci numbers using accumulated values.
{
    n=$1
    a=${2:-0}  # default value 0
    b=${3:-1}  # default value 1
    case $n in
	0)
	    echo $a;;
	*)
	    echo `fib_accumulator $((n-1)) $b $((a+b))`;;
    esac
}


fib_binet ()
{
    n=$1
    sqrt5=`bc <<< "scale=10; sqrt5 = sqrt(5); phi = (1 + sqrt5) / 2; phi ^ $n"`
    echo $sqrt5 -------
}

main ()
{
    echo `fib_naive 1`
    echo `fib_naive 10`
    ###
    echo `fib_case 1`
    echo `fib_case 10`
    ###
    echo `fib_memoized 1`
    echo `fib_memoized 10`
    ###
    echo `fib_accumulator 1`
    echo `fib_accumulator 10`
    ###
    echo `fib_binet 1`
    echo `fib_binet 10`
    ###
    echo Exiting with status: $?
}


if [ "${1}" != "--source-only" ]; then
    main "${@}"
fi
