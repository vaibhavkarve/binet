#!/usr/bin/env python3.10
import functools as ft
import math
import time
from typing import Final, Callable, TypeAlias


FibFunc: TypeAlias = Callable[[int], int]

def fib_naive(n: int) -> int:
    """Attempt naive calculation of the nth fibonacci number."""
    if n < 2:
        return n
    return fib_naive(n-1) + fib_naive(n-2)


def fib_case(n: int) -> int:
    """Attempt naive, but using Python 3.10's structural pattern matching sytax."""
    match n:
        case 0 | 1:
            return n
        case n:
            return fib_case(n-1) + fib_case(n-2)


@ft.lru_cache
def fib_memoized(n: int) -> int:
    """Memoize the naive calculation using functools library."""
    if n < 2:
        return n
    return fib_memoized(n-1) + fib_memoized(n-2)


def fib_accumulator(n: int, a: int = 0, b: int = 1) -> int:
    """Recursively compute the fibonacci numbers using accumulated values."""
    if not n:
        return a
    return fib_accumulator(n=n-1, a=b, b=a+b)


def fib_binet(n: int) -> int:
    """Compute nth Fibonacci number using Binet's formula.

    This is a loop-less computation!
    """
    sqrt5: float = math.sqrt(5)
    phi: float = (1 + sqrt5)/2
    return round(phi**n / sqrt5, 0)  # type: ignore

def fib_binet_log(n: int) -> int:
    """Compute nth Fibonacci number using Binet's formula.

    This is a loop-less computation!
    """
    sqrt5: float = math.sqrt(5)
    phi: float = (1 + sqrt5)/2
    phi_log: float = math.log(phi)
    return round(math.exp(phi_log*n) / sqrt5, 0)  # type: ignore


def time_fib_function(fib: FibFunc) -> None:
    """Time a given fibonacci function against a fixed large input.

    We count time using `time.perf_counter_ns` which
    - counts time in nano-seconds
    - and is the most precise time available in python.
    """
    input_n: int = 35
    answer: int = 9_227_465
    start_time: float = time.perf_counter_ns()
    assert fib(input_n) == answer
    print(f"{fib.__name__:>15}({input_n}) took {time.perf_counter_ns() - start_time:>11} ns.")


def main() -> None:
    for fib in FIBONACCI_FUNCTIONS:
        time_fib_function(fib)


FIBONACCI_FUNCTIONS: Final[tuple[FibFunc]] = (
    fib_naive,
    fib_case,
    fib_memoized,
    fib_accumulator,
    fib_binet,
    fib_binet_log,
    )


if __name__ == '__main__':
    main()
