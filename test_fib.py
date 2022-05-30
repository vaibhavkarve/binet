#!/usr/bin/env python3.10
from typing import Callable

import pytest

from fib import FIBONACCI_FUNCTIONS


@pytest.mark.parametrize("n,answer", zip(
    range(1, 12),
    [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]))
@pytest.mark.parametrize("fib", FIBONACCI_FUNCTIONS)
def test_fib(fib: Callable[[int], int], n: int, answer: int) -> None:
    assert fib(n) == answer
