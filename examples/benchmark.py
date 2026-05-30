#!/usr/bin/env python

"""
Benchmark equivalent to benchmark.kl in Python.

Mirrors the Kola version structurally:
- Same helpers (is_even, is_odd, count_evens, sum_list, map_list, range)
- Same computation phases with the same numbers
- Same result at the end

Key difference: Python uses fast built-in loops and mutable lists,
while Kola uses structural recursion (num_rec, list_rec) and persistent
immutable lists. This is "total functional interpreter" vs "mature
imperative interpreter with native data structures".
"""

import time

# --- helpers (mirroring Kola) ---


def is_even(n):
    return n % 2 == 0


def is_odd(n):
    return n % 2 != 0


def count_evens(lst):
    """Equivalent of Kola's list_rec ... count_evens."""
    acc = 0
    for x in lst:
        if is_even(x):
            acc += 1
    return acc


def sum_list(lst):
    """Equivalent of Kola's list_rec ... sum_list."""
    acc = 0
    for x in lst:
        acc += x
    return acc


def map_list(f, lst):
    """Equivalent of Kola's map_list (maps then reverses).

    In Kola the inner list_rec builds a reversed list via prepend,
    then list_reverse flips it. Here we just build forward — same result.
    """
    mapped = []
    for x in lst:
        mapped.append(f(x))
    return mapped


def range_n(n):
    """Equivalent of Kola's range: builds [0, 1, ..., n - 1]."""
    return list(range(n))


# --- main benchmark ---


def main():
    t0 = time.perf_counter()

    # Phase 1: Build a list of 2000 elements
    big_list = range_n(20000)

    # Phase 2: map is_even
    evens_list = map_list(is_even, big_list)

    # Phase 3: map is_odd
    odds_list = map_list(is_odd, big_list)

    # Phase 4: count evens in the original list
    even_count = count_evens(big_list)

    # Phase 5: sum of the big list
    total = sum_list(big_list)

    # Phase 6: build another range and chain operations
    big_list2 = range_n(15000)
    doubled = map_list(lambda x: x * 2, big_list2)
    doubled_sum = sum_list(doubled)

    # Phase 7: build a third range and do more ops
    big_list3 = range_n(10000)
    added = map_list(lambda x: x + 5, big_list3)
    added_sum = sum_list(added)

    # Phase 8: count evens in all three lists
    count1 = count_evens(big_list)
    count2 = count_evens(big_list2)
    count3 = count_evens(big_list3)

    # Build result record (dict in Python)
    result = {
        "even_count": even_count,
        "total": total,
        "doubled_sum": doubled_sum,
        "added_sum": added_sum,
        "count1": count1,
        "count2": count2,
        "count3": count3,
        "final": even_count
        + total
        + doubled_sum
        + added_sum
        + count1
        + count2
        + count3,
    }

    t1 = time.perf_counter()
    elapsed = (t1 - t0) * 1000  # ms

    print(f"Result: {result}")
    print(f"Time:   {elapsed:.2f} ms")


if __name__ == "__main__":
    main()
