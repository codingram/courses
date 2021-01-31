def permutations(sequence: str) -> list:
    """
    Enumerate all permutations of a given string
    sequence (string): an arbitrary string to permute.
    Returns: a list of all permutations of sequence
    """
    if len(sequence) == 1:
        return [sequence]
    else:
        result = []
        for i, let in enumerate(sequence):
            for n in permutations(sequence[:i] + sequence[i + 1 :]):
                result += [let + n]
        return result


def permutations_gen(items):
    """Permutations generator"""
    if len(items) == 0:
        yield ""
    else:
        for i in range(len(items)):
            for cc in permutations_gen(items[:i] + items[i + 1 :]):
                yield items[i] + "".join(cc)


def k_permutations(items, k):
    """k permutations generator"""
    if k == 0:
        yield []
    else:
        for item in items:
            for kp in k_permutations(items, k - 1):
                if item not in kp:
                    yield [item] + kp


def test():
    TESTS_PERM = [
        ("", []),
        ("a", ["a"]),
        ("ab", ["ab", "ba"]),
        ("abc", ["abc", "acb", "bac", "bca", "cab", "cba"]),
        (
            "abcd",
            [
                "abcd",
                "abdc",
                "acbd",
                "acdb",
                "adbc",
                "adcb",
                "bacd",
                "badc",
                "bcad",
                "bcda",
                "bdac",
                "bdca",
                "cabd",
                "cadb",
                "cbad",
                "cbda",
                "cdab",
                "cdba",
                "dabc",
                "dacb",
                "dbac",
                "dbca",
                "dcab",
                "dcba",
            ],
        ),
    ]

    for t in TESTS_PERM:
        expected = t[1]
        try:
            perm = permutations(t[0])
            try:
                assert perm == expected
                print(f"PASSED: {perm}")
            except AssertionError:
                print(f"FAILED: Expected {expected} but got {perm}")
        except Exception as err:
            print("Unexpected error:", err)


if __name__ == "__main__":
    test()
