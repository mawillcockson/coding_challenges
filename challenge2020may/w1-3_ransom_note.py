class Solution:
    def canConstruct(self, ransomNote: str, magazine: str) -> bool:
        # Quick case
        if len(ransomNote) > len(magazine):
            return False

        for letter in ransomNote:
            if letter in magazine:
                magazine = magazine.replace(letter, "", 1)
            else:
                return False

        return True

# NOTE: Works, and crashes type checker! 😀 see screenshot
min_str_len = 1
max_str_len = 20
tests = 10
extra = 10000
assert max_str_len >= max_str_len, "Notes must be longer than Magazines"
from string import ascii_lowercase as letters
from random import choices, randint, sample
from typing import Iterator, Tuple, Callable
from itertools import repeat, starmap, islice
random_mag: Callable[[], str] = lambda: "".join(choices(letters, k=randint(min_str_len, max_str_len)))
random_note: Callable[[str], str] = lambda s: "".join(sample(s, k=randint(min_str_len, len(s))))
random_pairs: Iterator[Tuple[str, str]] = ((random_mag(), random_mag()) for i in repeat(None))
flipped_random_success: Iterator[Tuple[str, str]] = ((a := random_mag(), random_note(a)) for i in repeat(None))
random_success = ((b,a) for (a,b) in flipped_random_success)
possible = Solution().canConstruct
print(f"RANDOM\n{'note'.center(max_str_len)} {'mag'.center(max_str_len)}")
for (i, (note, mag)) in enumerate(random_pairs):
    if i >= tests: break
    print(f"{note.rjust(max_str_len)} {mag.rjust(max_str_len)} -> {possible(note, mag)}")
print(f"SUCCESSFUL\n{'note'.center(max_str_len)} {'mag'.center(max_str_len)}")
for (i, (note, mag)) in enumerate(random_success):
    if i >= tests: break
    print(f"{note.rjust(max_str_len)} {mag.rjust(max_str_len)} -> {possible(note, mag)}")
print(f"Out of {extra} test{'s' if extra > 1 else ''}, {extra - sum(starmap(possible, islice(random_success, extra)))} failed")
