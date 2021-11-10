"""
tests the solution in the named file
"""
import math
import sys
from argparse import ArgumentParser
from importlib import import_module
from itertools import chain, starmap
from pathlib import Path
from pprint import pformat
from random import randint, random
from statistics import median
from typing import Callable, List, NamedTuple, Tuple


class Parameters(NamedTuple):
    "parameters passed as star-args to the function under test"
    nums1: List[int]
    nums2: List[int]


Answer = float


class TestCase(NamedTuple):
    "a test case"
    case: Parameters
    correct_answer: Answer


Function = Callable[[List[int], List[int]], Answer]

TEST_CASES: List[Tuple[List[int], List[int], float]] = [
    (
[1513770, 1515273, 1517959, 1518453, 1518664, 1521141, 1523048, 1523213, 1524651, 1526571, 1528569, 1530370, 1530722, 1536648, 1537547, 1538983, 1542257, 1543572, 1548472, 1553784, 1553878, 1555750, 1557866, 1561980, 1562217, 1563582, 1572597, 1572616, 1578402, 1580724, 1581898, 1582502, 1583005, 1586738, 1590144, 1594880, 1602029, 1602142, 1605165, 1605219, 1606260, 1606963, 1614580, 1616583, 1619348, 1622664, 1631868, 1635508, 1636520, 1637918, 1641661, 1642894, 1645124, 1645300, 1653019, 1657687, 1658274, 1658716, 1661932, 1663630, 1663979, 1667014, 1672941, 1678407, 1678546, 1679162, 1679439, 1685080, 1686475, 1690737, 1692764, 1693225, 1693424, 1696045, 1696863, 1697127, 1697247, 1698928, 1699673, 1699801, 1701446, 1704966, 1705377, 1712514, 1713395, 1720869, 1721642, 1727337, 1729943, 1731463, 1737225, 1737788, 1738668, 1738827, 1743257, 1745798, 1755083, 1756026, 1757176, 1758095, 1758761, 1760150, 1761173, 1763165, 1767922, 1768514, 1768666, 1772707, 1774409, 1774683, 1775980, 1783663, 1783673, 1783782, 1787539, 1791118, 1791834, 1794951, 1795258, 1796030, 1796052, 1798775, 1800526, 1801051, 1803779, 1804873, 1807380, 1809885, 1810286, 1811020, 1813971, 1815507, 1817261, 1819150, 1820261, 1822971, 1823584, 1823980, 1824712, 1829176, 1831264, 1833761, 1834845, 1836563, 1836594, 1837540, 1840939, 1842248, 1846190, 1846809, 1852322, 1855423, 1856996, 1860678, 1864589, 1867323, 1867906, 1869439, 1871612, 1878280, 1878926, 1879056, 1881198, 1886859, 1888341, 1891772, 1892292, 1895949, 1896672, 1901700, 1904197, 1904693, 1908923, 1910927, 1911207, 1911257, 1915866, 1916289, 1917944, 1919467, 1923967, 1934149, 1936011, 1937435, 1939304, 1948113, 1953690, 1954070, 1958606, 1959191, 1963030, 1966117, 1969037, 1970923, 1976700, 1977938, 1978165, 1980818, 1980862, 1981082, 1982040, 1982126, 1984027, 1984446, 1985745, 1989831, 1994926, 1995015, 1997270, 2003311, 2004749, 2005010, 2006243, 2008935, 2010899, 2011405, 2012218, 2012835, 2014113, 2015749, 2018324, 2019498, 2019626, 2020229, 2020948, 2021108, 2021754, 2023151, 2029473, 2037128, 2037261, 2039117, 2045852, 2047744, 2050945, 2052316, 2055321, 2057904, 2059147, 2059887, 2060768, 2063473, 2069359, 2075118, 2076962, 2077856, 2079195, 2083139, 2084000, 2085203, 2085657, 2086408, 2089556, 2092826, 2094631, 2094948, 2095710, 2096149, 2096509, 2103550, 2104484, 2106300, 2107175, 2107320, 2107587, 2107844, 2108179, 2109812, 2111058, 2111132, 2113476, 2114618, 2121698, 2124540, 2126611, 2129022, 2137795, 2137818, 2141421, 2144505, 2147269, 2147905, 2148742, 2153125, 2154676, 2156158, 2164625, 2167575, 2173655, 2177424, 2181408, 2184775, 2186135, 2186221, 2194957, 2195008, 2195590, 2201741, 2201989, 2202248, 2204507, 2205937, 2207065, 2209029, 2210834, 2213402, 2215699, 2219618, 2220243, 2220770, 2226995, 2232923, 2232924, 2234424, 2235756, 2236786, 2237441, 2237505, 2238247, 2240877, 2242786, 2244423, 2247252, 2247340, 2254747, 2255896, 2260920, 2263798, 2265764, 2266586, 2271458, 2271691, 2272351, 2272408, 2274046, 2274940, 2276536, 2279636, 2284017, 2284075, 2286975, 2289167, 2290817, 2293409, 2293933, 2302216, 2306500, 2307564, 2307822, 2311696, 2322670, 2324171, 2326640, 2328216, 2328342, 2334724, 2335262, 2340856, 2346521, 2350494, 2350872, 2351715, 2353479, 2354001, 2356192, 2358755, 2360614, 2364526, 2367578, 2368097, 2370873, 2371951, 2374471, 2375005, 2375459, 2375980, 2382598, 2386314, 2386328, 2387321, 2395428, 2398099, 2398745, 2402228, 2404205, 2409729, 2410668, 2411100, 2411722, 2414089, 2414777, 2417320, 2423281, 2424824, 2426867, 2427006, 2428642, 2432322, 2432441, 2432832, 2433439, 2433761, 2434479, 2436287, 2437365, 2439422, 2441417, 2444641, 2447062, 2449243, 2450712, 2454725, 2457064, 2461991, 2463754, 2465275, 2466779, 2468361, 2474528, 2480039, 2480094],
        [1514724],
        2004030.0,
    ),
    ([0, 8], [1, 1, 9], 1.0),
    ([1, 3, 7, 9], [2, 5, 8], 5.0),
    ([0, 8], [1, 1, 7], 1.0),
    ([0, 0], [1, 1, 1], 1.0),
    ([0, 0, 0, 3], [1, 4], 0.5),
    ([1, 3], [2], 2.0),
    ([1, 2], [3, 4], 2.5),
    ([0, 0], [0, 0], 0.0),
    ([], [1], 1.0),
    ([2], [], 2.0),
    ([0], [1], 0.5),
    ([1], [0], 0.5),
]
NUM_RANDOM_TESTS = 10_000
MAX_LENGTH = 1_000
MAX_INT = 10 ** 6
MIN_INT = -MAX_INT


def make_test_case(
    nums1: List[int],
    nums2: List[int],
    correct_answer: Answer,
) -> TestCase:
    "make a test case from inputs"
    return TestCase(
        Parameters(nums1=nums1, nums2=nums2),
        correct_answer=correct_answer,
    )


def generate_test_case() -> TestCase:
    "generate a random test case"
    nums1_length = randint(0, MAX_LENGTH)
    if nums1_length == 0:
        nums2_length = randint(1, MAX_LENGTH)
    else:
        nums2_length = randint(0, MAX_LENGTH)

    # this won't be exactly correct
    nums1 = [int(random() * (MAX_INT - MIN_INT) - MIN_INT) for _ in range(nums1_length)]
    nums2 = [int(random() * (MAX_INT - MIN_INT) - MIN_INT) for _ in range(nums2_length)]

    # # more correct
    # nums1 = [randint(MIN_INT, MAX_INT) for _ in range(nums1_length)]
    # nums2 = [randint(MIN_INT, MAX_INT) for _ in range(nums2_length)]

    nums1.sort()
    nums2.sort()
    correct_answer = Answer(median([*nums1, *nums2]))

    if randint(0, 1):
        return TestCase(
            Parameters(nums1=nums1, nums2=nums2), correct_answer=correct_answer
        )
    return TestCase(Parameters(nums1=nums2, nums2=nums1), correct_answer=correct_answer)


def test(function: Function) -> None:
    "performs tests on the function to simulate the LeetCode submission"
    passed_count = 0
    for case_index, test_case in enumerate(
        chain(
            starmap(make_test_case, TEST_CASES),
            (generate_test_case() for _ in range(NUM_RANDOM_TESTS)),
        )
    ):
        random_case = case_index + 1 > len(TEST_CASES)
        if not random_case:
            case_number = str(case_index + 1)
            print(f"test #{case_number}")
        else:
            # sys.exit("passed all explicit")
            case_number = "random"

        case = test_case.case
        correct_answer = test_case.correct_answer

        try:
            answer = function(*case)
            passed_count += 1
        except NotImplementedError:
            if not random_case:
                print("skipped")
            continue

        if not check(answer, correct_answer):
            print(f"failure for case #{case_number}:")
            print(f"case:\n{pformat(case)}")
            print(f"correct answer:\n{correct_answer}")
            print(f"answer:\n{answer}")
            # breakpoint()  # pylint: disable=forgotten-debug-statement
            sys.exit(1)

    print(f"passed {passed_count} out of {len(TEST_CASES) + NUM_RANDOM_TESTS}")


def check(answer: Answer, correct_answer: Answer) -> bool:
    "check if the answer is correct"
    return math.isclose(answer, correct_answer)


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("file", help="filename of python file to test")
    args = parser.parse_args()
    path = Path(args.file)  # type: ignore
    if not path.is_file():
        print(f"{path} is not a file")
        sys.exit(1)

    sys.path.append(str(path.parent))
    module = import_module(path.stem)
    test(module.Solution().findMedianSortedArrays)  # type: ignore
