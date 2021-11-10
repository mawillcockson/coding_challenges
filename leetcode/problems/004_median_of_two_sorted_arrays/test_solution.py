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
    ([1000086, 1003855, 1006165, 1007706, 1008737, 1016423, 1017110, 1017130, 1019718, 1024621, 1029716, 1034078, 1034362, 1036808, 1038717, 1041835, 1044965, 1045286, 1047831, 1048121, 1048610, 1050424, 1050547, 1050556, 1053310, 1054116, 1056529, 1057730, 1060019, 1069011, 1070313, 1070609, 1072559, 1074057, 1074063, 1074936, 1077623, 1080561, 1094091, 1094161, 1097916, 1098008, 1099322, 1099653, 1105701, 1109446, 1110437, 1113812, 1114663, 1116498, 1121125, 1124176, 1125006, 1135130, 1137677, 1139295, 1140412, 1142049, 1143756, 1146492, 1147433, 1152432, 1152557, 1154487, 1159752, 1160055, 1165869, 1167080, 1170400, 1172554, 1173469, 1173538, 1178020, 1178513, 1179816, 1183035, 1183101, 1185030, 1190788, 1193028, 1198251, 1199007, 1200339, 1200589, 1203019, 1205824, 1211114, 1211227, 1213824, 1214361, 1214451, 1216619, 1217633, 1220360, 1224259, 1224878, 1227202, 1228807, 1231608, 1232305, 1232478, 1233425, 1235159, 1236459, 1236499, 1237180, 1237630, 1239736, 1240196, 1247746, 1248836, 1249508, 1249653, 1253642, 1261493, 1262654, 1264065, 1267442, 1268037, 1269948, 1270271, 1270373, 1273249, 1273465, 1278077, 1278586, 1281327, 1288307, 1288971, 1292977, 1295380, 1296806, 1296917, 1298899, 1300061, 1306879, 1307803, 1309525, 1310164, 1317279, 1318600, 1318678, 1319296, 1326416, 1328228, 1330018, 1336155, 1336742, 1342058, 1346037, 1348623, 1350616, 1352913, 1353087, 1353404, 1353505, 1358628, 1359879, 1368446, 1369332, 1370596, 1372059, 1373446, 1374256, 1378491, 1385303, 1386348, 1386505, 1386812, 1388706, 1388726, 1390234, 1392388, 1392525, 1394248, 1399225, 1408874, 1410716, 1412130, 1418829, 1419717, 1420732, 1422044, 1422788, 1422977, 1426882, 1427389, 1429074, 1429359, 1429648, 1433239, 1434337, 1435457, 1436341, 1437838, 1438113, 1441204, 1443284, 1444764, 1446102, 1453481, 1453852, 1453914, 1455515, 1457271, 1458771, 1459685, 1460163, 1460976, 1461107, 1464885, 1467144, 1469162, 1478706, 1481856, 1481863, 1482066, 1486782, 1489624, 1490034, 1492374, 1494488, 1495063, 1496843, 1498071, 1502043, 1502065, 1502691, 1503026, 1504116, 1509069, 1510826, 1511940, 1513770, 1515273, 1517959, 1518453, 1518664, 1521141, 1523048, 1523213, 1524651, 1526571, 1528569, 1530370, 1530722, 1536648, 1537547, 1538983, 1542257, 1543572, 1548472, 1553784, 1553878, 1555750, 1557866, 1561980, 1562217, 1563582, 1572597, 1572616, 1578402, 1580724, 1581898, 1582502, 1583005, 1586738, 1590144, 1594880, 1602029, 1602142, 1605165, 1605219, 1606260, 1606963, 1614580, 1616583, 1619348, 1622664, 1631868, 1635508, 1636520, 1637918, 1641661, 1642894, 1645124, 1645300, 1653019, 1657687, 1658274, 1658716, 1661932, 1663630, 1663979, 1667014, 1672941, 1678407, 1678546, 1679162, 1679439, 1685080, 1686475, 1690737, 1692764, 1693225, 1693424, 1696045, 1696863, 1697127, 1697247, 1698928, 1699673, 1699801, 1701446, 1704966, 1705377, 1712514, 1713395, 1720869, 1721642, 1727337, 1729943, 1731463, 1737225, 1737788, 1738668, 1738827, 1743257, 1745798, 1755083, 1756026, 1757176, 1758095, 1758761, 1760150, 1761173, 1763165, 1767922, 1768514, 1768666, 1772707, 1774409, 1774683, 1775980, 1783663, 1783673, 1783782, 1787539, 1791118, 1791834, 1794951, 1795258, 1796030, 1796052, 1798775, 1800526, 1801051, 1803779, 1804873, 1807380, 1809885, 1810286, 1811020, 1813971, 1815507, 1817261, 1819150, 1820261, 1822971, 1823584, 1823980, 1824712, 1829176, 1831264, 1833761, 1834845, 1836563, 1836594, 1837540, 1840939, 1842248, 1846190, 1846809, 1852322, 1855423, 1856996, 1860678, 1864589, 1867323, 1867906, 1869439, 1871612, 1878280, 1878926, 1879056, 1881198, 1886859, 1888341, 1891772, 1892292, 1895949, 1896672, 1901700, 1904197, 1904693, 1908923, 1910927, 1911207, 1911257, 1915866, 1916289, 1917944, 1919467, 1923967, 1934149, 1936011, 1937435, 1939304, 1948113, 1953690, 1954070, 1958606, 1959191, 1963030, 1966117, 1969037, 1970923, 1976700, 1977938, 1978165, 1980818, 1980862, 1981082, 1982040, 1982126, 1984027, 1984446, 1985745, 1989831, 1994926, 1995015, 1997270, 2003311, 2004749, 2005010, 2006243, 2008935, 2010899, 2011405, 2012218, 2012835, 2014113, 2015749, 2018324, 2019498, 2019626, 2020229, 2020948, 2021108, 2021754, 2023151, 2029473, 2037128, 2037261, 2039117, 2045852, 2047744, 2050945, 2052316, 2055321, 2057904, 2059147, 2059887, 2060768, 2063473, 2069359, 2075118, 2076962, 2077856, 2079195, 2083139, 2084000, 2085203, 2085657, 2086408, 2089556, 2092826, 2094631, 2094948, 2095710, 2096149, 2096509, 2103550, 2104484, 2106300, 2107175, 2107320, 2107587, 2107844, 2108179, 2109812, 2111058, 2111132, 2113476, 2114618, 2121698, 2124540, 2126611, 2129022, 2137795, 2137818, 2141421, 2144505, 2147269, 2147905, 2148742, 2153125, 2154676, 2156158, 2164625, 2167575, 2173655, 2177424, 2181408, 2184775, 2186135, 2186221, 2194957, 2195008, 2195590, 2201741, 2201989, 2202248, 2204507, 2205937, 2207065, 2209029, 2210834, 2213402, 2215699, 2219618, 2220243, 2220770, 2226995, 2232923, 2232924, 2234424, 2235756, 2236786, 2237441, 2237505, 2238247, 2240877, 2242786, 2244423, 2247252, 2247340, 2254747, 2255896, 2260920, 2263798, 2265764, 2266586, 2271458, 2271691, 2272351, 2272408, 2274046, 2274940, 2276536, 2279636, 2284017, 2284075, 2286975, 2289167, 2290817, 2293409, 2293933, 2302216, 2306500, 2307564, 2307822, 2311696, 2322670, 2324171, 2326640, 2328216, 2328342, 2334724, 2335262, 2340856, 2346521, 2350494, 2350872, 2351715, 2353479, 2354001, 2356192, 2358755, 2360614, 2364526, 2367578, 2368097, 2370873, 2371951, 2374471, 2375005, 2375459, 2375980, 2382598, 2386314, 2386328, 2387321, 2395428, 2398099, 2398745, 2402228, 2404205, 2409729, 2410668, 2411100, 2411722, 2414089, 2414777, 2417320, 2423281, 2424824, 2426867, 2427006, 2428642, 2432322, 2432441, 2432832, 2433439, 2433761, 2434479, 2436287, 2437365, 2439422, 2441417, 2444641, 2447062, 2449243, 2450712, 2454725, 2457064, 2461991, 2463754, 2465275, 2466779, 2468361, 2474528, 2480039, 2480094, 2485276, 2485750, 2487243, 2487781, 2493930, 2497925, 2501176, 2503244, 2503841, 2504940, 2506805, 2506924, 2508545, 2509838, 2509846, 2510771, 2513538, 2516016, 2517100, 2517990, 2519323, 2529130, 2529768, 2532512, 2534472, 2535617, 2536376, 2537245, 2541076, 2541622, 2541796, 2542788, 2544473, 2544906, 2546302, 2549825, 2552040, 2553133, 2555388, 2561538, 2566275, 2567670, 2567805, 2570786, 2572220, 2576602, 2577871, 2579602, 2581031, 2585806, 2589174, 2592754, 2592899, 2595384, 2598150, 2602229, 2603282, 2605258, 2606437, 2608595, 2609387, 2609670, 2610399, 2618689, 2622572, 2622708, 2624081, 2625444, 2625484, 2625824, 2627101, 2630237, 2632570, 2644183, 2644436, 2646188, 2646798, 2648847, 2651814, 2653136, 2663394, 2663461, 2664424, 2664603, 2664868, 2664956, 2665633, 2669307, 2669408, 2671069, 2672063, 2675022, 2675829, 2676224, 2677652, 2678634, 2680269, 2680750, 2681543, 2686457, 2686956, 2688167, 2691680, 2692420, 2692668, 2697614, 2698954, 2698962, 2699735, 2702936, 2705810, 2709169, 2718849, 2720409, 2725154, 2728840, 2729296, 2730538, 2732995, 2736651, 2737256, 2738099, 2742951, 2743171, 2745127, 2748057, 2751884, 2751948, 2754483, 2754873, 2758848, 2762984, 2770014, 2770758, 2771567, 2776087, 2776206, 2777841, 2779482, 2781147, 2785482, 2786348, 2786593, 2788580, 2790063, 2797156, 2798813, 2799303, 2800654, 2800676, 2800873, 2803887, 2804151, 2806996, 2807908, 2814161, 2814411, 2818397, 2819120, 2819988, 2827676, 2831384, 2832137, 2835967, 2841103, 2849007, 2849325, 2851989, 2852037, 2852849, 2857479, 2858529, 2859359, 2866615, 2869681, 2872704, 2873877, 2874684, 2875407, 2875419, 2875438, 2875486, 2876418, 2877796, 2879235, 2891089, 2893810, 2895472, 2897419, 2898103, 2899446, 2901156, 2901852, 2906719, 2910159, 2913922, 2914070, 2915843, 2917100, 2917278, 2919005, 2919668, 2921275, 2923012, 2936468, 2937352, 2937453, 2937709, 2937857, 2938114, 2943079, 2945453, 2948060, 2948413, 2956914, 2957093, 2962377, 2964625, 2969527, 2971576, 2971966, 2976516, 2984132, 2984729, 2984866, 2990447, 2991745, 2991912, 2991961, 2993166, 2996459, 2997019, 2998020], [1514724], 2004030.0),
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
