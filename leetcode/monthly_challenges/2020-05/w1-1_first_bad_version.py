# mypy: ignore-missing-imports, follow-imports=silent, warn-unreachable, warn-unused-configs, disallow-any-generics
# mypy: disallow-subclassing-any, disallow-untyped-calls, disallow-untyped-defs, disallow-incomplete-defs
# mypy: check-untyped-defs, disallow-untyped-decorators, no-implicit-optional, warn-redundant-casts
# mypy: warn-unused-ignores, warn-return-any, no-implicit-reexport, strict-equality
from math import floor
from functools import lru_cache

#n: int = 2
#p = len(str(n)) # padding size
#checks = 0
#
#def isBadVersion(version: int) -> bool:
#    return version >= 2
#
#@lru_cache(maxsize=None)
#def is_bad(v: int) -> bool:
#    global checks
#    checks += 1
#    return isBadVersion(v)
#
#def mid(start: int, end: int) -> int:
#    return floor((end - start) / 2) + start
#
#def firstBadVersion(n: int) -> int:
#    def search(start: int, end: int) -> int:
#        mid_n = mid(start, end)
#        print(f"{start: ={p}} >- {mid_n: ={p}} -< {end: ={p}}")
#
#        at_min_step = abs(end - mid_n) <= 1
#        
#        # Assumes a bad version exists
#        if at_min_step and is_bad(mid_n):
#            return mid_n
#        elif at_min_step:
#            return end
#        elif is_bad(mid_n):
#            return search(start, mid_n)
#        else:
#            return search(mid_n, end)
#
#        # NOTE: Less optimal, as it performs an extra check to make sure the last
#        # version is bad, instead of assuming it is
#        #
#        # if at_min_step and not is_bad(mid_n) and not is_bad(n):
#        #     raise ValueError("No bad version!")
#        # elif at_min_step and is_bad(mid_n):
#        #     return mid_n
#        # elif at_min_step:
#        #     return n
#        # elif is_bad(mid_n):
#        #     return search(start, mid_n)
#        # else:
#        #     return search(mid_n, end)
#
#    return search(1, n)
#
#print(f"""{firstBadVersion(n)}
#checks: {checks}""")

# Does not work, see screenshot; don't know why
## The isBadVersion API is already defined for you.
## @param version, an integer
## @return a bool
## def isBadVersion(version):
#from math import floor
#from functools import lru_cache
#
#class Solution(object):
#    @staticmethod
#    @lru_cache(maxsize=None)
#    def is_bad(v: int) -> bool:
#        print(f"{v} is {'bad' if (ret := isBadVersion(v)) else 'ok'}")
#        return isBadVersion(v)
#
#    @staticmethod
#    def mid(start: int, end: int) -> int:
#        return floor((end - start) / 2) + start
#
#    def firstBadVersion(self, n: int) -> int:
#        """
#        :type n: int
#        :rtype: int
#        """
#        
#        print(f"n: {n}")
#
#        def search(start: int, end: int) -> int:
#            mid_n = self.mid(start, end)
#            at_min_step = abs(end - mid_n) <= 1
#            
#            p = len(str(n)) # padding width
#            print(f"{start: ={p}} >- {mid_n: ={p}} -< {end: ={p}}")
#
#
#            if at_min_step and self.is_bad(mid_n):
#                print(f"mid_n is {'' if isBadVersion(mid_n) else 'not'} the bad one, so returning it")
#                return mid_n
#            elif at_min_step:
#                print("mid_n isn't bad, so it's gotta be end")
#                return end
#            elif self.is_bad(mid_n):
#                print("Keep diggin lower")
#                return search(start, mid_n)
#            else:
#                print("Keep diggin higher")
#                return search(mid_n, end)
#
#        return search(1, n)


# Submitted:
# The isBadVersion API is already defined for you.
# @param version, an integer
# @return a bool
# def isBadVersion(version):
from math import floor

def isBadVersion(version: int) -> bool: ...

class Solution:
    def __init__(self, n: int = 1) -> None:
        global isBadVersion
        isBadVersion = lambda x: x >= n

    @staticmethod
    def mid(start: int, end: int) -> int:
        return floor((end - start) / 2) + start

    def firstBadVersion(self, n: int) -> int:
        """
        :type n: int
        :rtype: int
        """

        def search(start: int, end: int) -> int:
            mid_n = self.mid(start, end)
            at_min_step = abs(end - mid_n) <= 1
            mid_is_bad = isBadVersion(mid_n)
            
            #p = len(str(n)) # padding width
            #print(f"{start: ={p}} >- {mid_n: ={p}} -< {end: ={p}}")

            if at_min_step and mid_is_bad and isBadVersion(start):
                return start
            elif at_min_step and mid_is_bad:
                # NOTE: mypy didn't catch this???
                #return mid_is_bad
                return mid_n
            elif at_min_step:
                return end
            elif mid_is_bad:
                return search(start, mid_n)
            else:
                return search(mid_n, end)

        return search(1, n)

print(f"{Solution(4).firstBadVersion(5)}")
