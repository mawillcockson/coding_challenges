class Solution:
    def numJewelsInStones(self, J: str, S: str) -> int:
        return sum(s.count(j) for j in J for s in S)

# From other's; NOTE: works because True is treated as 1 in a "numerical context"
class Solution:
    def numJewelsInStones(self, J: str, S: str) -> int:
        return sum(s in J for s in S)
