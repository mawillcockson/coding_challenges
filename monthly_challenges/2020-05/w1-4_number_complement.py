class Solution:
    def findComplement(self, num: int) -> int:
        return int(f'{num:b}'.replace('0', 'a').replace('1', '0').replace('a', '1'), base=2)
        # return num ^ (2**num.bit_length() - 1)
        # return num ^ ((1<<num.bit_length()) - 1)

c = Solution().findComplement
print(f'''
{c(5)}
{c(1)}
''')
