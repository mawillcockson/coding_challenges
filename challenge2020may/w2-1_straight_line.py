r"""
You are given an array coordinates, coordinates[i] = [x, y], where [x, y] represents the coordinate of a point. Check if these points make a straight line in the XY plane.

Input: coordinates = [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]]
Output: true

Example 2:

Input: coordinates = [[1,1],[2,2],[3,4],[4,5],[5,6],[7,7]]
Output: false

 
Constraints:

* 2 <= coordinates.length <= 1000
* coordinates[i].length == 2
* -10^4 <= coordinates[i][0], coordinates[i][1] <= 10^4
* coordinates contains no duplicate point.
"""
from typing import List, NamedTuple, Union, Tuple, Iterable


class Point(NamedTuple):
    x: int
    y: int


AnyPoint = Union[Point, Tuple[int, int]]


class Line:
    def __init__(self, a: AnyPoint, b: AnyPoint) -> None:
        self.a = Point(*a)
        self.b = Point(*b)
        self.slope = (self.a.y - self.b.y) / (self.a.x - self.b.x)
        # y = m*x + b  ->  b = y - m*x
        self.y_intercept = self.a.y - self.slope * self.a.x

    def __contains__(self, point: AnyPoint) -> bool:
        # Does Y == m*X + b?
        point_ = Point(*point)
        return point_.y == self.slope * point_.x + self.y_intercept

    def __repr__(self) -> str:
        return f"Line(a=({self.a.x}, {self.a.y}), b=({self.b.x}, {self.b.y}))"


class Solution:
    def points(self, coordinates: List[List[int]]) -> Iterable[Point]:
        for (x, y) in coordinates:
            yield Point(x=x, y=y)

    def checkStraightLine(self, coordinates: List[List[int]]) -> bool:
        if len(coordinates) < 2:
            return False
        elif len(coordinates) == 2:
            return True
        else:
            line = Line(a=Point(*coordinates[0]), b=Point(*coordinates[1]))

        for point in self.points(coordinates):
            if point not in line:
                return False

        return True
