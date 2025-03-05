--#!/usr/bin/env psql --no-password test
/*
Table: Triangle
+-------------+------+
| Column Name | Type |
+-------------+------+
| x           | int  |
| y           | int  |
| z           | int  |
+-------------+------+
In SQL, (x, y, z) is the primary key column for this table.
Each row of this table contains the lengths of three line segments.


Report for every three line segments whether they can form a triangle.

Return the result table in any order.

The result format is in the following example.


Example 1:
Input: 
Triangle table:
+----+----+----+
| x  | y  | z  |
+----+----+----+
| 13 | 15 | 30 |
| 10 | 20 | 15 |
+----+----+----+
Output: 
+----+----+----+----------+
| x  | y  | z  | triangle |
+----+----+----+----------+
| 13 | 15 | 30 | No       |
| 10 | 20 | 15 | Yes      |
+----+----+----+----------+
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Triangle (
  x int NOT NULL CHECK (x >= 0),
  y int NOT NULL CHECK (y >= 0),
  z int NOT NULL CHECK (z >= 0),
  CONSTRAINT triangle_pk PRIMARY key (x, y, z)
);

INSERT INTO
  Triangle (x, y, z)
VALUES
  ('13', '15', '30'),
  ('10', '20', '15');

SELECT
  x,
  y,
  z,
  CASE
    WHEN greatest (x, y, z) < ((x + y + z) - greatest (x, y, z)) THEN 'Yes'
    ELSE 'No'
  END AS triangle
FROM
  Triangle;

ROLLBACK TO "test";

ROLLBACK;
