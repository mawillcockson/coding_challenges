--#!/usr/bin/env psql --no-password test
/*
Table: Employee
+-------------+------+
| Column Name | Type |
+-------------+------+
| id          | int  |
| salary      | int  |
+-------------+------+
id is the primary key (column with unique values) for this table.
Each row of this table contains information about the salary of an employee.


Write a solution to find the second highest distinct salary from the Employee table. If there is no second highest salary, return null (return None in Pandas).

The result format is in the following example.


Example 1:
Input: 
Employee table:
+----+--------+
| id | salary |
+----+--------+
| 1  | 100    |
| 2  | 200    |
| 3  | 300    |
+----+--------+
Output: 
+---------------------+
| SecondHighestSalary |
+---------------------+
| 200                 |
+---------------------+

Example 2:
Input: 
Employee table:
+----+--------+
| id | salary |
+----+--------+
| 1  | 100    |
+----+--------+
Output: 
+---------------------+
| SecondHighestSalary |
+---------------------+
| null                |
+---------------------+
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Employee (
  "id" int PRIMARY key,
  salary int NOT NULL CHECK (salary >= 0)
);

INSERT INTO
  Employee ("id", salary)
VALUES
  ('1', '100'),
  ('2', '200'),
  ('3', '300');

DELETE FROM Employee
WHERE
  "id" <> 1;

WITH
  step1 AS (
    SELECT DISTINCT
      salary
    FROM
      Employee
    ORDER BY
      salary DESC
    LIMIT
      1
    OFFSET
      1
  )
SELECT
  coalesce(
    (
      SELECT
        salary
      FROM
        step1
    ),
    NULL
  ) AS "SecondHighestSalary";

ROLLBACK TO "test";

ROLLBACK;
