--#!/usr/bin/env psql --no-password test
/*
Table: Employee
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| id          | int     |
| name        | varchar |
| department  | varchar |
| managerId   | int     |
+-------------+---------+
id is the primary key (column with unique values) for this table.
Each row of this table indicates the name of an employee, their department, and the id of their manager.
If managerId is null, then the employee does not have a manager.
No employee will be the manager of themself.


Write a solution to find managers with at least five direct reports.

Return the result table in any order.

The result format is in the following example.


Example 1:

Input: 
Employee table:
+-----+-------+------------+-----------+
| id  | name  | department | managerId |
+-----+-------+------------+-----------+
| 101 | John  | A          | null      |
| 102 | Dan   | A          | 101       |
| 103 | James | A          | 101       |
| 104 | Amy   | A          | 101       |
| 105 | Anne  | A          | 101       |
| 106 | Ron   | B          | 101       |
+-----+-------+------------+-----------+
Output: 
+------+
| name |
+------+
| John |
+------+
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Employee (
  id int PRIMARY key,
  name varchar(255) NOT NULL CHECK (name <> ''),
  department varchar(255) NOT NULL CHECK (name <> ''),
  managerId int REFERENCES Employee (id)
);

INSERT INTO
  Employee (id, name, department, managerId)
VALUES
  ('101', 'John', 'A', NULL),
  ('102', 'Dan', 'A', '101'),
  ('103', 'James', 'A', '101'),
  ('104', 'Amy', 'A', '101'),
  ('105', 'Anne', 'A', '101'),
  ('106', 'Ron', 'B', '101');

WITH
  counts AS (
    SELECT
      Employee.managerId AS id,
      count(Employee.managerId) AS reports
    FROM
      Employee
    WHERE
      Employee.managerId IS NOT NULL
    GROUP BY
      Employee.managerId
  )
SELECT
  Employee.name AS name
FROM
  counts
  INNER JOIN Employee ON counts.id = Employee.id
WHERE
  counts.reports >= 5;

ROLLBACK TO "test";

ROLLBACK;
