--#!/usr/bin/env psql --no-password test
/*
Table: Employee
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| empId       | int     |
| name        | varchar |
| supervisor  | int     |
| salary      | int     |
+-------------+---------+
empId is the column with unique values for this table.
Each row of this table indicates the name and the ID of an employee in addition to their salary and the id of their manager.

Table: Bonus
+-------------+------+
| Column Name | Type |
+-------------+------+
| empId       | int  |
| bonus       | int  |
+-------------+------+
empId is the column of unique values for this table.
empId is a foreign key (reference column) to empId from the Employee table.
Each row of this table contains the id of an employee and their respective bonus.


Write a solution to report the name and bonus amount of each employee with a bonus less than 1000.

Return the result table in any order.

The result format is in the following example.


Example 1:

Input: 
Employee table:
+-------+--------+------------+--------+
| empId | name   | supervisor | salary |
+-------+--------+------------+--------+
| 3     | Brad   | null       | 4000   |
| 1     | John   | 3          | 1000   |
| 2     | Dan    | 3          | 2000   |
| 4     | Thomas | 3          | 4000   |
+-------+--------+------------+--------+
Bonus table:
+-------+-------+
| empId | bonus |
+-------+-------+
| 2     | 500   |
| 4     | 2000  |
+-------+-------+
Output: 
+------+-------+
| name | bonus |
+------+-------+
| Brad | null  |
| John | null  |
| Dan  | 500   |
+------+-------+
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Employee (
  empId int PRIMARY key,
  name varchar(255) NOT NULL,
  supervisor int,
  salary int NOT NULL
);

CREATE TABLE Bonus (
  empId int PRIMARY key REFERENCES Employee (empId),
  bonus int NOT NULL CHECK (bonus > 0)
);

INSERT INTO
  Employee (empId, name, supervisor, salary)
VALUES
  ('3', 'Brad', NULL, '4000'),
  ('1', 'John', '3', '1000'),
  ('2', 'Dan', '3', '2000'),
  ('4', 'Thomas', '3', '4000');

INSERT INTO
  Bonus (empId, bonus)
VALUES
  ('2', '500'),
  ('4', '2000');

SELECT
  name,
  Bonus.bonus
FROM
  Employee
  LEFT OUTER JOIN Bonus ON Employee.empId = Bonus.empId
WHERE
  Bonus.bonus < 1000
  OR Bonus.bonus IS NULL;

ROLLBACK TO "test";

ROLLBACK;
