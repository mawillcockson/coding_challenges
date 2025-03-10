--#!/usr/bin/env psql --no-password test
/*
Table: Employees
+-------------+----------+
| Column Name | Type     |
+-------------+----------+
| employee_id | int      |
| name        | varchar  |
| manager_id  | int      |
| salary      | int      |
+-------------+----------+
In SQL, employee_id is the primary key for this table.
This table contains information about the employees, their salary, and the ID of their manager. Some employees do not have a manager (manager_id is null). 


Find the IDs of the employees whose salary is strictly less than $30000 and whose manager left the company. When a manager leaves the company, their information is deleted from the Employees table, but the reports still have their manager_id set to the manager that left.

Return the result table ordered by employee_id.

The result format is in the following example.


Example 1:
Input:  
Employees table:
+-------------+-----------+------------+--------+
| employee_id | name      | manager_id | salary |
+-------------+-----------+------------+--------+
| 3           | Mila      | 9          | 60301  |
| 12          | Antonella | null       | 31000  |
| 13          | Emery     | null       | 67084  |
| 1           | Kalel     | 11         | 21241  |
| 9           | Mikaela   | null       | 50937  |
| 11          | Joziah    | 6          | 28485  |
+-------------+-----------+------------+--------+
Output: 
+-------------+
| employee_id |
+-------------+
| 11          |
+-------------+
Explanation: 
The employees with a salary less than $30000 are 1 (Kalel) and 11 (Joziah).
Kalel's manager is employee 11, who is still in the company (Joziah).
Joziah's manager is employee 6, who left the company because there is no row for employee 6 as it was deleted.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Employees (
  employee_id int PRIMARY key,
  name varchar(20) NOT NULL CHECK (name <> ''),
  manager_id int CHECK (manager_id >= 0),
  salary int NOT NULL CHECK (salary >= 0)
);

INSERT INTO
  Employees (employee_id, name, manager_id, salary)
VALUES
  ('3', 'Mila', '9', '60301'),
  ('12', 'Antonella', NULL, '31000'),
  ('13', 'Emery', NULL, '67084'),
  ('1', 'Kalel', '11', '21241'),
  ('9', 'Mikaela', NULL, '50937'),
  ('11', 'Joziah', '6', '28485');

SELECT
  employee_id
FROM
  Employees
WHERE
  salary < 30000
  AND manager_id IS NOT NULL
  AND manager_id NOT IN (
    SELECT DISTINCT
      employee_id
    FROM
      Employees
  )
ORDER BY
  employee_id;

ROLLBACK TO "test";

ROLLBACK;
