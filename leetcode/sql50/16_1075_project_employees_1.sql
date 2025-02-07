--#!/usr/bin/env psql --no-password test
/*
Table: Project
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| project_id  | int     |
| employee_id | int     |
+-------------+---------+
(project_id, employee_id) is the primary key of this table.
employee_id is a foreign key to Employee table.
Each row of this table indicates that the employee with employee_id is working on the project with project_id.

Table: Employee
+------------------+---------+
| Column Name      | Type    |
+------------------+---------+
| employee_id      | int     |
| name             | varchar |
| experience_years | int     |
+------------------+---------+
employee_id is the primary key of this table. It's guaranteed that experience_years is not NULL.
Each row of this table contains information about one employee.


Write an SQL query that reports the average experience years of all the employees for each project, rounded to 2 digits.

Return the result table in any order.

The query result format is in the following example.


Example 1:
Input: 
Project table:
+-------------+-------------+
| project_id  | employee_id |
+-------------+-------------+
| 1           | 1           |
| 1           | 2           |
| 1           | 3           |
| 2           | 1           |
| 2           | 4           |
+-------------+-------------+
Employee table:
+-------------+--------+------------------+
| employee_id | name   | experience_years |
+-------------+--------+------------------+
| 1           | Khaled | 3                |
| 2           | Ali    | 2                |
| 3           | John   | 1                |
| 4           | Doe    | 2                |
+-------------+--------+------------------+
Output: 
+-------------+---------------+
| project_id  | average_years |
+-------------+---------------+
| 1           | 2.00          |
| 2           | 2.50          |
+-------------+---------------+
Explanation: The average experience years for the first project is (3 + 2 + 1) / 3 = 2.00 and for the second project is (3 + 2) / 2 = 2.50
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Project (
  project_id int NOT NULL,
  employee_id int NOT NULL,
  CONSTRAINT project_pk PRIMARY key (project_id, employee_id)
);

CREATE TABLE Employee (
  employee_id int PRIMARY key,
  name varchar(10) NOT NULL,
  experience_years int NOT NULL
);

INSERT INTO
  Project (project_id, employee_id)
VALUES
  ('1', '1'),
  ('1', '2'),
  ('1', '3'),
  ('2', '1'),
  ('2', '4');

INSERT INTO
  Employee (employee_id, name, experience_years)
VALUES
  ('1', 'Khaled', '3'),
  ('2', 'Ali', '2'),
  ('3', 'John', '1'),
  ('4', 'Doe', '2');

ALTER TABLE Project add CONSTRAINT project_employee_id_fk FOREIGN key (employee_id) REFERENCES Employee (employee_id);

ALTER TABLE project validate CONSTRAINT project_employee_id_fk;

SELECT
  project_id,
  round(cast(avg(experience_years) AS numeric), 2) AS average_years
FROM
  Project
  INNER JOIN Employee ON Project.employee_id = Employee.employee_id
GROUP BY
  project_id;

ROLLBACK TO "test";

ROLLBACK;
