--#!/usr/bin/env psql --no-password test
/*
Table: Employee
+--------------+---------+
| Column Name  | Type    |
+--------------+---------+
| id           | int     |
| name         | varchar |
| salary       | int     |
| departmentId | int     |
+--------------+---------+
id is the primary key (column with unique values) for this table.
departmentId is a foreign key (reference column) of the ID from the Department table.
Each row of this table indicates the ID, name, and salary of an employee. It also contains the ID of their department.

Table: Department
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| id          | int     |
| name        | varchar |
+-------------+---------+
id is the primary key (column with unique values) for this table.
Each row of this table indicates the ID of a department and its name.


A company's executives are interested in seeing who earns the most money in each of the company's departments. A high earner in a department is an employee who has a salary in the top three unique salaries for that department.

Write a solution to find the employees who are high earners in each of the departments.

Return the result table in any order.

The result format is in the following example.


Example 1:
Input: 
Employee table:
+----+-------+--------+--------------+
| id | name  | salary | departmentId |
+----+-------+--------+--------------+
| 1  | Joe   | 85000  | 1            |
| 2  | Henry | 80000  | 2            |
| 3  | Sam   | 60000  | 2            |
| 4  | Max   | 90000  | 1            |
| 5  | Janet | 69000  | 1            |
| 6  | Randy | 85000  | 1            |
| 7  | Will  | 70000  | 1            |
+----+-------+--------+--------------+
Department table:
+----+-------+
| id | name  |
+----+-------+
| 1  | IT    |
| 2  | Sales |
+----+-------+
Output: 
+------------+----------+--------+
| Department | Employee | Salary |
+------------+----------+--------+
| IT         | Max      | 90000  |
| IT         | Joe      | 85000  |
| IT         | Randy    | 85000  |
| IT         | Will     | 70000  |
| Sales      | Henry    | 80000  |
| Sales      | Sam      | 60000  |
+------------+----------+--------+
Explanation: 
In the IT department:
- Max earns the highest unique salary
- Both Randy and Joe earn the second-highest unique salary
- Will earns the third-highest unique salary

In the Sales department:
- Henry earns the highest salary
- Sam earns the second-highest salary
- There is no third-highest salary as there are only two employees

Constraints:

There are no employees with the exact same name, salary and department.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Employee (
  id int PRIMARY key,
  name varchar(255) NOT NULL CHECK (name <> ''),
  salary int NOT NULL CHECK (salary >= 0),
  departmentId int NOT NULL,
  CONSTRAINT "why didn't they put this in the schema?" UNIQUE (id, name, salary)
);

CREATE TABLE Department (
  id int PRIMARY KEY,
  name varchar(255) NOT NULL CHECK (name <> '')
);

INSERT INTO
  Employee (id, name, salary, departmentId)
VALUES
  ('1', 'Joe', '85000', '1'),
  ('2', 'Henry', '80000', '2'),
  ('3', 'Sam', '60000', '2'),
  ('4', 'Max', '90000', '1'),
  ('5', 'Janet', '69000', '1'),
  ('6', 'Randy', '85000', '1'),
  ('7', 'Will', '70000', '1');

INSERT INTO
  Department (id, name)
VALUES
  ('1', 'IT'),
  ('2', 'Sales');

ALTER TABLE Employee add CONSTRAINT employee_departmentId_fk FOREIGN key (departmentId) REFERENCES Department (id);

ALTER TABLE Employee validate CONSTRAINT employee_departmentId_fk;

SELECT
  Department.name AS "Department",
  Employee.name AS "Employee",
  Employee.salary AS "Salary"
FROM
  Employee
  INNER JOIN Department ON Employee.departmentId = Department.id
WHERE
  Employee.salary IN (
    SELECT DISTINCT
      e2.salary
    FROM
      Employee AS e2
    WHERE
      e2.departmentId = Employee.departmentId
    ORDER BY
      e2.salary DESC
    LIMIT
      3
  );

ROLLBACK TO "test";

ROLLBACK;
