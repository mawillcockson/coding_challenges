--#!/usr/bin/env psql --no-password test
/*
Table: Patients
+--------------+---------+
| Column Name  | Type    |
+--------------+---------+
| patient_id   | int     |
| patient_name | varchar |
| conditions   | varchar |
+--------------+---------+
patient_id is the primary key (column with unique values) for this table.
'conditions' contains 0 or more code separated by spaces. 
This table contains information of the patients in the hospital.


Write a solution to find the patient_id, patient_name, and conditions of the patients who have Type I Diabetes. Type I Diabetes always starts with DIAB1 prefix.

Return the result table in any order.

The result format is in the following example.


Example 1:
Patients table:
+------------+--------------+--------------+
| patient_id | patient_name | conditions   |
+------------+--------------+--------------+
| 1          | Daniel       | YFEV COUGH   |
| 2          | Alice        |              |
| 3          | Bob          | DIAB100 MYOP |
| 4          | George       | ACNE DIAB100 |
| 5          | Alain        | DIAB201      |
+------------+--------------+--------------+
Output: 
+------------+--------------+--------------+
| patient_id | patient_name | conditions   |
+------------+--------------+--------------+
| 3          | Bob          | DIAB100 MYOP |
| 4          | George       | ACNE DIAB100 | 
+------------+--------------+--------------+
Explanation: Bob and George both have a condition that starts with DIAB1.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Patients (
  patient_id int PRIMARY key,
  patient_name varchar(30) NOT NULL CHECK (patient_name <> ''),
  conditions varchar(100) NOT NULL
);

INSERT INTO
  Patients (patient_id, patient_name, conditions)
VALUES
  ('1', 'Daniel', 'YFEV COUGH'),
  ('2', 'Alice', ''),
  ('3', 'Bob', 'DIAB100 MYOP'),
  ('4', 'George', 'ACNE DIAB100'),
  ('5', 'Alain', 'DIAB201');

SELECT
  patient_id,
  patient_name,
  conditions
FROM
  Patients
WHERE
  (
    SELECT
      bool_or (starts_with (CONDITION, 'DIAB1'))
    FROM
      string_to_table (conditions, ' ') AS a1 (CONDITION)
  );

ROLLBACK TO "test";

ROLLBACK;
