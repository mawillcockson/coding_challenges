--#!/usr/bin/env psql --no-password test
/*
Table: Seat
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| id          | int     |
| student     | varchar |
+-------------+---------+
id is the primary key (unique value) column for this table.
Each row of this table indicates the name and the ID of a student.
The ID sequence always starts from 1 and increments continuously.


Write a solution to swap the seat id of every two consecutive students. If the number of students is odd, the id of the last student is not swapped.

Return the result table ordered by id in ascending order.

The result format is in the following example.


Example 1:
Input: 
Seat table:
+----+---------+
| id | student |
+----+---------+
| 1  | Abbot   |
| 2  | Doris   |
| 3  | Emerson |
| 4  | Green   |
| 5  | Jeames  |
+----+---------+
Output: 
+----+---------+
| id | student |
+----+---------+
| 1  | Doris   |
| 2  | Abbot   |
| 3  | Green   |
| 4  | Emerson |
| 5  | Jeames  |
+----+---------+
Explanation: 
Note that if the number of students is odd, there is no need to change the last one's seat.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Seat (
  id int PRIMARY key generated BY DEFAULT AS IDENTITY (
    sequence name seat_id_seq increment BY 1 NO maxvalue START
    WITH
      1
  ),
  student varchar(255) NOT NULL CHECK (student <> '')
);

INSERT INTO
  Seat (id, student)
VALUES
  ('1', 'Abbot'),
  ('2', 'Doris'),
  ('3', 'Emerson'),
  ('4', 'Green'),
  ('5', 'Jeames');

SELECT
  id,
  CASE mod(id, 2)
    WHEN 1 THEN last_value (student) OVER w
    WHEN 0 THEN first_value (student) OVER w
  END AS student
FROM
  Seat
WINDOW
  w AS (
    PARTITION BY
      ceil(cast(id AS numeric) / 2)
    ORDER BY
      id ASC ROWS BETWEEN unbounded preceding
      AND unbounded following
  );

ROLLBACK TO "test";

ROLLBACK;
