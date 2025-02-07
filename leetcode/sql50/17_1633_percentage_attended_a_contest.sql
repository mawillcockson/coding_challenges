--#!/usr/bin/env psql --no-password test
/*
Table: Users
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| user_id     | int     |
| user_name   | varchar |
+-------------+---------+
user_id is the primary key (column with unique values) for this table.
Each row of this table contains the name and the id of a user.

Table: Register
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| contest_id  | int     |
| user_id     | int     |
+-------------+---------+
(contest_id, user_id) is the primary key (combination of columns with unique values) for this table.
Each row of this table contains the id of a user and the contest they registered into.


Write a solution to find the percentage of the users registered in each contest rounded to two decimals.

Return the result table ordered by percentage in descending order. In case of a tie, order it by contest_id in ascending order.

The result format is in the following example.


Example 1:
Input: 
Users table:
+---------+-----------+
| user_id | user_name |
+---------+-----------+
| 6       | Alice     |
| 2       | Bob       |
| 7       | Alex      |
+---------+-----------+
Register table:
+------------+---------+
| contest_id | user_id |
+------------+---------+
| 215        | 6       |
| 209        | 2       |
| 208        | 2       |
| 210        | 6       |
| 208        | 6       |
| 209        | 7       |
| 209        | 6       |
| 215        | 7       |
| 208        | 7       |
| 210        | 2       |
| 207        | 2       |
| 210        | 7       |
+------------+---------+
Output: 
+------------+------------+
| contest_id | percentage |
+------------+------------+
| 208        | 100.0      |
| 209        | 100.0      |
| 210        | 100.0      |
| 215        | 66.67      |
| 207        | 33.33      |
+------------+------------+
Explanation: 
All the users registered in contests 208, 209, and 210. The percentage is 100% and we sort them in the answer table by contest_id in ascending order.
Alice and Alex registered in contest 215 and the percentage is ((2/3) * 100) = 66.67%
Bob registered in contest 207 and the percentage is ((1/3) * 100) = 33.33%
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Users (
  user_id int PRIMARY key,
  user_name varchar(20) NOT NULL CHECK (user_name <> '')
);

CREATE TABLE Register (
  contest_id int,
  user_id int NOT NULL REFERENCES Users (user_id),
  CONSTRAINT register_pk PRIMARY key (contest_id, user_id)
);

INSERT INTO
  Users (user_id, user_name)
VALUES
  ('6', 'Alice'),
  ('2', 'Bob'),
  ('7', 'Alex');

INSERT INTO
  Register (contest_id, user_id)
VALUES
  ('215', '6'),
  ('209', '2'),
  ('208', '2'),
  ('210', '6'),
  ('208', '6'),
  ('209', '7'),
  ('209', '6'),
  ('215', '7'),
  ('208', '7'),
  ('210', '2'),
  ('207', '2'),
  ('210', '7');

WITH
  counts AS (
    SELECT
      contest_id,
      count(Register.user_id) AS registered
    FROM
      Users
      INNER JOIN Register ON Users.user_id = Register.user_id
    GROUP BY
      Register.contest_id
  )
SELECT
  contest_id,
  round(
    cast(
      cast(registered AS float) / (
        SELECT
          count(user_id)
        FROM
          Users
      ) * 100 AS numeric
    ),
    2
  ) AS percentage
FROM
  counts
ORDER BY
  percentage DESC,
  contest_id ASC;

ROLLBACK TO "test";

ROLLBACK;
