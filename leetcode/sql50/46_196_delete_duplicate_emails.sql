--#!/usr/bin/env psql --no-password test
/*
Table: Person
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| id          | int     |
| email       | varchar |
+-------------+---------+
id is the primary key (column with unique values) for this table.
Each row of this table contains an email. The emails will not contain uppercase letters.


Write a solution to delete all duplicate emails, keeping only one unique email with the smallest id.

For SQL users, please note that you are supposed to write a DELETE statement and not a SELECT one.

For Pandas users, please note that you are supposed to modify Person in place.

After running your script, the answer shown is the Person table. The driver will first compile and run your piece of code and then show the Person table. The final order of the Person table does not matter.

The result format is in the following example.


Example 1:
Input: 
Person table:
+----+------------------+
| id | email            |
+----+------------------+
| 1  | john@example.com |
| 2  | bob@example.com  |
| 3  | john@example.com |
+----+------------------+
Output: 
+----+------------------+
| id | email            |
+----+------------------+
| 1  | john@example.com |
| 2  | bob@example.com  |
+----+------------------+
Explanation: john@example.com is repeated two times. We keep the row with the smallest Id = 1.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Person (
  -- The schema on Leetcode lists the column name as `Id`, and the INSERT
  -- statements in the same schema use `id`. The runtime complains that `Id`
  -- is not a valid column name.
  "id" int PRIMARY key, -- I really don't think it's good to have a column name of just `id` in sql
  "email" varchar(255) NOT NULL CHECK (
    "email" <> ''
    AND NOT regexp_like ("email", '[[:upper:]]')
  )
);

INSERT INTO
  Person ("id", "email")
VALUES
  ('1', 'john@example.com'),
  ('2', 'bob@example.com'),
  ('3', 'john@example.com');

DELETE FROM Person AS p1
WHERE
  p1."id" IN (
    SELECT
      "id"
    FROM
      Person AS p2
    WHERE
      p2."email" = p1."email"
    ORDER BY
      "id" ASC
    OFFSET
      1
  );

SELECT
  *
FROM
  Person;

ROLLBACK TO "test";

ROLLBACK;
