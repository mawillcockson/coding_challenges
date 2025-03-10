--#!/usr/bin/env psql --no-password test
/*
Table: Users
+----------------+---------+
| Column Name    | Type    |
+----------------+---------+
| user_id        | int     |
| name           | varchar |
+----------------+---------+
user_id is the primary key (column with unique values) for this table.
This table contains the ID and the name of the user. The name consists of only lowercase and uppercase characters.


Write a solution to fix the names so that only the first character is uppercase and the rest are lowercase.

Return the result table ordered by user_id.

The result format is in the following example.


Example 1:
Input: 
Users table:
+---------+-------+
| user_id | name  |
+---------+-------+
| 1       | aLice |
| 2       | bOB   |
+---------+-------+
Output: 
+---------+-------+
| user_id | name  |
+---------+-------+
| 1       | Alice |
| 2       | Bob   |
+---------+-------+
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Users (
  user_id int PRIMARY key,
  name varchar(40) NOT NULL CHECK (regexp_like (name, '^[a-zA-Z]+$'))
);

INSERT INTO
  Users (user_id, name)
VALUES
  ('1', 'aLice'),
  ('2', 'bOB');

SELECT
  user_id,
  upper(regexp_replace (name, '^(\w).*', '\1')) || lower(regexp_replace (name, '^.(\w+)?', '\1')) AS name
FROM
  Users
ORDER BY
  user_id ASC;

ROLLBACK TO "test";

ROLLBACK;
