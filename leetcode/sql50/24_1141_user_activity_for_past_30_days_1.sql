--#!/usr/bin/env psql --no-password test
/*
Table: Activity
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| user_id       | int     |
| session_id    | int     |
| activity_date | date    |
| activity_type | enum    |
+---------------+---------+
This table may have duplicate rows.
The activity_type column is an ENUM (category) of type ('open_session', 'end_session', 'scroll_down', 'send_message').
The table shows the user activities for a social media website. 
Note that each session belongs to exactly one user.


Write a solution to find the daily active user count for a period of 30 days ending 2019-07-27 inclusively. A user was active on someday if they made at least one activity on that day.

Return the result table in any order.

The result format is in the following example.


Example 1:
Input: 
Activity table:
+---------+------------+---------------+---------------+
| user_id | session_id | activity_date | activity_type |
+---------+------------+---------------+---------------+
| 1       | 1          | 2019-07-20    | open_session  |
| 1       | 1          | 2019-07-20    | scroll_down   |
| 1       | 1          | 2019-07-20    | end_session   |
| 2       | 4          | 2019-07-20    | open_session  |
| 2       | 4          | 2019-07-21    | send_message  |
| 2       | 4          | 2019-07-21    | end_session   |
| 3       | 2          | 2019-07-21    | open_session  |
| 3       | 2          | 2019-07-21    | send_message  |
| 3       | 2          | 2019-07-21    | end_session   |
| 4       | 3          | 2019-06-25    | open_session  |
| 4       | 3          | 2019-06-25    | end_session   |
+---------+------------+---------------+---------------+
Output: 
+------------+--------------+ 
| day        | active_users |
+------------+--------------+ 
| 2019-07-20 | 2            |
| 2019-07-21 | 2            |
+------------+--------------+ 
Explanation: Note that we do not care about days with zero active users.
*/
BEGIN;

SAVEPOINT "test";

CREATE type ActivityType AS ENUM (
  'open_session',
  'end_session',
  'scroll_down',
  'send_message'
);

CREATE TABLE Activity (
  user_id int NOT NULL,
  session_id int NOT NULL,
  activity_date date NOT NULL,
  activity_type ActivityType
);

INSERT INTO
  Activity (user_id, session_id, activity_date, activity_type)
VALUES
  ('1', '1', '2019-07-20', 'open_session'),
  ('1', '1', '2019-07-20', 'scroll_down'),
  ('1', '1', '2019-07-20', 'end_session'),
  ('2', '4', '2019-07-20', 'open_session'),
  ('2', '4', '2019-07-21', 'send_message'),
  ('2', '4', '2019-07-21', 'end_session'),
  ('3', '2', '2019-07-21', 'open_session'),
  ('3', '2', '2019-07-21', 'send_message'),
  ('3', '2', '2019-07-21', 'end_session'),
  ('4', '3', '2019-06-25', 'open_session'),
  ('4', '3', '2019-06-25', 'end_session');

WITH RECURSIVE
  dates_infinite ("date") AS (
    VALUES
      (
        cast('2019-07-27' AS date) - cast('29 DAYS' AS interval)
      )
    UNION ALL
    SELECT
      "date" + cast('1 DAY' AS interval)
    FROM
      dates_infinite
  ),
  dates AS (
    SELECT
      cast("date" AS date) AS "date"
    FROM
      dates_infinite
    LIMIT
      30
  ),
  counts AS (
    SELECT
      dates."date" AS "date",
      count(DISTINCT Activity.user_id) AS active_users
    FROM
      dates
      LEFT OUTER JOIN Activity ON dates."date" = Activity.activity_date
    GROUP BY
      dates."date"
  )
SELECT
  "date" AS "day",
  active_users AS active_users
FROM
  counts
WHERE
  active_users > 0;

--(user_id, session_id, activity_date, activity_type)
ROLLBACK TO "test";

ROLLBACK;
