--#!/usr/bin/env psql --no-password test
/*
Table: Weather
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| id            | int     |
| recordDate    | date    |
| temperature   | int     |
+---------------+---------+
id is the column with unique values for this table.
There are no different rows with the same recordDate.
This table contains information about the temperature on a certain day.


Write a solution to find all dates' id with higher temperatures compared to its previous dates (yesterday).

Return the result table in any order.

The result format is in the following example.


Example 1:

Input: 
Weather table:
+----+------------+-------------+
| id | recordDate | temperature |
+----+------------+-------------+
| 1  | 2015-01-01 | 10          |
| 2  | 2015-01-02 | 25          |
| 3  | 2015-01-03 | 20          |
| 4  | 2015-01-04 | 30          |
+----+------------+-------------+
Output: 
+----+
| id |
+----+
| 2  |
| 4  |
+----+
Explanation: 
In 2015-01-02, the temperature was higher than the previous day (10 -> 25).
In 2015-01-04, the temperature was higher than the previous day (20 -> 30).

*/
BEGIN;

SAVEPOINT "test";

WITH RECURSIVE
  nums (i) AS (
    VALUES
      (1)
    UNION ALL
    SELECT
      nums.i + 1
    FROM
      nums
  )
SELECT
  array_agg (i) OVER (
    ROWS BETWEEN 1 preceding
    AND CURRENT ROW
  )
FROM
  -- LIMIT in recursive query is not implemented in postgres, so I'm applying the
  -- limit in a subquery
  (
    SELECT
      i
    FROM
      nums
    LIMIT
      10
  );

CREATE TABLE Weather (
  id int PRIMARY key,
  recordDate date UNIQUE NOT NULL,
  temperature int NOT NULL
);

INSERT INTO
  Weather (id, recordDate, temperature)
VALUES
  ('1', '2015-01-01', '10'),
  ('2', '2015-01-02', '25'),
  ('3', '2015-01-03', '20'),
  ('4', '2015-01-04', '30') RETURNING *;

SELECT
  last_id AS id
FROM
  (
    SELECT
      first_value (id) OVER w AS first_id,
      last_value (id) OVER w AS last_id,
      first_value (temperature) OVER w AS first_temp,
      last_value (temperature) OVER w AS last_temp
    FROM
      Weather
    WINDOW
      w AS (
        ORDER BY
          recordDate RANGE BETWEEN '1 DAY' preceding
          AND CURRENT ROW
      )
  )
WHERE
  first_temp < last_temp;

-- using lag() from editorial
WITH
  test_weather (id, recordDate, temperature) AS (
    VALUES
      (1, cast('2000-12-14' AS date), 3),
      (2, cast('2000-12-16' AS date), 5)
  ),
  yesterday_and_today AS (
    SELECT
      id AS todays_id,
      recordDate AS todays_date,
      lag (recordDate, 1, NULL) OVER w AS yesterdays_date,
      temperature AS todays_temperature,
      lag (temperature, 1, NULL) OVER w AS yesterdays_temperature
    FROM
      Weather
    WINDOW
      w AS (
        ORDER BY
          recordDate
      )
  )
SELECT
  todays_id AS id
FROM
  yesterday_and_today
WHERE
  todays_date - yesterdays_date = 1
  AND yesterdays_temperature < todays_temperature;

ROLLBACK TO "test";

ROLLBACK;
