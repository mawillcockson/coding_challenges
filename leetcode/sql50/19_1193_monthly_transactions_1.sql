--#!/usr/bin/env psql --no-password test
/*
Table: Transactions
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| id            | int     |
| country       | varchar |
| state         | enum    |
| amount        | int     |
| trans_date    | date    |
+---------------+---------+
id is the primary key of this table.
The table has information about incoming transactions.
The state column is an enum of type ["approved", "declined"].


Write an SQL query to find for each month and country, the number of transactions and their total amount, the number of approved transactions and their total amount.

Return the result table in any order.

The query result format is in the following example.


Example 1:
Input: 
Transactions table:
+------+---------+----------+--------+------------+
| id   | country | state    | amount | trans_date |
+------+---------+----------+--------+------------+
| 121  | US      | approved | 1000   | 2018-12-18 |
| 122  | US      | declined | 2000   | 2018-12-19 |
| 123  | US      | approved | 2000   | 2019-01-01 |
| 124  | DE      | approved | 2000   | 2019-01-07 |
+------+---------+----------+--------+------------+
Output: 
+----------+---------+-------------+----------------+--------------------+-----------------------+
| month    | country | trans_count | approved_count | trans_total_amount | approved_total_amount |
+----------+---------+-------------+----------------+--------------------+-----------------------+
| 2018-12  | US      | 2           | 1              | 3000               | 1000                  |
| 2019-01  | US      | 1           | 1              | 2000               | 2000                  |
| 2019-01  | DE      | 1           | 1              | 2000               | 2000                  |
+----------+---------+-------------+----------------+--------------------+-----------------------+
*/
BEGIN;

SAVEPOINT "test";

CREATE type StateType AS enum ('approved', 'declined');

CREATE TABLE Transactions (
  id int PRIMARY key,
  country varchar(4) NOT NULL CHECK (country <> ''),
  state StateType NOT NULL,
  amount int NOT NULL,
  trans_date date NOT NULL
);

INSERT INTO
  Transactions (id, country, state, amount, trans_date)
VALUES
  ('121', 'US', 'approved', '1000', '2018-12-18'),
  ('122', 'US', 'declined', '2000', '2018-12-19'),
  ('123', 'US', 'approved', '2000', '2019-01-01'),
  ('124', 'DE', 'approved', '2000', '2019-01-07');

SELECT
  to_char(trans_date, 'YYYY-MM') AS "month",
  country,
  count(amount) AS trans_count,
  count(
    CASE state
      WHEN 'approved' THEN 1
      ELSE NULL
    END
  ) AS approved_count,
  sum(amount) AS trans_total_amount,
  sum(
    CASE state
      WHEN 'approved' THEN amount
      ELSE 0
    END
  ) AS approved_total_amount
FROM
  Transactions
GROUP BY
  "month", country;

ROLLBACK TO "test";

ROLLBACK;
