--#!/usr/bin/env psql --no-password test
/*
Table: Products
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| product_id    | int     |
| new_price     | int     |
| change_date   | date    |
+---------------+---------+
(product_id, change_date) is the primary key (combination of columns with unique values) of this table.
Each row of this table indicates that the price of some product was changed to a new price at some date.


Write a solution to find the prices of all products on 2019-08-16. Assume the price of all products before any change is 10.

Return the result table in any order.

The result format is in the following example.


Example 1:
Input: 
Products table:
+------------+-----------+-------------+
| product_id | new_price | change_date |
+------------+-----------+-------------+
| 1          | 20        | 2019-08-14  |
| 2          | 50        | 2019-08-14  |
| 1          | 30        | 2019-08-15  |
| 1          | 35        | 2019-08-16  |
| 2          | 65        | 2019-08-17  |
| 3          | 20        | 2019-08-18  |
+------------+-----------+-------------+
Output: 
+------------+-------+
| product_id | price |
+------------+-------+
| 2          | 50    |
| 1          | 35    |
| 3          | 10    |
+------------+-------+
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Products (
  product_id int NOT NULL CHECK (product_id >= 0),
  new_price int NOT NULL CHECK (new_price > 0),
  change_date date NOT NULL,
  CONSTRAINT products_pk PRIMARY key (product_id, change_date)
);

INSERT INTO
  Products (product_id, new_price, change_date)
VALUES
  ('1', '20', '2019-08-14'),
  ('2', '50', '2019-08-14'),
  ('1', '30', '2019-08-15'),
  ('1', '35', '2019-08-16'),
  ('2', '65', '2019-08-17'),
  ('3', '20', '2019-08-18');

WITH
  product_ids AS (
    SELECT DISTINCT
      product_id
    FROM
      Products
  ),
  defaults AS (
    SELECT
      product_id,
      cast(10 AS int) AS new_price,
      cast('2019-08-16' AS date) AS change_date
    FROM
      product_ids
  ),
  at_point AS (
    SELECT
      d.product_id,
      p.new_price,
      d.change_date
    FROM
      Products AS p
      RIGHT OUTER JOIN defaults AS d ON p.product_id = d.product_id
      AND p.change_date = '2019-08-16'
    WHERE
      p.new_price IS NULL
  ),
  with_point AS (
    SELECT
      *
    FROM
      at_point
    UNION ALL
    SELECT
      *
    FROM
      Products
  ),
  price_at_point AS (
    SELECT
      p.product_id,
      p.change_date,
      coalesce(
        coalesce(
          p.new_price,
          lag (p.new_price) OVER (
            PARTITION BY
              p.product_id
            ORDER BY
              p.change_date ASC RANGE BETWEEN UNBOUNDED PRECEDING
              AND CURRENT ROW
          )
        ),
        10
      ) AS price
    FROM
      with_point AS p
  )
SELECT
  product_id,
  price
FROM
  price_at_point
WHERE
  change_date = '2019-08-16';

ROLLBACK TO "test";

ROLLBACK;
