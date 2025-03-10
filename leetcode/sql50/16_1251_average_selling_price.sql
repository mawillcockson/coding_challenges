--#!/usr/bin/env psql --no-password test
/*
Table: Prices
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| product_id    | int     |
| start_date    | date    |
| end_date      | date    |
| price         | int     |
+---------------+---------+
(product_id, start_date, end_date) is the primary key (combination of columns with unique values) for this table.
Each row of this table indicates the price of the product_id in the period from start_date to end_date.
For each product_id there will be no two overlapping periods. That means there will be no two intersecting periods for the same product_id.

Table: UnitsSold
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| product_id    | int     |
| purchase_date | date    |
| units         | int     |
+---------------+---------+
This table may contain duplicate rows.
Each row of this table indicates the date, units, and product_id of each product sold. 


Write a solution to find the average selling price for each product. average_price should be rounded to 2 decimal places. If a product does not have any sold units, its average selling price is assumed to be 0.

Return the result table in any order.

The result format is in the following example.


Example 1:
Input: 
Prices table:
+------------+------------+------------+--------+
| product_id | start_date | end_date   | price  |
+------------+------------+------------+--------+
| 1          | 2019-02-17 | 2019-02-28 | 5      |
| 1          | 2019-03-01 | 2019-03-22 | 20     |
| 2          | 2019-02-01 | 2019-02-20 | 15     |
| 2          | 2019-02-21 | 2019-03-31 | 30     |
+------------+------------+------------+--------+
UnitsSold table:
+------------+---------------+-------+
| product_id | purchase_date | units |
+------------+---------------+-------+
| 1          | 2019-02-25    | 100   |
| 1          | 2019-03-01    | 15    |
| 2          | 2019-02-10    | 200   |
| 2          | 2019-03-22    | 30    |
+------------+---------------+-------+
Output: 
+------------+---------------+
| product_id | average_price |
+------------+---------------+
| 1          | 6.96          |
| 2          | 16.96         |
+------------+---------------+
Explanation: 
Average selling price = Total Price of Product / Number of products sold.
Average selling price for product 1 = ((100 * 5) + (15 * 20)) / 115 = 6.96
Average selling price for product 2 = ((200 * 15) + (30 * 30)) / 230 = 16.96
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Prices (
  product_id int NOT NULL,
  start_date date NOT NULL,
  end_date date NOT NULL,
  price int NOT NULL,
  CONSTRAINT prices_pk PRIMARY key (product_id, start_date, end_date)
);

CREATE TABLE UnitsSold (
  product_id int NOT NULL,
  purchase_date date NOT NULL,
  units int NOT NULL
);

INSERT INTO
  Prices (product_id, start_date, end_date, price)
VALUES
  ('1', '2019-02-17', '2019-02-28', '5'),
  ('1', '2019-03-01', '2019-03-22', '20'),
  ('2', '2019-02-01', '2019-02-20', '15'),
  ('2', '2019-02-21', '2019-03-31', '30');

INSERT INTO
  UnitsSold (product_id, purchase_date, units)
VALUES
  ('1', '2019-02-25', '100'),
  ('1', '2019-03-01', '15'),
  ('2', '2019-02-10', '200'),
  ('2', '2019-03-22', '30');

WITH
  period_totals AS (
    SELECT
      p.product_id,
      coalesce(units * price, 0) AS period_total,
      coalesce(units, 0) as units
    FROM
      Prices AS p
      LEFT OUTER JOIN UnitsSold AS us ON us.product_id = p.product_id
      AND (
        (us.purchase_date, us.purchase_date) OVERLAPS (p.start_date, p.end_date)
        OR us.purchase_date = p.end_date
      )
  )
SELECT
  product_id,
  CASE
    WHEN sum(units) = 0 THEN 0.00
    ELSE round(
      cast(
        cast(sum(period_total) AS double PRECISION) / sum(units) AS numeric
      ),
      2
    )
  END AS average_price
FROM
  period_totals
GROUP BY
  product_id;

ROLLBACK TO "test";

ROLLBACK;
