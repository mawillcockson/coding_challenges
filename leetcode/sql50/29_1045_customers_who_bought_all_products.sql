--#!/usr/bin/env psql --no-password test
/*
Table: Customer
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| customer_id | int     |
| product_key | int     |
+-------------+---------+
This table may contain duplicates rows. 
customer_id is not NULL.
product_key is a foreign key (reference column) to Product table.

Table: Product
+-------------+---------+
| Column Name | Type    |
+-------------+---------+
| product_key | int     |
+-------------+---------+
product_key is the primary key (column with unique values) for this table.


Write a solution to report the customer ids from the Customer table that bought all the products in the Product table.

Return the result table in any order.

The result format is in the following example.


Example 1:
Input: 
Customer table:
+-------------+-------------+
| customer_id | product_key |
+-------------+-------------+
| 1           | 5           |
| 2           | 6           |
| 3           | 5           |
| 3           | 6           |
| 1           | 6           |
+-------------+-------------+
Product table:
+-------------+
| product_key |
+-------------+
| 5           |
| 6           |
+-------------+
Output: 
+-------------+
| customer_id |
+-------------+
| 1           |
| 3           |
+-------------+
Explanation: 
The customers who bought all the products (5 and 6) are customers with IDs 1 and 3.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Customer (customer_id int NOT NULL, product_key int);

CREATE TABLE Product (product_key int PRIMARY key);

INSERT INTO
  Customer (customer_id, product_key)
VALUES
  ('1', '5'),
  ('2', '6'),
  ('3', '5'),
  ('3', '6'),
  ('1', '6');

INSERT INTO
  Product (product_key)
VALUES
  ('5'),
  ('6');

ALTER TABLE Customer add CONSTRAINT customer_product_key_fk FOREIGN key (product_key) REFERENCES Product (product_key);

ALTER TABLE Customer validate CONSTRAINT customer_product_key_fk;

WITH
  products AS (
    SELECT
      count(product_key) AS quantity
    FROM
      Product
  ),
  uniques AS (
    SELECT DISTINCT
      customer_id,
      product_key
    FROM
      Customer
  ),
  counts AS (
    SELECT
      customer_id,
      count(product_key) AS purchased
    FROM
      uniques
    GROUP BY
      customer_id
  )
SELECT
  customer_id
FROM
  counts
  CROSS JOIN products
WHERE
  counts.purchased = products.quantity;

ROLLBACK TO "test";

ROLLBACK;
