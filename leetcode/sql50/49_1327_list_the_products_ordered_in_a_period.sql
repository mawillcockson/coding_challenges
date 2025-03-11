--#!/usr/bin/env psql --no-password test
/*
Table: Products
+------------------+---------+
| Column Name      | Type    |
+------------------+---------+
| product_id       | int     |
| product_name     | varchar |
| product_category | varchar |
+------------------+---------+
product_id is the primary key (column with unique values) for this table.
This table contains data about the company's products.

Table: Orders
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| product_id    | int     |
| order_date    | date    |
| unit          | int     |
+---------------+---------+
This table may have duplicate rows.
product_id is a foreign key (reference column) to the Products table.
unit is the number of products ordered in order_date.


Write a solution to get the names of products that have at least 100 units ordered in February 2020 and their amount.

Return the result table in any order.

The result format is in the following example.


Example 1:
Input: 
Products table:
+-------------+-----------------------+------------------+
| product_id  | product_name          | product_category |
+-------------+-----------------------+------------------+
| 1           | Leetcode Solutions    | Book             |
| 2           | Jewels of Stringology | Book             |
| 3           | HP                    | Laptop           |
| 4           | Lenovo                | Laptop           |
| 5           | Leetcode Kit          | T-shirt          |
+-------------+-----------------------+------------------+
Orders table:
+--------------+--------------+----------+
| product_id   | order_date   | unit     |
+--------------+--------------+----------+
| 1            | 2020-02-05   | 60       |
| 1            | 2020-02-10   | 70       |
| 2            | 2020-01-18   | 30       |
| 2            | 2020-02-11   | 80       |
| 3            | 2020-02-17   | 2        |
| 3            | 2020-02-24   | 3        |
| 4            | 2020-03-01   | 20       |
| 4            | 2020-03-04   | 30       |
| 4            | 2020-03-04   | 60       |
| 5            | 2020-02-25   | 50       |
| 5            | 2020-02-27   | 50       |
| 5            | 2020-03-01   | 50       |
+--------------+--------------+----------+
Output: 
+--------------------+---------+
| product_name       | unit    |
+--------------------+---------+
| Leetcode Solutions | 130     |
| Leetcode Kit       | 100     |
+--------------------+---------+
Explanation: 
Products with product_id = 1 is ordered in February a total of (60 + 70) = 130.
Products with product_id = 2 is ordered in February a total of 80.
Products with product_id = 3 is ordered in February a total of (2 + 3) = 5.
Products with product_id = 4 was not ordered in February 2020.
Products with product_id = 5 is ordered in February a total of (50 + 50) = 100.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Products (
  product_id int PRIMARY key,
  product_name varchar(40) NOT NULL CHECK (product_name <> ''),
  product_category varchar(40) NOT NULL CHECK (product_name <> '')
);

CREATE TABLE Orders (
  product_id int REFERENCES Products (product_id),
  order_date date NOT NULL,
  unit int NOT NULL CHECK (unit >= 0)
);

INSERT INTO
  Products (product_id, product_name, product_category)
VALUES
  ('1', 'Leetcode Solutions', 'Book'),
  ('2', 'Jewels of Stringology', 'Book'),
  ('3', 'HP', 'Laptop'),
  ('4', 'Lenovo', 'Laptop'),
  ('5', 'Leetcode Kit', 'T-shirt');

INSERT INTO
  Orders (product_id, order_date, unit)
VALUES
  ('1', '2020-02-05', '60'),
  ('1', '2020-02-10', '70'),
  ('2', '2020-01-18', '30'),
  ('2', '2020-02-11', '80'),
  ('3', '2020-02-17', '2'),
  ('3', '2020-02-24', '3'),
  ('4', '2020-03-01', '20'),
  ('4', '2020-03-04', '30'),
  ('4', '2020-03-04', '60'),
  ('5', '2020-02-25', '50'),
  ('5', '2020-02-27', '50'),
  ('5', '2020-03-01', '50');

-- Write a solution to get the names of products that have at least 100 units ordered in February 2020 and their amount.
WITH
/* I could do the join as part of the group-by and having statement, but that
* would require grouping on `product_name`, and that column is not part of the
* primary key of the Products table, which means there can be multiple products
* with the same name, as long as they have different ids. This would be poor
* database design in most real-world situations, and so here, since I can't
* address that, I think it's better to surface it by potentially showing
* multiple products, all with similar names, as different products. This query
* design of a CTE that filters, and then doing the join second, allows that. */
step1 AS (
    SELECT
      product_id,
      sum(unit) AS "unit"
    FROM
      Orders
    GROUP BY
      cast(date_trunc ('month', order_date) AS date),
      product_id
    HAVING
      cast(date_trunc ('month', order_date) AS date) = '2020-02-01'
  )
SELECT
-- product_name | unit
  p1.product_name,
  s1.unit
FROM
  step1 AS s1
  INNER JOIN Products AS p1 ON s1.product_id = p1.product_id
WHERE
  s1.unit >= 100;

ROLLBACK TO "test";

ROLLBACK;
