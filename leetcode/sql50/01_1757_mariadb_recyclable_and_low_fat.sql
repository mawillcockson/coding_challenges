#!/usr/bin/env mariadb test
/* Leetcode uses MySQL, and at time of writing, the following produces:

SHOW VARIABLES LIKE 'version';

| Variable_name | Value  |
| ------------- | ------ |
| version       | 8.0.40 |
*/

DROP TABLE IF EXISTS Products;
CREATE TABLE IF NOT EXISTS Products (product_id int, low_fats ENUM('Y', 'N'), recyclable ENUM('Y','N'));
INSERT INTO Products (product_id, low_fats, recyclable) VALUES ('0', 'Y', 'N');
INSERT INTO Products (product_id, low_fats, recyclable) VALUES ('1', 'Y', 'Y');
INSERT INTO Products (product_id, low_fats, recyclable) VALUES ('2', 'N', 'Y');
INSERT INTO Products (product_id, low_fats, recyclable) VALUES ('3', 'Y', 'Y');
INSERT INTO Products (product_id, low_fats, recyclable) VALUES ('4', 'N', 'N');

SELECT product_id FROM Products WHERE low_fats = 'Y' AND recyclable = 'Y';
