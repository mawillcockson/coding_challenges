--#!/usr/bin/env psql --no-password test
/* Leetcode uses PostgreSQL 16 */
/*
BEGIN TRANSACTION;
SAVEPOINT "test";

DROP TABLE IF EXISTS Products;
DROP TYPE IF EXISTS yesno;

CREATE TYPE yesno AS ENUM ('Y', 'N');
CREATE TEMPORARY TABLE Products (product_id int, low_fats yesno, recyclable yesno) ON COMMIT DROP;
INSERT INTO Products (product_id, low_fats, recyclable) VALUES
    ('0', 'Y', 'N'),
    ('1', 'Y', 'Y'),
    ('2', 'N', 'Y'),
    ('3', 'Y', 'Y'),
    ('4', 'N', 'N');
*/
SELECT product_id FROM Products WHERE low_fats = 'Y' AND recyclable = 'Y';
/*
ROLLBACK TO "test";
ROLLBACK;
*/
