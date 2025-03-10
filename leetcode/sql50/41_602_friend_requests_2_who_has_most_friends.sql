--#!/usr/bin/env psql --no-password test
/*
Table: RequestAccepted
+----------------+---------+
| Column Name    | Type    |
+----------------+---------+
| requester_id   | int     |
| accepter_id    | int     |
| accept_date    | date    |
+----------------+---------+
(requester_id, accepter_id) is the primary key (combination of columns with unique values) for this table.
This table contains the ID of the user who sent the request, the ID of the user who received the request, and the date when the request was accepted.


Write a solution to find the people who have the most friends and the most friends number.

The test cases are generated so that only one person has the most friends.

The result format is in the following example.


Example 1:
Input: 
RequestAccepted table:
+--------------+-------------+-------------+
| requester_id | accepter_id | accept_date |
+--------------+-------------+-------------+
| 1            | 2           | 2016/06/03  |
| 1            | 3           | 2016/06/08  |
| 2            | 3           | 2016/06/08  |
| 3            | 4           | 2016/06/09  |
+--------------+-------------+-------------+
Output: 
+----+-----+
| id | num |
+----+-----+
| 3  | 3   |
+----+-----+
Explanation: 
The person with id 3 is a friend of people 1, 2, and 4, so he has three friends in total, which is the most number than any others.

Follow up: In the real world, multiple people could have the same most number of friends. Could you find all these people in this case?
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE RequestAccepted (
  requester_id int NOT NULL,
  accepter_id int NULL, -- why would this be nullable??
  accept_date date NULL, -- this also makes no sense to allow to be nullable!
  CONSTRAINT RequestAccepted_pk PRIMARY key (requester_id, accepter_id),
  CONSTRAINT RequestAccepted_no_self_friend CHECK (requester_id <> accepter_id)
);

INSERT INTO
  RequestAccepted (requester_id, accepter_id, accept_date)
VALUES
  ('1', '2', '2016/06/03'),
  ('1', '3', '2016/06/08'),
  ('2', '3', '2016/06/08'),
  ('3', '4', '2016/06/09'),
  (6, 1, '2020-01-01'),
  (6, 2, '2020-01-01'),
  (6, 3, '2020-01-01'),
  (6, 4, '2020-01-01'),
  (6, 5, '2020-01-01');

WITH
  requesters AS (
    SELECT
      requester_id,
      count(requester_id) AS "requested"
    FROM
      RequestAccepted
    GROUP BY
      requester_id
  ),
  acceptors AS (
    SELECT
      accepter_id,
      count(accepter_id) AS "accepted"
    FROM
      RequestAccepted
    GROUP BY
      accepter_id
  ),
  counts AS (
    SELECT
      coalesce(requester_id, accepter_id) AS "id",
      coalesce(requested, 0) + coalesce(accepted, 0) AS "num"
    FROM
      requesters
      FULL OUTER JOIN acceptors ON requester_id = accepter_id
    ORDER BY
      "num" DESC
  )
SELECT
  "id",
  num
FROM
  counts
WHERE
  num = (
    SELECT
      max(num)
    FROM
      counts
  );

ROLLBACK TO "test";

ROLLBACK;
