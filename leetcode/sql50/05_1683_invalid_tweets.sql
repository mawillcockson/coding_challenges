--#!/usr/bin/env psql --no-password test
/*
+----------------+---------+
| Column Name    | Type    |
+----------------+---------+
| tweet_id       | int     |
| content        | varchar |
+----------------+---------+
tweet_id is the primary key (column with unique values) for this table.
content consists of characters on an American Keyboard, and no other special characters.
This table contains all the tweets in a social media app.


Write a solution to find the IDs of the invalid tweets. The tweet is invalid if the number of characters used in the content of the tweet is strictly greater than 15.

Return the result table in any order.

The result format is in the following example.


Example 1:

Input: 
Tweets table:
+----------+-----------------------------------+
| tweet_id | content                           |
+----------+-----------------------------------+
| 1        | Let us Code                       |
| 2        | More than fifteen chars are here! |
+----------+-----------------------------------+
Output: 
+----------+
| tweet_id |
+----------+
| 2        |
+----------+
Explanation: 
Tweet 1 has length = 11. It is a valid tweet.
Tweet 2 has length = 33. It is an invalid tweet.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE tweets (
  tweet_id integer PRIMARY key generated BY DEFAULT AS IDENTITY (
    sequence name tweets_id START
    WITH
      1 increment BY 1
  ),
  content varchar(50) NOT NULL CHECK (content <> '')
);

INSERT INTO
  tweets (tweet_id, content)
VALUES
  ('1', 'Let us Code'),
  ('2', 'More than fifteen chars are here!') returning *;

SELECT
  tweet_id
FROM
  tweets
WHERE
  character_length(normalize(content)) > 15;

ROLLBACK TO "test";

ROLLBACK;
