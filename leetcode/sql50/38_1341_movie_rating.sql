--#!/usr/bin/env psql --no-password test
/*
Table: Movies
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| movie_id      | int     |
| title         | varchar |
+---------------+---------+
movie_id is the primary key (column with unique values) for this table.
title is the name of the movie.

Table: Users
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| user_id       | int     |
| name          | varchar |
+---------------+---------+
user_id is the primary key (column with unique values) for this table.
The column 'name' has unique values.

Table: MovieRating
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| movie_id      | int     |
| user_id       | int     |
| rating        | int     |
| created_at    | date    |
+---------------+---------+
(movie_id, user_id) is the primary key (column with unique values) for this table.
This table contains the rating of a movie by a user in their review.
created_at is the user's review date. 


Write a solution to:

Find the name of the user who has rated the greatest number of movies. In case of a tie, return the lexicographically smaller user name.
Find the movie name with the highest average rating in February 2020. In case of a tie, return the lexicographically smaller movie name.

The result format is in the following example.


Example 1:
Input: 
Movies table:
+-------------+--------------+
| movie_id    |  title       |
+-------------+--------------+
| 1           | Avengers     |
| 2           | Frozen 2     |
| 3           | Joker        |
+-------------+--------------+
Users table:
+-------------+--------------+
| user_id     |  name        |
+-------------+--------------+
| 1           | Daniel       |
| 2           | Monica       |
| 3           | Maria        |
| 4           | James        |
+-------------+--------------+
MovieRating table:
+-------------+--------------+--------------+-------------+
| movie_id    | user_id      | rating       | created_at  |
+-------------+--------------+--------------+-------------+
| 1           | 1            | 3            | 2020-01-12  |
| 1           | 2            | 4            | 2020-02-11  |
| 1           | 3            | 2            | 2020-02-12  |
| 1           | 4            | 1            | 2020-01-01  |
| 2           | 1            | 5            | 2020-02-17  | 
| 2           | 2            | 2            | 2020-02-01  | 
| 2           | 3            | 2            | 2020-03-01  |
| 3           | 1            | 3            | 2020-02-22  | 
| 3           | 2            | 4            | 2020-02-25  | 
+-------------+--------------+--------------+-------------+
Output: 
+--------------+
| results      |
+--------------+
| Daniel       |
| Frozen 2     |
+--------------+
Explanation: 
Daniel and Monica have rated 3 movies ("Avengers", "Frozen 2" and "Joker") but Daniel is smaller lexicographically.
Frozen 2 and Joker have a rating average of 3.5 in February but Frozen 2 is smaller lexicographically.
*/
BEGIN;

SAVEPOINT "test";

CREATE TABLE Movies (
  movie_id int PRIMARY key,
  title varchar(30) NOT NULL CHECK (title <> '')
);

CREATE TABLE Users (
  user_id int PRIMARY key,
  name varchar(30) NOT NULL CHECK (name <> '')
);

CREATE TABLE MovieRating (
  movie_id int NOT NULL,
  user_id int NOT NULL,
  rating int NOT NULL CHECK (
    rating >= 1
    AND rating <= 5
  ),
  created_at date NOT NULL CHECK (
    created_at - cast(date_trunc ('day', now ()) AS date) <= 0
  ),
  CONSTRAINT movierating_pk PRIMARY key (movie_id, user_id)
);

INSERT INTO
  Movies (movie_id, title)
VALUES
  ('1', 'Avengers'),
  ('2', 'Frozen 2'),
  ('3', 'Joker');

INSERT INTO
  Users (user_id, name)
VALUES
  ('1', 'Daniel'),
  ('2', 'Monica'),
  ('3', 'Maria'),
  ('4', 'James');

INSERT INTO
  MovieRating (movie_id, user_id, rating, created_at)
VALUES
  ('1', '1', '3', '2020-01-12'),
  ('1', '2', '4', '2020-02-11'),
  ('1', '3', '2', '2020-02-12'),
  ('1', '4', '1', '2020-01-01'),
  ('2', '1', '5', '2020-02-17'),
  ('2', '2', '2', '2020-02-01'),
  ('2', '3', '2', '2020-03-01'),
  ('3', '1', '3', '2020-02-22'),
  ('3', '2', '4', '2020-02-25');

ALTER TABLE MovieRating add CONSTRAINT movierating_movie_id_fk FOREIGN key (movie_id) REFERENCES Movies (movie_id);

ALTER TABLE MovieRating validate CONSTRAINT movierating_movie_id_fk;

ALTER TABLE MovieRating add CONSTRAINT movierating_user_id_fk FOREIGN key (user_id) REFERENCES Users (user_id);

ALTER TABLE MovieRating validate CONSTRAINT movierating_user_id_fk;

WITH
  num_reviews AS (
    SELECT
      user_id,
      count(user_id) AS total
    FROM
      MovieRating
    GROUP BY
      user_id
  ),
  most_reviews AS (
    SELECT
      u.name
    FROM
      num_reviews AS n
      INNER JOIN Users AS u ON n.user_id = u.user_id
    ORDER BY
      n.total DESC,
      u.name ASC
    LIMIT
      1
  ),
  average_monthly_rating AS (
    SELECT
      movie_id,
      cast(date_trunc ('month', created_at) AS date) AS "month",
      avg(rating) AS avg_rating
    FROM
      MovieRating
    GROUP BY
      movie_id,
      "month"
  ),
  highest_monthly_rating AS (
    SELECT
      m.title
    FROM
      average_monthly_rating AS a
      INNER JOIN Movies AS m ON a.movie_id = m.movie_id
    WHERE
      a."month" = '2020-02-01'
    ORDER BY
      a.avg_rating DESC,
      m.title ASC
    LIMIT
      1
  )
SELECT
  name AS results
FROM
  most_reviews
UNION ALL
SELECT
  title AS results
FROM
  highest_monthly_rating;

ROLLBACK TO "test";

ROLLBACK;
