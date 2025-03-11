Hello, I believe this person is trying to report the same issue as was reported in #20005 and #19728. The latter issue indicates a fix, which has not yet been implemented for the problem.

Also, unfortunately, the fix suggested in #19728 for that test case would not work in at least PostgreSQL:

```sql
CREATE TABLE Activities (
  sell_date date,
  product varchar(20)
);

INSERT INTO
  Activities (sell_date, product)
VALUES
  ('2020-05-30', 'Headphone'),
  ('2020-06-02', 'Mask'),   -- these two are swapped as
  ('2020-06-01', 'Pencil'), -- compared to the test case
  ('2020-05-30', 'Basketball'),
  ('2020-06-01', 'Bible'),
  ('2020-06-02', 'Mask'),
  ('2020-05-30', 'T-Shirt');

EXPLAIN SELECT
  sell_date,
  count(DISTINCT product) AS "num_sold",
  string_agg (
    DISTINCT product,
    ','
    ORDER BY
      product ASC
  ) AS "products"
FROM
  Activities
GROUP BY
  sell_date;
-- no `ORDER BY sell_date ASC` clause is included
```

Output:

```text
                                QUERY PLAN                                
--------------------------------------------------------------------------
 GroupAggregate  (cost=63.16..74.66 rows=200 width=44)
   Group Key: sell_date
   ->  Sort  (cost=63.16..65.41 rows=900 width=62)
         Sort Key: sell_date, product
         ->  Seq Scan on activities  (cost=0.00..19.00 rows=900 width=62)
(5 rows)
```

I assume that this means that the output table will always be sorted on `sell_date`.

I'm guessing this is because the `GROUP BY` clause is present, and one of the following is true:

- at least one aggregate function has a `DISTINCT`
- at least one aggregate function has an `ORDER BY` clause.

I think that the number of times this issue has been reported indicates that this automatic sorting behavior is unexpected. #20005 is using [`pandas`][], and so I'm not sure if this is caused by the same automatic sorting, but regardless, I think the question could be improved by either highlighting this behavior—showing how it's a natural consequence of the query—or by forcing it to be handled by requiring the output ordering to be something other than the default of ascending.

[this issue]: <https://github.com/LeetCode-Feedback/LeetCode-Feedback/issues/27740>
[`pandas`]: <https://pandas.pydata.org/>
