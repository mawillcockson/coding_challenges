--#!/usr/bin/env psql --no-password test
/*
challenge description
*/
BEGIN;
SAVEPOINT "test";



ROLLBACK TO "test";
ROLLBACK;
