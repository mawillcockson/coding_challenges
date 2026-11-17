Issues:
1) The log file is in `~/data` not `/data`
2) The comment about `input()` evaluating expressions is referencing python2, when the shebang is using python3
3) The user input is re-processed for each line of the log file
4) Splitting and downcasing the user input should be done by: `error_patterns.extend(error.lower().split(' '))`
5) The letter case of the user input and log lines shouldn't be changed; use the `re.IGNORECASE` flag mentioned in the prior lessons
6) `file.close()` is redundant inside a `with open()` context manager
7) The content of the `if __name__ == "__main__"` block should moved to a `main()` function, and replaced with a call to that function
