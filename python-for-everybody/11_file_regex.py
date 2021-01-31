# Exercise 1:
#
# Write a simple program to simulate the operation of the grep command on Unix.
# Ask the user to enter a regular expression and count the number of lines that
# matched the regular expression:
#
# ^Author
# ^X-
# java$

import re

LINE_SEP = "-" * 50

fh = open("mbox.txt")
regEx = input(f"{LINE_SEP}\nEnter a regular expression: ")

count = 0
for line in fh:
    if re.search(regEx, line):
        count += 1

print(f"mbox.txt had {count} lines that matched {regEx}\n{LINE_SEP}")
