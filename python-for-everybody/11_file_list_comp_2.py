# Exercise 2:
#
# Write a program to look for lines of the form:
#
#   New Revision: 39772
#
# Extract the number from each of the lines using a regular expression and the
# findall() method. Compute the average of the numbers and print out the average
# as an integer.

import re

while True:
    fname = input("Enter file name: ")
    try:
        f = open(fname).read()
        break
    except IOError as err:
        print(err)

# List comprehension - Only after reading the file
numList = [int(num) for num in re.findall("New .+: ([0-9]+)", f)]
total = sum(numList)
count = len(numList)

print("Average:", int(total / count))
