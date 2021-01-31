# In this assignment you will read through and parse a file (ass_text.txt) with
# text and numbers. You will extract all the numbers in the file and compute the
# sum of the numbers.

import re

while True:
    try:
        f = open(input("Enter file name: ")).read()
        break
    except IOError as err:
        print(err)

# List comprehension
listcomp = [int(num) for num in re.findall("[0-9]+", f)]

print("Total:", sum(listcomp))
print("Quantity:", len(listcomp))
