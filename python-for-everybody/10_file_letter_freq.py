# Exercise 3:
#
# Write a program that reads a file and prints the letters in decreasing order
# of frequency. Your program should convert all the input to lower case and only
# count the letters a-z. Your program should not count spaces, digits, punctuation,
# or anything other than the letters a-z.
#
# Find text samples from several different languages and see how letter frequency
# varies between languages. Compare your results with the tables at
# https://wikipedia.org/wiki/Letter_frequencies.

import re

LS = "-" * 50

while True:
    try:
        fname = input("Enter file name: ")
        fh = open(fname)
        break
    except IOError as err:
        print(err)

letDict = {}
totalVal = 0

# Using regex
for line in fh:
    letList = re.findall("[a-z]", line.lower())
    for let in letList:
        letDict[let] = letDict.get(let, 0) + 1
        totalVal += 1

letSort = sorted([(val, let) for let, val in letDict.items()], reverse=True)


print(
    f"{LS}\n{'Letter'.center(10)}{'Frequency'.center(12)}{'Percentage'.center(12)}\n{LS}"
)

for val, let in letSort:
    per = round(val / totalVal * 100, 3)
    print(f"{let+' '.ljust(9,'.')}  {val:<10}{per:>7} %")

print(f"{LS}\nTotal letter count: {totalVal}\n{LS}")

# Using old ways
# from string import punctuation, digits
# for line in fh:
#     line = line.lower().translate(str.maketrans('','', punctuation + digits))
#     words = line.split()
#     for i in words:
#         letter = list(i)
#         for j in letter:
#             letDict[j] = letDict.get(j,0) + 1
#             totalVal += 1
