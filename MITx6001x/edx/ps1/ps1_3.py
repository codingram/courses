""" Assume s is a string of lower case characters.

Write a program that prints the longest substring of s in which the letters occur
in alphabetical order. For example, if s = 'azcbobobegghakl', then your program
should print:

    Longest substring in alphabetical order is: beggh

In the case of ties, print the first substring. For example, if s = 'abcbcd',
then your program should print:

    Longest substring in alphabetical order is: abc
"""

# s = 'abcbcd'
s = "azcbobobegghakl"
# s = 'aaaaaaaaaaaabaaaaaaaaaazaaaaaaaaaafaaaaaaaaayaaaaaaaa'

strg = s[0]

for let in s[1:]:
    strg = strg + let if strg[-1] <= let else strg + " " + let

strList = strg.split()

largest = ""
for i in strList:
    if len(i) > len(largest):
        largest = i

print("Longest substring in alphabetical order is:", largest)
