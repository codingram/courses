"""Assume s is a string of lower case characters.

Write a program that counts up the number of vowels contained in the string s.
Valid vowels are: 'a', 'e', 'i', 'o', and 'u'.
For example, if s = 'azcbobobegghakl', your program should print:

Number of vowels: 5
"""

# s = input('Enter characters: ')
s = "azcbobobegghakl"
count = 0
for i in s:
    if i in "aeiou":
        count += 1

print("Number of vowels:", count)

# s = 'azcbobobegghakl'
# print('Number of vowels:', sum(i in 'aeiou' for i in s))
