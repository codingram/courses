# Exercise 3:
#
# Write a program to read through a mail log, build a histogram using a
# dictionary to count how many messages have come from each email address,
# and print the dictionary.

fname = input("Enter file name: ")

try:
    fhand = open(fname)
except:
    print("Error, file doesn't exist or incorrect file name.")
    exit()

day = {}
lwords = []

for line in fhand:
    wordlist = line.split()
    if len(wordlist) < 2 or wordlist[0] != "From":
        continue
    lwords.append(wordlist[1])

for word in lwords:
    day[word] = day.get(word, 0) + 1
print(day)
