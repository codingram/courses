# Exercise 2:
#
# Write a program that categorizes each mail message by which day of the week
# the commit was done. To do this look for lines that start with “From”, then
# look for the third word and keep a running count of each of the days of the
# week. At the end of the program print out the contents of your dictionary
# (order does not matter).

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
    if len(wordlist) < 3 or wordlist[0] != "From":
        continue
    lwords.append(wordlist[2])

for word in lwords:
    day[word] = day.get(word, 0) + 1
print(day)
