# Exercise 4:
#
# Add code to the above program to figure out who has the most messages in the file.
# After all the data has been read and the dictionary has been created, look through
# the dictionary using a maximum loop (see Chapter 5: Maximum and minimum loops) to
# find who has the most messages and print how many messages the person has.

fname = input("Enter file name: ")

try:
    fhand = open(fname)
except:
    print("Error, file doesn't exist or incorrect file name.")
    exit()

day = {}
lwords = []

# Creating list with the email address in it.
for line in fhand:
    wordlist = line.split()
    if len(wordlist) < 2 or wordlist[0] != "From":
        continue
    lwords.append(wordlist[1])

# Creating a dictionary with email address and it's count in it.
for word in lwords:
    day[word] = day.get(word, 0) + 1

emailCount = 0
emailAdd = None

# Finding out the maximum count for email address.
for k, v in day.items():
    if v > emailCount:
        emailCount = v
        emailAdd = k

print(emailAdd, emailCount)
