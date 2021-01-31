# Exercise 1:
#
# Revise a previous program as follows:
# Read and parse the “From” lines and pull out the addresses from the line.
# Count the number of messages from each person using a dictionary.
#
# After all the data has been read, print the person with the most commits by
# creating a list of (count, email) tuples from the dictionary. Then sort the
# list in reverse order and print out the person who has the most commits.

while True:
    try:
        fname = input("Enter file name: ")
        fh = open(fname)
        break
    except IOError as err:
        print(err)

elist = []
for line in fh:
    line = line.rstrip()
    words = line.split()
    if len(words) < 2 or words[0] != "From":
        continue
    elist.append(words[1])

edict = {}
for i in elist:
    edict[i] = edict.get(i, 0) + 1

elsort = []
for email, count in edict.items():
    elsort.append((count, email))

elsort = sorted(elsort, reverse=True)

for count, email in elsort[:1]:
    print(email, count)
