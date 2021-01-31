# Exercise 5:
#
# This program records the domain name (instead of the address) where the message
# was sent from instead of who the mail came from (i.e., the whole email address).
# At the end of the program, print out the contents of your dictionary.

fname = input("Enter file name: ")

try:
    fhand = open(fname)
except:
    print("Error, file doesn't exist or incorrect file name.")
    exit()

email = {}
adds = []

# Creating list with the address part of the email in it.
for line in fhand:
    wordlist = line.split()
    if len(wordlist) < 2 or wordlist[0] != "From":
        continue
    elist = wordlist[1]
    elist = elist.split("@")
    adds.append(elist[1])

# Creating a dictionary with email address and it's count in it.
for word in adds:
    email[word] = email.get(word, 0) + 1
print("\n", email, "\n")

emailCount = 0
emailAdd = None

# Finding out the maximum count for email address.
for k, v in email.items():
    if v > emailCount:
        emailCount = v
        emailAdd = k

print("\nHighest count\n", emailAdd, "\n", emailCount, "\n")
