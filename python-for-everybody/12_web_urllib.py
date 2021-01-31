# Exercise 3:
#
# Use urllib to replicate the previous exercise of
# (1) retrieving the document from a URL,
# (2) displaying up to 3000 characters, and
# (3) counting the overall number of characters in the document.
# Don’t worry about the headers for this exercise, simply show the first
# 3000 characters of the document contents.

import urllib.request, urllib.parse, urllib.error

print("Enter 'e' to exit the program.")

while True:
    url = input("Enter URL: ")
    try:
        fh = urllib.request.urlopen(url)
        break
    except:
        print("Error, an improperly formatted URL or non-existant URL.")

count = 0
pdata = b""

for line in fh:
    if len(line) < 1:
        break
    count += len(line)
    pdata += line

print(pdata[:3000].decode().strip() + "......")
print("\nTotal number of characters:", count, "\n")


#####################################

# Direct printing the data on the file after reading the entire text.
# fr = fh.read()
# print(fr.decode(),end = '')

# Line after line just like before
# for line in fh:
#     print(line.decode().strip())


# Counting the letter directly from the remote file
# count = {}
# for line in fh:
#     words = line.decode().strip()
#     for i in words:
#         count[i] = count.get(i,0) + 1
# print(count)
