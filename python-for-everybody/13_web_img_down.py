# To download image directly from the web, using the .jpeg image URL.

import os
import urllib.request, urllib.parse, urllib.error
import ssl

# Ignore SSL certificate errors
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

print("Please enter a URL ending with .jpg or .jpeg : ")
urlstr = input().strip()
img = urllib.request.urlopen(urlstr, context=ctx)

# Get the last "word"
words = urlstr.split("/")
fname = words[-1]

# Don't overwrite the file
if os.path.exists(fname):
    if input("Replace " + fname + " (Y/n)?") != "Y":
        print("Data not copied")
        exit()
    print("Replacing", fname)

fhand = open(fname, "wb")
size = 0
while True:
    info = img.read(100000)
    if len(info) < 1:
        break
    size = size + len(info)
    fhand.write(info)

print(size, "characters copied to", fname)
fhand.close()
