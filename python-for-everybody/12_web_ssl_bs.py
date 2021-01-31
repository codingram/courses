# Exercise 4:
#
# Change the urllinks.py program to extract and count paragraph (p) tags from the
# retrieved HTML document and display the count of the paragraphs as the output of
# your program. Do not display the paragraph text, only count them. Test your
# program on several small web pages as well as some larger web pages.

import urllib.request, urllib.parse, urllib.error
from bs4 import BeautifulSoup
import ssl

# Ignore SSL certificate errors
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

# Checking the URL
print("Enter 'e' to exit the program.")
while True:
    url = input("Enter URL: ")
    if url == "e":
        exit()
    try:
        html = urllib.request.urlopen(url, context=ctx)
        break
    except:
        print("Error, wrong URL.")

# Input the tag and read the entire document
inputTag = input("Enter a tag: ")
rhtml = html.read()
soup = BeautifulSoup(rhtml, "html.parser")

# Retrieve all of the input tags and print the total count
count = 0
tags = soup(inputTag)
print(tags)
for i in tags:
    count += 1

print("Total count of <" + inputTag + "> tags: " + str(count) + "\n")
