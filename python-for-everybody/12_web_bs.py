# Assignment: Scrapping HTML data with BeautifulSoup
#
# In this assignment you will write a Python program similar to
# http://www.py4e.com/code3/urllink2.py. The program will use urllib
# to read the HTML from the data files below, and parse the data,
# extracting numbers and compute the sum of the numbers in the file.

# Sample data: http://py4e-data.dr-chuck.net/comments_42.html (Sum = 2553)
# Actual data: http://py4e-data.dr-chuck.net/comments_754934.html (Sum = 2239)

import urllib.request, urllib.error, urllib.parse
from bs4 import BeautifulSoup

# Read the content of the url
urlRead = urllib.request.urlopen(input("Enter URL: ")).read()

# Parse it through BeautifulSoup
soup = BeautifulSoup(urlRead, "html.parser")

# List full of span tags and all the attributes and content inside it
spanTag = soup("span")

# Nested list comp to get the number in the span tag as an integer in the list
numList = [int(i) for i in [num.string for num in spanTag]]

# Printing the quantity and sum
print("Count:", len(numList))
print("Sum:", sum(numList))
