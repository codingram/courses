# In this assignment you will write a Python program that expands on
# http://www.py4e.com/code3/urllinks.py. The program will use urllib to
# read the HTML from the data files below, extract the href= vaues from
# the anchor tags, scan for a tag that is in a particular position relative
# to the first name in the list, follow that link and repeat the process a
# number of times and report the last name you find.
#
# URL: http://py4e-data.dr-chuck.net/known_by_Fikret.html
# URL: http://py4e-data.dr-chuck.net/known_by_Rossi.html

import urllib.request, urllib.error, urllib.parse
from bs4 import BeautifulSoup

url = input("Enter URL: ")
count = int(input("Enter count: ")) + 1
position = int(input("Enter position: "))

for i in range(count):
    print("Retrieving:", url)
    fileRead = urllib.request.urlopen(url).read().decode()
    soup = BeautifulSoup(fileRead, "html.parser")
    anchorTag = soup("a")
    linkList = [link.get("href", None) for link in anchorTag]
    url = linkList[position - 1]
