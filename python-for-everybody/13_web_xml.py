# Extracting Data from XML
# In this assignment you will write a Python program somewhat similar to
# http://www.py4e.com/code3/geoxml.py. The program will prompt for a URL,
# read the XML data from that URL using urllib and then parse and extract
# the comment counts from the XML data, compute the sum of the numbers in
# the file.

# Sample data: http://py4e-data.dr-chuck.net/comments_42.xml (Sum=2553)
# Actual data: http://py4e-data.dr-chuck.net/comments_754936.xml (Sum=2435)

import xml.etree.ElementTree as ET
import urllib.request, urllib.parse, urllib.error

url = input("Enter location: ")
print("Retrieving", url)
urlRead = urllib.request.urlopen(url).read().decode()
print("Retrieved", len(urlRead), "characters")
urlTree = ET.fromstring(urlRead)

countTag = urlTree.findall(".//count")
numList = [int(num.text) for num in countTag]

print("Count:", len(numList))
print("Sum:", sum(numList))
