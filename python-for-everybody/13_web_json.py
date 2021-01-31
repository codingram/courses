# Extracting Data from JSON
# In this assignment you will write a Python program somewhat similar to
# http://www.py4e.com/code3/json2.py. The program will prompt for a URL,
# read the JSON data from that URL using urllib and then parse and extract
# the comment counts from the JSON data, compute the sum of the numbers in
# the file and enter the sum below:

# Sample data: http://py4e-data.dr-chuck.net/comments_42.json (Sum=2553)
# Actual data: http://py4e-data.dr-chuck.net/comments_754937.json (Sum=2192)

import urllib.request, urllib.error, urllib.parse
import json

url = input("Enter location: ")
print("Retrieving", url)

urlRead = urllib.request.urlopen(url).read().decode()
print("Retrieved", len(urlRead), "characters")

urlTree = json.loads(urlRead)

numList = [int(nameDict["count"]) for nameDict in urlTree["comments"]]

print("Count:", len(numList))
print("Sum:", sum(numList))
