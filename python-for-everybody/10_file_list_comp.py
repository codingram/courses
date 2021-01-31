# Exercise 2:
#
# Write a program to read through the mbox-short.txt and figure out the distribution
# by hour of the day for each of the messages. You can pull the hour out from the
# 'From ' line by finding the time and then splitting the string a second time using
# a colon.
#
#   From stephen.marquard@uct.ac.za Sat Jan  5 09:14:16 2008
#
# Once you have accumulated the counts for each hour, print out the counts, sorted by
# hour as shown below.

while True:
    try:
        fname = input("Enter file name: ")
        fh = open(fname)
        break
    except IOError as err:
        print(err)

hourList = []
for line in fh:
    line = line.rstrip()
    if not line.startswith("From "):
        continue
    hour = line.find(":")
    hourList.append(line[hour - 2 : hour])

hourDict = {}
for hr in hourList:
    hourDict[hr] = hourDict.get(hr, 0) + 1

# List comprehension: Assign it to hourSort = sort this list ([Make a list of tuples
# (hour,count) for hour,count in the dictionary hourDict items])
hourSort = sorted([(hour, count) for hour, count in hourDict.items()])

# List comprehension does all the below four lines of code in one.
# hourSort = []
# for hour,count in hourDict.items():
#     hourSort.append((hour,count))
# hourSort.sort()

for hour, count in hourSort:
    print(hour, count)
