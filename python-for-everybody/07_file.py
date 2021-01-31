# Exercise 1:
#
# Write a program to read through a file and print the contents of the file
# (line by line) all in upper case.

fname = input("Enter the file name: ")

try:
    fhand = open(fname)
except:
    print("Error, file doesn't exist or file name is mispelled.")
    exit()

count = 0
for line in fhand:
    line = line.rstrip()
    line = line.upper()
    count = count + 1
    print(line)
print("Count:", count)
