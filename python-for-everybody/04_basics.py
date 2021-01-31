# Exercise 1:
#
# Write a program which repeatedly reads numbers until the user enters “done”.
# Once “done” is entered, print out the total, count, and average of the numbers.
# If the user enters anything other than a number, detect their mistake using
# try and except and print an error message and skip to the next number.

sum = float(0)
count = 0

print("Enter 'e' to exit the program and 'done' to get the output.")
while True:
    num = input("Enter a number: ")
    if num == "e":
        exit()
    if num == "done":
        break
    try:
        num = int(num)
    except:
        print("Invalid input")
        continue
    sum = sum + num
    count = count + 1

average = sum / count
print("Total:", sum)
print("Count:", count)
print("Average", average)

# Exercise 2:
#
# Write another program that prompts for a list of numbers as above and at the
# end prints out both the maximum and minimum of the numbers instead of the average.

smallest = None
largest = None

print("Enter 'e' to exit the program and 'done' to get the output.")
while True:
    num = input("Enter a number: ")
    if num == "e":
        exit()
    if num == "done":
        break
    try:
        num = int(num)
    except:
        print("Invalid input")
        continue
    if smallest is None:
        smallest = num
    elif num < smallest:
        smallest = num
    if largest is None:
        largest = num
    elif num > largest:
        largest = num

print("Maximum is", largest)
print("Minimum is", smallest)
