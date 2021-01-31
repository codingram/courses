# Exercise 2:
#
# Rewrite your pay computation to give the employee 1.5 times the hourly rate
# for hours worked above 40 hours and using try and except so that your program
# handles non-numeric input gracefully by printing a message and exiting the program.

while True:
    try:
        h = float(input("Enter hours: "))
        r = float(input("Enter rate: "))
        break
    except:
        print("Error, please enter numeric input")

if h <= 40:
    pay = h * r
else:
    eh = h - 40
    ep = r * 1.5 * eh
    pay = (40 * r) + ep
print(pay)


# Exercise 3:
#
# Write a program to prompt for a score between 0.0 and 1.0. If the score is out
# of range, print an error message. If the score is between 0.0 and 1.0, print a
# grade using the following table:
#
# To provide grade when the entered value is between 0.0 and 1.0.
# If the value is greater than 1.0, it prints 'Score out of range' and if value
# is less than 0.0 i.e., negative, it will print 'Score cannot be negative'

score = float(input("Enter score [Between 0.0 and 1.0]: "))

if score > 1.0:
    print("Error, score out of range")
elif score >= 0.9:
    print("A")
elif score >= 0.8:
    print("B")
elif score >= 0.7:
    print("C")
elif score >= 0.6:
    print("D")
elif 0.0 <= score < 0.6:
    print("F")
else:
    print("Error, score cannot be negative")
