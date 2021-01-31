# Exercise 2:
#
# Write a program that uses input to prompt a user for their name and
# then welcomes them.

name = input("Please enter your name: ")
print("Hello", name)

# Exercise 3:
#
# Write a program to prompt the user for hours and rate per hour to compute
# gross pay.

hrs = float(input("Enter hours: "))
rate = float(input("Enter rate: "))

# At that time I didn't realize I could directly float into the input.
# EDIT: I probably meant 'I could directly cast the input into float' but
# well thats how beginners think ;)

pay = hrs * rate
print("Pay: ", pay)
