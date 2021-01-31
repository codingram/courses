"""
Part B: Saving, with a raise
Background
​
In Part A, we unrealistically assumed that your salary didn’t change.
But you are an MIT graduate, and clearly you are going to be worth more
to your company over time! So we are going to build on your solution to
Part A by factoring in a raise every six months.

In ​ps1b.py​, copy your solution to Part A (as we are going to
reuse much of that machinery). Modify your program to include the following:

1. Have the user input a semi-annual salary raise ​semi_annual_raise​
(as a decimal percentage)
2. After the 6t​h​ month, increase your salary by that percentage.
Do the same after the 12th​ month, the 18​ month, and so on.

Write a program to calculate how many months it will take you save up enough
money for a down payment. LIke before, assume that your investments earn a return
of ​r​ = 0.04 (or 4%) and the required down payment percentage is
0.25 (or 25%). Have the user enter the following variables:

1. The starting annual salary (annual_salary)
2. The percentage of salary to be saved (portion_saved)
3. The cost of your dream home (total_cost)
4. The semi­annual salary raise (semi_annual_raise)

"""

annual_salary = int(input("Enter your starting annual salary: "))
portion_saved = float(input("Enter portion of salary to be saved: "))
total_cost = int(input("Enter the total cost of your dream home: "))
semi_annual_raise = float(input("Enter the semi annual raise: "))

monthly_salary = annual_salary / 12
portion_down_payment = 0.25

down_payment_cost = total_cost * portion_down_payment

current_savings = 0

month = 0
r = 0.04

while current_savings <= down_payment_cost:
    month += 1
    monthly_savings = monthly_salary * portion_saved
    current_savings += current_savings * r / 12
    current_savings += monthly_savings
    if month % 6 == 0:
        monthly_salary += monthly_salary * semi_annual_raise


print("Number of months:", month)
