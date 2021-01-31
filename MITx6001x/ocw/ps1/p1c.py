"""
Part C: Finding the right amount to save away
​
In Part B, you had a chance to explore how both the percentage of your salary that
you save each month and your annual raise affect how long it takes you to save for
a down payment. This is nice, but suppose you want to set a particular goal,
e.g. to be able to afford the down payment in three years. How much should you
save each month to achieve this? In this problem, you are going to write a program
to answer that question. To simplify things, assume:

1. Your semi­annual raise is .07 (7%)
2. Your investments have an annual return of 0.04 (4%)
3. The down payment is 0.25 (25%) of the cost of the house
4. The cost of the house that you are saving for is $1M.

You are now going to try to find the best rate of savings to achieve a down payment
on a $1M house in 36 months. Since hitting this exactly is a challenge, we simply want
your savings to be within $100 of the required down payment.

In​ ps1c.py​, write a program to calculate the best savings rate, as a
function of your starting salary. You should use ​bisection search​ to help
you do this efficiently. You should keep track of the number of steps it takes your
bisections search to finish. You should be able to reuse some of the code you wrote
for part B in this problem.

Because we are searching for a value that is in principle a float, we are going to limit
ourselves to two decimals of accuracy (i.e., we may want to save at 7.04% ­­ or 0.0704
in decimal – but we are not going to worry about the difference between 7.041% and
7.039%). This means we can search for an integer​ between 0 and 10000
(using integer division), and then convert it to a decimal percentage
(using float division) to use when we are calculating the
​current_savings​ after 36 months. By using this range, there are
only a finite number of numbers that we are searching over, as opposed to the
infinite number of decimals between 0 and 1. This range will help prevent infinite
loops. The reason we use 0 to 10000 is to account for two additional decimal places
in the range 0% to 100%. Your code should print out a decimal (e.g. 0.0704 for 7.04%).

Try different inputs for your starting salary, and see how the percentage you need
to save changes to reach your desired down payment. Also keep in mind it may not be
possible for to save a down payment in a year and a half for some salaries. In this
case your function should notify the user that it is not possible to save for the down
payment in 36 months with a print statement. ​Please make your program print
results in the format shown in the test cases below.


Test Case 1
>>>
Enter the starting salary:​ 150000
Best savings rate:​ 0.4411
Steps in bisection search:​ 12
>>>

Test Case 2
>>>
Enter the starting salary:​ 300000
Best savings rate:​ 0.2206
Steps in bisection search:​ 9
>>>

Test Case 3
>>>
Enter the starting salary:​ 10000
It is not possible to pay the down payment in three years.
>>>

"""

annual_salary = float(input("Enter the starting salary: "))


total_cost = 1000000
down_payment = 0.25 * total_cost
semi_annual_raise = 0.07
annual_return = r = 0.04


low = 0
high = 10000
savings_rate = (low + high) // 2
current_savings = 0
bisection_steps = 0

while abs(current_savings - down_payment) >= 100:
    current_savings = 0
    rate = savings_rate / 10000
    monthly_salary = annual_salary / 12

    for month in range(1, 37):
        current_savings += monthly_salary * rate + current_savings * r / 12
        if month % 6 == 0:
            monthly_salary += monthly_salary * semi_annual_raise

    if current_savings > down_payment - 100:
        high = savings_rate
    else:
        low = savings_rate
    savings_rate = (high + low) // 2
    bisection_steps += 1

    if bisection_steps > 13:
        break

if bisection_steps > 13:
    print("It is not possible to pay the down payment in three years.")
else:
    print("Best savings rate:", rate)
    print("Steps in bisection search:", bisection_steps)
