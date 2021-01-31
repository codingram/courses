#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul 21 10:42:07 2020

@author: dhruvmanilawala

Problem 2.2 - Paying Debt Off in a Year

Now write a program that calculates the minimum fixed monthly payment needed in
order pay off a credit card balance within 12 months. By a fixed monthly payment,
we mean a single number which does not change each month, but instead is a constant
amount that will be paid each month.

In this problem, we will not be dealing with a minimum monthly payment rate.

The following variables contain values as described below:

balance - the outstanding balance on the credit card
annualInterestRate - annual interest rate as a decimal

The program should print out one line: the lowest monthly payment that will pay
off all debt in under 1 year.

"""

# User input variables
balance = int(input("Enter the outstanding balance on the credit card: "))
annualInterestRate = float(input("Enter the annual interest rate as a decimal: "))

# Initialization
monthlyInterestRate = annualInterestRate / 12.0
minPayment = 0


def remaining_balance(balance, monthlyInterestRate):
    remBalance = balance
    for month in range(1, 13):
        unpaidBalance = remBalance - minPayment
        interest = unpaidBalance * monthlyInterestRate
        remBalance = unpaidBalance + interest
    return remBalance


remBalance = balance
while remBalance > 0:
    minPayment += 10
    remBalance = remaining_balance(balance, monthlyInterestRate)

print("Lowest payment:", minPayment)
