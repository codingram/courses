#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul 21 10:15:56 2020

@author: dhruvmanilawala

Problem 2.1 - Paying Debt off in a Year

Write a program to calculate the credit card balance after one year if a person
only pays the minimum monthly payment required by the credit card company each month.

The following variables contain values as described below:

balance - the outstanding balance on the credit card
annualInterestRate - annual interest rate as a decimal
monthlyPaymentRate - minimum monthly payment rate as a decimal

For each month, calculate statements on the monthly payment and remaining balance.
At the end of 12 months, print out the remaining balance. Be sure to print out no
more than two decimal digits of accuracy.
"""

# User input variables
balance = int(input("Enter the outstanding balance on the credit card: "))
annualInterestRate = float(input("Enter the annual interest rate as a decimal: "))
monthlyPaymentRate = float(
    input("Enter the minimum monthly payment rate as a decimal: ")
)

# Initialization
monthlyInterestRate = annualInterestRate / 12.0


def remaining_balance(balance, monthlyPaymentRate, monthlyInterestRate):
    forBalance = balance
    for month in range(1, 13):
        minPayment = forBalance * monthlyPaymentRate
        unpaidBalance = forBalance - minPayment
        interest = unpaidBalance * monthlyInterestRate
        forBalance = round(unpaidBalance + interest, 2)
    return forBalance


remBalance = remaining_balance(balance, monthlyPaymentRate, monthlyInterestRate)

print("Remaining balance:", remBalance)
