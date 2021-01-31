# Rewrite the rate program from the previous chapter using a function called
# computepay which takes two parameters (hours and rate).


def computepay(h, r):
    if h <= 40:
        pay = h * r
    else:
        eh = h - 40
        pay = (eh * r * 1.5) + (40 * r)
    return pay


print("Enter 'e' to exit the program.")
while True:
    try:
        h = float(input("Enter Hours: "))
        r = float(input("Enter rate: "))
        if h == "e" or r == "e":
            exit()
        break
    except:
        print("Error, not a number.")
        continue

p = computepay(h, r)
print("Pay", p, "$")


# Rewrite the grade program from the previous chapter using a function called
# computegrade that takes a score as its parameter and returns a grade as a string.


def computegrade(score):
    if score > 1.0:
        return "Error, score out of range"
    elif score >= 0.9:
        return "A"
    elif score >= 0.8:
        return "B"
    elif score >= 0.7:
        return "C"
    elif score >= 0.6:
        return "D"
    elif 0.0 <= score < 0.6:
        return "F"
    else:
        return "Error, score cannot be negative"


print("Enter 'e' to exit the program.")
while True:
    try:
        score = float(input("Enter score : "))
        if score == "e":
            exit()
        break
    except:
        print("Error, not a number.")

grade = computegrade(score)
print("Grade", grade)
