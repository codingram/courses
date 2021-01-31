# Exercise 5:
#
# Given:
text = "X-DSPAM-Confidence:    0.8475"

# Extract the number at the end using find() and string slicing and print it as float.

num = text.find(":")  # Finds the string ':'
num = text[num + 1 :]  # Assign strings AFTER ':' till the end in num
num = num.strip()  # Strip the spaces around num string
num = float(num)  # Convert string, which is a number, in floating decimal
print(num)


# Exercise 1:
#
# While loop to print the letters of a word in reverse order.

print("Program to print the letters of a word in reverse order.")
word = input("Enter any word: ")
n = len(word) - 1

while n < len(word):
    letter = word[n]
    print(letter)
    n = n - 1
    if n < 0:
        break

print("There are", len(word), "letters in", word)
