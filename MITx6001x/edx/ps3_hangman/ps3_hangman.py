# Hangman game
import random
import string

WORDLIST_FILENAME = "/Users/dhruvmanilawala/cs/py/mit/edx/psets/p3_hangman/words.txt"


def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.

    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print("-" * 20)
    print("Loading word list from file...")

    inFile = open(WORDLIST_FILENAME, "r")
    line = inFile.readline()
    wordList = line.split()
    print(len(wordList), "words loaded.")
    return wordList


def chooseWord(wordList):
    """
    wordlist (list): list of words (strings)

    Returns a word from wordlist at random
    """
    return random.choice(wordList)


# Initialization:
# wordlist, list of words from the file
# inputCheck, string of punctuation, digits and whitespace for user input checking
wordList = loadWords()
inputCheck = string.punctuation + string.digits + string.whitespace


def isWordGuessed(secretWord, lettersGuessed):
    """
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: boolean, True if all the letters of secretWord are in lettersGuessed;
      False otherwise
    """
    #  found = (
    #      "".join([letter for letter in secretWord if letter in lettersGuessed])
    #      == secretWord
    #  )
    for letter in secretWord:
        found = True if letter in lettersGuessed else False
        if not found:
            break
    return found


def getGuessedWord(secretWord, lettersGuessed):
    """
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters and underscores that represents
      what letters in secretWord have been guessed so far.
    """
    #  word = "".join(
    #      [letter if letter in lettersGuessed else "_ " for letter in secretWord]
    #  )
    word = ""
    for letter in secretWord:
        word = word + letter if letter in lettersGuessed else word + "_ "
    return word


def getAvailableLetters(lettersGuessed):
    """
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters that represents what letters have not
      yet been guessed.
    """
    letterList = list(string.ascii_lowercase)
    for letter in lettersGuessed:
        letterList.remove(letter)
    letterString = "".join(letterList)
    return letterString


def hangman(secretWord):
    """
    secretWord: string, the secret word to guess.

    Starts up an interactive game of Hangman.
    """
    print("Welcome to the game, Hangman!")
    print("I am thinking of a word that is", len(secretWord), "letters long.")

    lettersGuessed = []
    numGuess = 8

    while numGuess > 0:
        guessedWord = getGuessedWord(secretWord, lettersGuessed)
        print("-" * 20)
        print("You have", numGuess, "guesses left.")

        availableLetters = getAvailableLetters(lettersGuessed)
        print("Available letters:", availableLetters)

        userLetter = input("Please guess a letter: ").lower()

        if len(userLetter) > 1 or userLetter in inputCheck:
            print("Oops! Only one alphabet is allowed.")
            continue

        if userLetter in lettersGuessed:
            print("Oops! You've already guessed that letter:", guessedWord)

        elif userLetter not in secretWord:
            print("Oops! That letter is not in my word:", guessedWord)
            lettersGuessed.append(userLetter)
            numGuess -= 1

        elif userLetter in secretWord:
            lettersGuessed.append(userLetter)
            guessedWord = getGuessedWord(secretWord, lettersGuessed)
            print("Good guess:", guessedWord)

            if isWordGuessed(secretWord, lettersGuessed):
                print("-" * 20)
                print("Congratulations, you won!")
                break

    if numGuess == 0:
        print("-" * 20)
        print("Sorry, you ran out of guesses. The word was " + secretWord + ".")


# Initializing the game
choice = "y"

while choice == "y":
    secretWord = chooseWord(wordList).lower()
    hangman(secretWord)
    choice = input("Do you want to play again? [y/n]: ")
    print("-" * 20)

print("Thank you for playing.")
