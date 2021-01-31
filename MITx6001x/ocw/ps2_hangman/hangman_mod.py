# Hangman Game
# -----------------------------------------#
import random
import string

WORDLIST_FILENAME = "/Users/dhruvmanilawala/cs/py/mit/ocw/ps2_hangman/words.txt"

# -----------------------------------------#


def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.

    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print("Loading word list from file...")
    inFile = open(WORDLIST_FILENAME, "r")
    line = inFile.readline()
    wordList = line.split()
    print(len(wordList), "words loaded.")
    return wordList


# -----------------------------------------#


def chooseWord(wordList):
    """
    wordlist (list): list of words (strings)

    Returns a word from wordlist at random
    """
    return random.choice(wordList)


# -----------------------------------------#


def isWordGuessed(secretWord, lettersGuessed):
    """
    secret_word: string, the word the user is guessing; assumes all letters are
      lowercase
    letters_guessed: list (of letters), which letters have been guessed so far;
      assumes that all letters are lowercase
    returns: boolean, True if all the letters of secret_word are in letters_guessed;
      False otherwise
    """
    for letter in secretWord:
        found = True if letter in lettersGuessed else False
        if found == False:
            break
    return found


# -----------------------------------------#


def getGuessedWord(secretWord, lettersGuessed):
    """
    secret_word: string, the word the user is guessing
    letters_guessed: list (of letters), which letters have been guessed so far
    returns: string, comprised of letters, underscores (_), and spaces that represents
      which letters in secret_word have been guessed so far.
    """
    word = ""
    for letter in secretWord:
        if letter in lettersGuessed:
            word += letter
        else:
            word += "_ "
    return word


# -----------------------------------------#


def getAvailableLetters(lettersGuessed):
    """
    lettersGuessed: list (of letters), which letters have been guessed so far
    returns: string (of letters), comprised of letters that represents which letters have not yet been guessed.
    """
    letterList = list(string.ascii_lowercase)
    for letter in lettersGuessed:
        letterList.remove(letter)
    letterString = "".join(letterList)
    return letterString


# -----------------------------------------#


def warningsCheck(secretWord, lettersGuessed, userLetter, numWarnings, numGuess):
    """
    secretWord: string, the secret word to guess
    lettersGussed: list (of letters), which letters have been guessed so far
    userLetter: string, user input letter
    numWarnings: integer, number of warnings remaining
    numGuess: integer, number of guesses remaining

    returns: prints appropriate message when user input is not an alphabet and also when the input is already guessed. Makes changes to the number of guesses and warnings and returns them as a tuple: (numGuess, numWarnings)
    """
    guessedWord = getGuessedWord(secretWord, lettersGuessed)
    if len(userLetter) > 1 or userLetter in inputCheck:
        if numWarnings == 0:
            print(
                "Oops! That is not a valid letter. You have no warnings left so you lose one guess:",
                guessedWord,
            )
            numGuess -= 1
        else:
            numWarnings -= 1
            print(
                "Oops! That is not a valid letter. You have",
                numWarnings,
                "warnings left:",
                guessedWord,
            )

    if userLetter in lettersGuessed:
        if numWarnings == 0:
            print(
                "Oops! You've already guessed that letter. You have no warnings left so you lose one guess:",
                guessedWord,
            )
            numGuess -= 1
        else:
            numWarnings -= 1
            print(
                "Oops! You've already guessed that letter. You have",
                numWarnings,
                "warnings left:",
                guessedWord,
            )

    return (numGuess, numWarnings)


# -----------------------------------------#


def hangman(secretWord):
    """
    secretWord: string, the secret word to guess.

    Starts up an interactive game of Hangman.

    """
    lettersGuessed = []
    numGuess = 6
    numWarnings = 3

    print("Welcome to the game, Hangman!")
    print("I am thinking of a word that is", len(secretWord), "letters long.")
    print("You have", numWarnings, "warnings left.")

    while numGuess > 0:
        guessedWord = getGuessedWord(secretWord, lettersGuessed)
        print("-" * 20)
        print("You have", numGuess, "guesses left.")

        availableLetters = getAvailableLetters(lettersGuessed)
        print("Available letters:", availableLetters)

        userLetter = input("Please guess a letter: ").lower()

        if len(userLetter) > 1 or userLetter in inputCheck:
            numGuess, numWarnings = warningsCheck(
                secretWord, lettersGuessed, userLetter, numWarnings, numGuess
            )
            continue

        elif userLetter in lettersGuessed:
            numGuess, numWarnings = warningsCheck(
                secretWord, lettersGuessed, userLetter, numWarnings, numGuess
            )

        elif userLetter not in secretWord:
            print("Oops! That letter is not in my word:", guessedWord)
            lettersGuessed.append(userLetter)
            numGuess = numGuess - 2 if userLetter in stringVowels else numGuess - 1

        elif userLetter in secretWord:
            lettersGuessed.append(userLetter)
            guessedWord = getGuessedWord(secretWord, lettersGuessed)
            print("Good guess:", guessedWord)

            if isWordGuessed(secretWord, lettersGuessed):
                totalScore = numGuess * len(set(secretWord))
                print("-" * 20)
                print("Congratulations, you won!")
                print("Your total score for this game is:", totalScore)
                break

    if numGuess == 0:
        print("-" * 20)
        print("Sorry, you ran out of guesses. The word was " + secretWord + ".")


# --------------- Hangman with hints --------------------#


def arrangeWords(wordList):
    """
    wordList  (list): list of words (strings)

    Returns a dictionary;
    keys, number of letters;
    values, list of words with same number of letters
    """
    wordDict = {}

    for word in wordList:
        if len(word) not in wordDict:
            wordDict[len(word)] = [word]
        else:
            wordDict[len(word)].append(word)

    return wordDict


# -----------------------------------------#


def matchWithGaps(myWordString, otherWord):
    """
    myWordString: string without _ characters, current guess of secret word
    otherWord: string, regular English word
    Assuming myWordString and otherWord are of the same length

    returns: boolean, True if all the actual letters of myWordString match the corresponding letters of otherWord, or the letter is the special symbol _ ; False otherwise
    """

    for i in range(len(myWordString)):
        if myWordString[i] == "_" or myWordString[i] == otherWord[i]:
            continue
        else:
            return False
    return True


# -----------------------------------------#


def showPossibleMatches(myWord, wordLength):
    """
    myWord: string with _ characters, current guess of secret word
    wordLength: integer, length of the secretWord
    returns: nothing, but prints out every word in wordlist that matches myWord

    Keep in mind that in hangman when a letter is guessed, all the positions at which that letter occurs in the secret word are revealed. Therefore, the hidden letter(_ ) cannot be one of the letters in the word that has already been revealed.

    """
    wordMatches = ""
    myWordString = "".join([letter for letter in myWord if letter != " "])

    for word in wordDict[wordLength]:
        if matchWithGaps(myWordString, word):
            wordMatches += word + " "

    print("Possible word matches are:\n" + wordMatches)


# -----------------------------------------#


def hangmanWithHints(secretWord):
    """
    secret_word: string, the secret word to guess.

    Starts up an interactive game of Hangman.

    """
    lettersGuessed = []
    numGuess = 6
    numWarnings = 3

    print("Welcome to the game, Hangman!")
    print("I am thinking of a word that is", len(secretWord), "letters long.")
    print("You have", numWarnings, "warnings left.")

    while numGuess > 0:
        guessedWord = getGuessedWord(secretWord, lettersGuessed)
        print("-" * 20)
        print("You have", numGuess, "guesses left.")

        availableLetters = getAvailableLetters(lettersGuessed)
        print("Available letters:", availableLetters)

        userLetter = input("Please guess a letter: ").lower()

        if len(userLetter) > 1 or userLetter in inputCheck:
            if userLetter == "*":
                wordLength = len(secretWord)
                showPossibleMatches(guessedWord, wordLength)
                continue

            numGuess, numWarnings = warningsCheck(
                secretWord, lettersGuessed, userLetter, numWarnings, numGuess
            )
            continue

        elif userLetter in lettersGuessed:
            numGuess, numWarnings = warningsCheck(
                secretWord, lettersGuessed, userLetter, numWarnings, numGuess
            )

        elif userLetter not in secretWord:
            print("Oops! That letter is not in my word:", guessedWord)
            lettersGuessed.append(userLetter)
            numGuess = numGuess - 2 if userLetter in stringVowels else numGuess - 1

        elif userLetter in secretWord:
            lettersGuessed.append(userLetter)
            guessedWord = getGuessedWord(secretWord, lettersGuessed)
            print("Good guess:", guessedWord)

            if isWordGuessed(secretWord, lettersGuessed):
                gameScore = numGuess * len(set(secretWord))
                print("-" * 20)
                print("Congratulations, you won!")
                print("Your total score for this game is:", gameScore)
                break

    if numGuess == 0:
        print("-" * 20)
        print("Sorry, you ran out of guesses. The word was " + secretWord + ".")


# ------------------- Initiating global variables ----------------------#

# wordlist, list of words from the file
# wordDict, dictionary with
# inputCheck, string of punctuation, digits and whitespace
# stringVowels, string of vowels

wordList = loadWords()
wordDict = arrangeWords(wordList)
inputCheck = string.punctuation + string.digits + string.whitespace
stringVowels = "aeiou"


# ------------------- Initiating the game ----------------------#

if __name__ == "__main__":

    # secretWord = chooseWord(wordList)
    # hangman(secretWord)

    secretWord = chooseWord(wordList)
    hangmanWithHints(secretWord)


# -----------------------------------------#
