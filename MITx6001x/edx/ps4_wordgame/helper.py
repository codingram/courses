#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import random
import string

WORDLIST_FILENAME = "words.txt"
VOWELS = "aeiou"
CONSONANTS = "bcdfghjklmnpqrstvwxyz"
HAND_SIZE = 9

SCRABBLE_LETTER_VALUES = {
    "a": 1,
    "b": 3,
    "c": 3,
    "d": 2,
    "e": 1,
    "f": 4,
    "g": 2,
    "h": 4,
    "i": 1,
    "j": 8,
    "k": 5,
    "l": 1,
    "m": 3,
    "n": 1,
    "o": 1,
    "p": 3,
    "q": 10,
    "r": 1,
    "s": 1,
    "t": 1,
    "u": 1,
    "v": 4,
    "w": 4,
    "x": 8,
    "y": 4,
    "z": 10,
}


def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.

    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print()
    print("Loading word list from file...")

    with open(WORDLIST_FILENAME) as inFile:
        wordList = []
        for line in inFile:
            wordList.append(line.strip().lower())

    print(len(wordList), "words loaded.")
    return wordList


def getFrequencyDict(sequence):
    """
    Returns a dictionary where the keys are elements of the sequence
    and the values are integer counts, for the number of times that
    an element is repeated in the sequence.

    sequence: string or list
    return: dictionary (str -> int)
    """
    freq = {}
    for x in sequence:
        freq[x] = freq.get(x, 0) + 1
    return freq


def getWordScore(word, n):
    """
    Returns the score for a word. Assumes the word is a valid word.

    The score for a word is the sum of the points for letters in the
    word, multiplied by the length of the word, PLUS 50 points if all n
    letters are used on the first turn.
    (see SCRABBLE_LETTER_VALUES)

    word: string (lowercase letters)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)
    returns: int >= 0
    """
    bonusPoints = 50 if len(word) == n else 0
    letterPoints = sum([SCRABBLE_LETTER_VALUES[letter] for letter in word]) * len(word)
    totalPoints = letterPoints + bonusPoints
    return totalPoints


def displayHand(hand):
    """
    Returns the string in a displayable format.
    The order of the letters is unimportant.

    hand: dictionary (string -> int)
    """
    letterString = ""
    for letter in hand.keys():
        for j in range(hand[letter]):
            letterString += letter + " "
    return letterString


def dealHand(n):
    """
    Returns a random hand containing n lowercase letters.
    At least n//3 the letters in the hand should be VOWELS.

    Hands are represented as dictionaries. The keys are
    letters and the values are the number of times the
    particular letter is repeated in that hand.

    n: int >= 0
    returns: dictionary (string -> int)
    """
    hand = {}
    numVowels = n // 3

    for i in range(numVowels):
        x = VOWELS[random.randrange(0, len(VOWELS))]
        hand[x] = hand.get(x, 0) + 1

    for i in range(numVowels, n):
        x = CONSONANTS[random.randrange(0, len(CONSONANTS))]
        hand[x] = hand.get(x, 0) + 1

    return hand


def updateHand(hand, word):
    """
    Assumes that 'hand' has all the letters in word.
    In other words, this assumes that however many times
    a letter appears in 'word', 'hand' has at least as
    many of that letter in it.

    Updates the hand: uses up the letters in the given word
    and returns the new hand, without those letters in it.

    Has no side effects: does not modify hand.

    word: string
    hand: dictionary (string -> int)
    returns: dictionary (string -> int)
    """
    newHand = dict(hand)
    for letter in word:
        if newHand[letter] > 0:
            newHand[letter] -= 1
    return newHand


def isValidWord(word, hand, wordList):
    """
    Returns True if word is in the wordList and is entirely
    composed of letters in the hand. Otherwise, returns False.

    Does not mutate hand or wordList.

    word: string
    hand: dictionary (string -> int)
    wordList: list of lowercase strings
    """
    for letter in word:
        if letter not in hand:
            return False
        elif word.count(letter) > hand[letter]:
            return False

    return True if word in wordList else False


def calculateHandlen(hand):
    """
    Returns the length (number of letters) in the current hand.

    hand: dictionary (string-> int)
    returns: integer
    """
    return sum([num for num in hand.values()])


def playHand(hand, wordList, n):
    """
    Allows the user to play the given hand, as follows:

    * The hand is displayed.
    * The user may input a word or a single period (the string ".")
      to indicate they're done playing
    * Invalid words are rejected, and a message is displayed asking
      the user to choose another word until they enter a valid word or "."
    * When a valid word is entered, it uses up letters from the hand.
    * After every valid word: the score for that word is displayed,
      the remaining letters in the hand are displayed, and the user
      is asked to input another word.
    * The sum of the word scores is displayed when the hand finishes.
    * The hand finishes when there are no more unused letters or the user
      inputs a "."

    hand: dictionary (string -> int)
    wordList: list of lowercase strings
    n: integer (HAND_SIZE; i.e., hand size required for additional points)
    """
    totalScore = 0

    while calculateHandlen(hand) > 0:
        print("Current hand:", displayHand(hand))

        userInput = input(
            'Enter word, or a "." to indicate that you are finished: '
        ).lower()

        if userInput == ".":
            break
        else:
            if not isValidWord(userInput, hand, wordList):
                print("Invalid word, please try again.\n")
                continue
            else:
                inputScore = getWordScore(userInput, n)
                totalScore += inputScore
                print(
                    '"'
                    + userInput
                    + '"'
                    + " earned "
                    + str(inputScore)
                    + " points. Total: "
                    + str(totalScore)
                    + " points\n"
                )

                hand = updateHand(hand, userInput)

    if userInput == ".":
        print()
        print("Game ended. Total score:", totalScore, "points.\n")
    else:
        print("Run out of letters. Total score:", totalScore, "points.\n")


# -------------------------- END OF MODULE -------------------------------
if __name__ == "__main__":

    def playGame(wordList):
        """
        Allow the user to play an arbitrary number of hands.
        """
        hand = None

        while True:
            gameChoice = input(
                "Enter n to deal a new hand, r to replay the last hand, or e to end game: "
            )

            if gameChoice == "e":
                break
            elif gameChoice == "n":
                hand = dealHand(HAND_SIZE)
                playHand(hand, wordList, HAND_SIZE)
            elif gameChoice == "r":
                if hand == None:
                    print(
                        "You have not played a hand yet. Please play a new hand first!\n"
                    )
                    continue
                else:
                    playHand(hand, wordList, HAND_SIZE)
            else:
                print("Invalid command.\n")
                continue

    wordList = loadWords()
    playGame(wordList)
