#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import helper
from helper import HAND_SIZE
import time


def compChooseWord(hand, wordList, n):
    """
    Given a hand and a wordList, find the word that gives
    the maximum value score, and return it.
    If no words in the wordList can be made from the hand, return None.

    hand: dictionary (string -> int)
    wordList: list (string)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)

    returns: string or None
    """
    bestScore = 0
    bestWord = None

    for word in wordList:
        if helper.isValidWord(word, hand, wordList):
            score = helper.getWordScore(word, n)
            if score > bestScore:
                bestScore = score
                bestWord = word

    return bestWord


def compPlayHand(hand, wordList, n):
    """
    Allows the computer to play the given hand, following the same procedure
    as playHand, except instead of the user choosing a word, the computer
    chooses it.

    hand: dictionary (string -> int)
    wordList: list (string)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)
    """
    totalScore = 0

    while helper.calculateHandlen(hand) > 0:
        print("Current hand:", helper.displayHand(hand))
        word = compChooseWord(hand, wordList, n)

        if word is None:
            break
        else:
            if not helper.isValidWord(word, hand, wordList):
                print("This is a terrible error! I need to check my own code!")
                break
            else:
                score = helper.getWordScore(word, n)
                totalScore += score
                print(
                    '"'
                    + word
                    + '" earned '
                    + str(score)
                    + " points. Total: "
                    + str(totalScore)
                    + " points"
                )
                hand = helper.updateHand(hand, word)
                print()

    print("Total score: " + str(totalScore) + " points.")


def playGame(wordList):
    """
    Allow the user to play an arbitrary number of hands.

    1) Asks the user to input 'n' or 'r' or 'e'.
        * If the user inputs 'e', immediately exit the game.
        * If the user inputs anything that's not 'n', 'r', or 'e', keep asking them again.

    2) Asks the user to input a 'u' or a 'c'.
        * If the user inputs anything that's not 'c' or 'u', keep asking them again.

    3) Switch functionality based on the above choices:
        * If the user inputted 'n', play a new (random) hand.
        * Else, if the user inputted 'r', play the last hand again.

        * If the user inputted 'u', let the user play the game
          with the selected hand, using playHand.
        * If the user inputted 'c', let the computer play the
          game with the selected hand, using compPlayHand.

    4) After the computer or user has played the hand, repeat from step 1

    wordList: list (string)
    """
    hand = None

    while True:
        gameChoice = input(
            "Enter 'n' to deal a new hand, 'r' to replay the last hand, or 'e' to end game: "
        )

        if gameChoice == "e":
            break
        elif hand is None and gameChoice == "r":
            print("You have not played a hand yet. Please play a new hand first!\n")
            continue
        elif gameChoice not in "ern":
            print("Invalid command.\n")
            continue

        while True:
            playerChoice = input(
                "Enter 'u' to have yourself play, 'c' to have the computer play: "
            )

            if playerChoice == "u":
                if gameChoice == "n":
                    hand = helper.dealHand(HAND_SIZE)
                    helper.playHand(hand, wordList, HAND_SIZE)
                else:
                    helper.playHand(hand, wordList, HAND_SIZE)
                break

            elif playerChoice == "c":
                if gameChoice == "n":
                    hand = helper.dealHand(HAND_SIZE)
                    compPlayHand(hand, wordList, HAND_SIZE)
                else:
                    compPlayHand(hand, wordList, HAND_SIZE)
                break
            else:
                print("Invalid command.\n")
                continue


if __name__ == "__main__":
    wordList = helper.loadWords()
    playGame(wordList)
