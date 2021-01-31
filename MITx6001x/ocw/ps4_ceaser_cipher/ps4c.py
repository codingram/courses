import string
from .permutation import permutations

WORDLIST_FILENAME = "words.txt"

VOWELS_LOWER = "aeiou"
VOWELS_UPPER = "AEIOU"
CONSONANTS_LOWER = "bcdfghjklmnpqrstvwxyz"
CONSONANTS_UPPER = "BCDFGHJKLMNPQRSTVWXYZ"


def load_words(file_name):
    """
    file_name (string): the name of the file containing
    the list of words to load
    Returns: a list of valid words.
    Words are strings of lowercase letters.
    """
    with open(file_name, "r") as file_hand:
        line = file_hand.readline()
        word_list = line.split()

    return word_list


def is_word(word_list, word):
    """
    Determines if word is a valid word, ignoring
    capitalization and punctuation

    word_list (list): list of words in the dictionary.
    word (string): a possible word.

    Returns: True if word is in word_list, False otherwise
    """
    word = word.lower()
    word = word.strip(string.punctuation)
    return word in word_list


class SubMessage(object):
    def __init__(self, text):
        """
        Initializes a SubMessage object
        text (string): the message's text
        """
        self._message_text = text

    @property
    def message_text(self):
        return self._message_text

    @staticmethod
    def build_transpose_dict(vowels_permutation):
        """
        vowels_permutation (string): a string containing a permutation of
        vowels (a, e, i, o, u)
        Returns: a dictionary mapping a letter (string) to
                 another letter (string).
        """
        sub_dict = dict(
            zip(
                VOWELS_LOWER + VOWELS_UPPER + CONSONANTS_LOWER + CONSONANTS_UPPER,
                vowels_permutation
                + vowels_permutation.upper()
                + CONSONANTS_LOWER
                + CONSONANTS_UPPER,
            )
        )
        return sub_dict

    def apply_transpose(self, transpose_dict):
        """
        transpose_dict (dict): a transpose dictionary
        Returns: an encrypted version of the message text, based
        on the dictionary
        """
        trans_table = str.maketrans(transpose_dict)
        transposed_text = self.message_text.translate(trans_table)
        return transposed_text


class EncryptedSubMessage(SubMessage):
    def __init__(self, text):
        """
        Initializes an EncryptedSubMessage object
        text (string): the encrypted message text
        """
        super().__init__(text)
        self._valid_words = load_words(WORDLIST_FILENAME)

    @property
    def valid_words(self):
        return self._valid_words

    def decrypt_message(self):
        """
        Attempt to decrypt the encrypted message
        Returns: the best decrypted message
        """
        all_perm = permutations("aeiou")
        valid_words = self.valid_words
        possible_decryption = {}

        for perms in all_perm:
            true_count = 0
            dec_dict = self.build_transpose_dict(perms)
            decrypted_text = self.apply_transpose(dec_dict)
            decrypted_text_list = decrypted_text.split(" ")
            for word in decrypted_text_list:
                if is_word(valid_words, word):
                    true_count += 1
            possible_decryption[(true_count, perms)] = decrypted_text

        max_true_count, best_perm = max(possible_decryption)
        best_encrypted = possible_decryption[(max_true_count, best_perm)]

        return best_encrypted


if __name__ == "__main__":

    # Example test case
    message = SubMessage("Hello World!")
    vowel_perm = "eaiuo"
    enc_dict = message.build_transpose_dict(vowel_perm)
    print("Original message:", message.message_text, "Permutation:", vowel_perm)
    print("Expected encryption:", "Hallu Wurld!")
    print("Actual encryption:", message.apply_transpose(enc_dict))

    enc_message = EncryptedSubMessage(message.apply_transpose(enc_dict))
    print("Decrypted message:", enc_message.decrypt_message())
