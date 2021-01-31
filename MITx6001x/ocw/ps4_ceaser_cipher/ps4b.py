import string

WORDLIST_FILENAME = "words.txt"


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


class Message(object):
    def __init__(self, text):
        """
        Initializes a Message object
        text (string): the message's text

        a Message object has two attributes:
            self.message_text (string, determined by input text)
            self.valid_words (list, determined using helper function load_words
        """
        self._message_text = text

    @property
    def message_text(self):
        return self._message_text

    @staticmethod
    def _build_shift_dict(shift):
        """
        Creates a dictionary that can be used to apply a cipher to a letter.
        The dictionary maps every uppercase and lowercase letter to a
        character shifted down the alphabet by the input shift.

        shift (integer): the amount by which to shift every letter of the
        alphabet. 0 <= shift < 26

        Returns: a dictionary mapping a letter (string) to
                 another letter (string).

        Oneline using dict comp:
        {chr(j+i): chr(j+(i+shift)%26) for j in (65, 97) for i in range(26)}
        """
        assert 0 <= shift < 26, "Shift cannot be longer than 26."
        shift_dict = {}
        letter_list = [string.ascii_lowercase, string.ascii_uppercase]
        for i in letter_list:
            for index in range(shift, shift + 26):
                shift_dict[i[index - shift]] = i[index] if index < 26 else i[index % 26]
        return shift_dict

    def _apply_shift(self, shift):
        """
        Applies the Caesar Cipher to self.message_text with the input shift.
        Creates a new string that is self.message_text shifted down the
        alphabet by some number of characters determined by the input shift

        shift (integer): the shift with which to encrypt the message.
        0 <= shift < 26

        Returns: the message text (string) in which every character
                 is shifted down the alphabet by the input shift
        """
        shift_dict = self._build_shift_dict(shift)
        trans_table = str.maketrans(shift_dict)
        shifted_text = self.message_text.translate(trans_table)
        return shifted_text


class PlaintextMessage(Message):
    def __init__(self, text, shift):
        """
        Initializes a PlaintextMessage object
        text (string): the message's text
        shift (integer): the shift associated with this message
        """
        super().__init__(text)
        self._shift = shift
        self._encrypting_dict = self._build_shift_dict(shift)
        self._encrypted_message = self._apply_shift(shift)

    @property
    def shift(self):
        return self._shift

    @property
    def encrypting_dict(self):
        return self._encrypting_dict.copy()

    @property
    def encrypted_message(self):
        return self._encrypted_message

    def change_shift(self, shift):
        """
        Updates: self.shift, self.encrypting_dict, self.message_text_encrypted
        shift (integer): new shift that should be associated
                         with this message.
        0 <= shift < 26
        Returns: None
        """
        self._shift = shift
        self._encrypting_dict = self._build_shift_dict(shift)
        self._encrypted_message = self._apply_shift(shift)


class CiphertextMessage(Message):
    def __init__(self, text):
        """
        Initializes a PlaintextMessage object
        text (string): encrypted message
        """
        super().__init__(text)
        self._valid_words = load_words(WORDLIST_FILENAME)

    @property
    def valid_words(self):
        return self._valid_words[:]

    def decrypt_message(self):
        """
        Returns: a tuple of the best shift value used to decrypt the message
        and the decrypted message text using that shift value
        Created a dictionary for debugging: possible_decryption
        """
        valid_words = self.valid_words
        possible_decryption = {}

        for shift in range(1, 25):
            true_count = 0
            decrypted_text = self._apply_shift(shift)
            decrypted_text_list = decrypted_text.split(" ")
            for word in decrypted_text_list:
                if is_word(valid_words, word):
                    true_count += 1
            possible_decryption[(true_count, shift)] = decrypted_text

        max_true_count, best_shift = max(possible_decryption)
        best_encrypted = possible_decryption[(max_true_count, best_shift)]

        return best_shift, best_encrypted


def decrypt_story():
    with open("story.txt", "r") as f:
        story_string = str(f.read())
        story = CiphertextMessage(story_string)
        return story.decrypt_message()


def test():
    TESTS = [
        ("Hello world", 2, "Jgnnq yqtnf"),
        ("ThiS is A TeST.", 24, "RfgQ gq Y RcQR."),
        (
            "If he had anything confidential to say, he wrote it in cipher, "
            "that is, by so changing the order of the letters of the alphabet, "
            "that not a word could be made out.",
            11,
            "Tq sp slo lyjestyr nzyqtopyetlw ez dlj, sp hczep te ty ntaspc, "
            "esle td, mj dz nslyrtyr esp zcopc zq esp wpeepcd zq esp lwaslmpe, "
            "esle yze l hzco nzfwo mp xlop zfe.",
        ),
    ]

    for t in TESTS:
        msg, shift, encrypted = t
        plaintext = PlaintextMessage(msg, shift)
        ciphertext = CiphertextMessage(encrypted)
        output = plaintext.encrypted_message
        try:
            assert output == encrypted
            print(f"PASSED: {output}")
            print(f"Decrypted: {ciphertext.decrypt_message()}\n")
        except AssertionError:
            print(f"FAILED: Expected: {encrypted} but got {output}")


if __name__ == "__main__":
    test()
