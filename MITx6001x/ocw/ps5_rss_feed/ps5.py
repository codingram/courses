from abc import ABCMeta, abstractmethod
import feedparser
import time
import threading
from project_util import translate_html
from mtTkinter import *
from datetime import datetime
import pytz
import re

SLEEPTIME = 40  # seconds -- how often we poll


# ======================
# Code for retrieving and parsing
# Google and Yahoo News feeds
# Do not change this code
# ======================


def process(url):
    """
    Fetches news items from the rss url and parses them.
    Returns a list of NewsStory-s.
    """
    feed = feedparser.parse(url)
    entries = feed.entries
    ret = []
    for entry in entries:
        guid = entry.guid
        title = translate_html(entry.title)
        link = entry.link
        description = translate_html(entry.description)
        pubdate = translate_html(entry.published)

        try:
            pubdate = datetime.strptime(pubdate, "%a, %d %b %Y %H:%M:%S %Z")
            pubdate.replace(tzinfo=pytz.timezone("GMT"))
            # pubdate = pubdate.astimezone(pytz.timezone('EST'))
            # pubdate.replace(tzinfo=None)
        except ValueError:
            pubdate = datetime.strptime(pubdate, "%a, %d %b %Y %H:%M:%S %z")

        news_story = NewsStory(guid, title, description, link, pubdate)
        ret.append(news_story)
    return ret


# ======================
# Data structure design
# ======================


class NewsStory(object):

    __slots__ = "_guid", "_title", "_description", "_link", "_pubdate"

    def __init__(self, guid, title, description, link, pubdate) -> None:
        self._guid = guid
        self._title = title
        self._description = description
        self._link = link
        self._pubdate = pubdate

    @property
    def guid(self):
        return self._guid

    @property
    def title(self):
        return self._title

    @property
    def description(self):
        return self._description

    @property
    def link(self):
        return self._link

    @property
    def pubdate(self):
        return self._pubdate


# ======================
# Triggers
# ======================


class Trigger(object, metaclass=ABCMeta):

    __slots__ = ()

    @abstractmethod
    def evaluate(self, story: NewsStory):
        """
        Returns True if an alert should be generated
        for the given news item, or False otherwise.
        """
        pass


class PhraseTrigger(Trigger, metaclass=ABCMeta):

    __slots__ = ("_phrase",)

    def __init__(self, phrase):
        self._phrase = phrase.strip().lower()

    def is_phrase_in(self, text):
        """Returns True if phrase is present in text otherwise False."""
        text_copy = text.lower()
        # Anything except letters and digits are allowed around the phrase words
        # \b is the word boundary, it's a space between the characters
        # including whitespaces
        pattern = "[^a-z0-9]+".join(self._phrase.split())
        pattern = r"\b[^a-z0-9]*" + pattern + r"[^a-z0-9]*\b"
        if re.search(pattern, text_copy):
            return True
        return False


class TitleTrigger(PhraseTrigger):

    __slots__ = ()

    def evaluate(self, story: NewsStory):
        return self.is_phrase_in(story.title)


class DescriptionTrigger(PhraseTrigger):

    __slots__ = ()

    def evaluate(self, story: NewsStory):
        return self.is_phrase_in(story.description)


class TimeTrigger(Trigger, metaclass=ABCMeta):

    __slots__ = ("_datetime",)

    def __init__(self, date_string):
        dt_obj = datetime.strptime(date_string, "%d %b %Y %H:%M:%S")
        self._datetime = dt_obj.replace(tzinfo=pytz.timezone("EST"))


class BeforeTrigger(TimeTrigger):

    __slots__ = ()

    def evaluate(self, story: NewsStory):
        try:
            return story.pubdate < self._datetime
        except TypeError:
            # If story.pubdate is datetime naive, make it aware
            aware_pubdate = story.pubdate.replace(tzinfo=pytz.timezone("EST"))
            return aware_pubdate < self._datetime


class AfterTrigger(TimeTrigger):

    __slots__ = ()

    def evaluate(self, story: NewsStory):
        try:
            return story.pubdate > self._datetime
        except TypeError:
            # If story.pubdate is datetime naive, make it aware
            aware_pubdate = story.pubdate.replace(tzinfo=pytz.timezone("EST"))
            return aware_pubdate > self._datetime


class NotTrigger(Trigger):

    __slots__ = ("_trig",)

    def __init__(self, trigger_obj):
        self._trig = trigger_obj

    def evaluate(self, story: NewsStory):
        return not self._trig.evaluate(story)


class AndTrigger(Trigger):

    __slots__ = ("_trig", "_other")

    def __init__(self, trigger_obj, other_trigger):
        self._trig = trigger_obj
        self._other = other_trigger

    def evaluate(self, story: NewsStory):
        return self._trig.evaluate(story) and self._other.evaluate(story)


class OrTrigger(Trigger):

    __slots__ = ("_trig", "_other")

    def __init__(self, trigger_obj, other_trigger):
        self._trig = trigger_obj
        self._other = other_trigger

    def evaluate(self, story: NewsStory):
        return self._trig.evaluate(story) or self._other.evaluate(story)


def filter_stories(stories, triggerlist):
    """
    Takes in a list of NewsStory instances.

    Returns: a list of only the stories for which a trigger in triggerlist fires.
    """
    filtered = []

    for story in stories:
        for trigger in triggerlist:
            if trigger.evaluate(story):
                filtered.append(story)
                break

    return filtered


TRIGGERS = {
    "TITLE": TitleTrigger,
    "DESCRIPTION": DescriptionTrigger,
    "AFTER": AfterTrigger,
    "BEFORE": BeforeTrigger,
    "NOT": NotTrigger,
    "AND": AndTrigger,
    "OR": OrTrigger,
}


def parse_trigger_lines(read_lines):
    """
    Parse the lines extracted from the triggers.txt file
    Returns the list of trigger objects according to the parsed lines.
    """
    trigger_list = []
    user_triggers = {}

    for line in read_lines:
        line = line.split(",")
        if line[0] == "ADD":
            trigs_name = line[1:]
            check = all(
                [isinstance(user_triggers[trigs], Trigger) for trigs in trigs_name]
            )
            if check:
                trigger_list.extend(map(user_triggers.__getitem__, trigs_name))
            else:
                raise ValueError(f"Incorrect argument values for 'ADD': {trigs_name!r}")
        elif line[1] not in TRIGGERS:
            raise ValueError(
                f"Incorrect argument value for trigger type: {line[0]!r}\n"
                f"Here are the supported values: {list(TRIGGERS.keys())}"
            )
        else:
            name, trig_type, *args = line
            if trig_type in {"AND", "OR", "NOT"}:
                check = all(
                    [isinstance(user_triggers[trigs], Trigger) for trigs in args]
                )
                if check:
                    args = map(user_triggers.__getitem__, args)
                    user_triggers[name] = TRIGGERS[trig_type](*args)
                else:
                    raise ValueError(
                        f"Incorrect argument values for conjunction triggers: {args!r}"
                    )
            else:
                user_triggers[name] = TRIGGERS[trig_type](*args)

    return trigger_list


def read_trigger_config(filename):
    """
    filename: the name of a trigger configuration file

    Returns: a list of trigger objects specified by the trigger configuration
        file.
    """
    lines = []
    with open(filename, "r", encoding="utf-8") as trigger_file:
        for line in trigger_file:
            line = line.strip()
            if not (len(line) == 0 or line.startswith("//")):
                lines.append(line)

    trigs_list = parse_trigger_lines(lines)

    return trigs_list


def main_thread(master):
    # A sample trigger list - you might need to change the phrases to correspond
    # to what is currently in the news
    # try:
    t1 = TitleTrigger("election")
    t2 = DescriptionTrigger("Trump")
    t3 = DescriptionTrigger("Clinton")
    t4 = AndTrigger(t2, t3)
    triggerlist = [t1, t4]

    triggerlist = read_trigger_config("triggers.txt")

    # HELPER CODE - you don't need to understand this!
    # Draws the popup window that displays the filtered stories
    # Retrieves and filters the stories from the RSS feeds
    frame = Frame(master)
    frame.pack(side=BOTTOM)
    scrollbar = Scrollbar(master)
    scrollbar.pack(side=RIGHT, fill=Y)

    t = "Google & Yahoo Top News"
    title = StringVar()
    title.set(t)
    ttl = Label(master, textvariable=title, font=("Helvetica", 18))
    ttl.pack(side=TOP)
    cont = Text(master, font=("Helvetica", 14), yscrollcommand=scrollbar.set)
    cont.pack(side=BOTTOM)
    cont.tag_config("title", justify="center")
    button = Button(frame, text="Exit", command=root.destroy)
    button.pack(side=BOTTOM)
    guidShown = []

    def get_cont(newstory):
        if newstory.guid not in guidShown:
            cont.insert(END, newstory.title + "\n", "title")
            cont.insert(
                END,
                "\n---------------------------------------------------------------\n",
                "title",
            )
            cont.insert(END, newstory.description)
            cont.insert(
                END,
                "\n*********************************************************************\n",
                "title",
            )
            guidShown.append(newstory.guid)

    while True:
        print("Polling . . .", end=" ")
        # Get stories from Google's Top Stories RSS news feed
        stories = process("https://news.google.com/news/rss")

        # Get stories from Yahoo's Top Stories RSS news feed
        stories.extend(process("https://www.yahoo.com/news/rss"))

        stories = filter_stories(stories, triggerlist)

        list(map(get_cont, stories))
        scrollbar.config(command=cont.yview)

        print("Sleeping...")
        time.sleep(SLEEPTIME)

    # except Exception as e:
    #     print(e)


if __name__ == "__main__":
    root = Tk()
    root.title("Some RSS parser")
    t = threading.Thread(target=main_thread, args=(root,))
    t.start()
    root.mainloop()
    # pass
