import sqlite3
import urllib.error
import ssl
from urllib.parse import urljoin
from urllib.parse import urlparse
from urllib.request import urlopen
from bs4 import BeautifulSoup
import time

start_time = time.time()

# --------------------------------------------------#
# Ignore SSL certificate errors

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

# --------------------------------------------------#
# Creating the database file and adding all the tables and columns

conn = sqlite3.connect("spider.sqlite")
cur = conn.cursor()

# -- html : HTML data of the webpage
# -- new_rank : Default value = 1.0
# -- from_id : ID of the page we're about to crawl into
# -- to_id : ID of the link within the page we crawled into
cur.executescript(
    """
    CREATE TABLE IF NOT EXISTS Pages (
    id INTEGER PRIMARY KEY, 
    url TEXT UNIQUE, 
    html TEXT,
    error INTEGER, 
    old_rank REAL, 
    new_rank REAL 
    );

    CREATE TABLE IF NOT EXISTS Links (
    from_id INTEGER, 
    to_id INTEGER,
    UNIQUE(from_id, to_id)
    );

    CREATE TABLE IF NOT EXISTS Webs (
    url TEXT UNIQUE);
    """
)

# --------------------------------------------------#
# Check to see if we are already in progress.
# Getting a random url from the table and if it's none (no crawl started yet) asking for a url and starting the crawl.

cur.execute(
    "SELECT id,url FROM Pages WHERE html is NULL and error is NULL ORDER BY RANDOM() LIMIT 1"
)
row = cur.fetchone()

if row is not None:
    print("Restarting existing crawl.  Remove spider.sqlite to start a fresh crawl.")
else:
    starturl = input("Enter web url or enter: ")
    if len(starturl) < 1:
        starturl = "http://www.dr-chuck.com/"

    # -- Removing the end slash
    if starturl.endswith("/"):
        starturl = starturl[:-1]
    web = starturl

    # -- Removing the last part of the url if it contains [.html or .htm ] Eg: www.eg.com/page1/eg.html => www.eg.com/page1
    if starturl.endswith(".htm") or starturl.endswith(".html"):
        pos = starturl.rfind("/")
        web = starturl[:pos]

    # -- Inserting the url in Webs table and Pages table. Commiting is a must to select the url from the database
    if len(web) > 1:
        cur.execute("INSERT OR IGNORE INTO Webs (url) VALUES ( ? )", (web,))
        cur.execute(
            "INSERT OR IGNORE INTO Pages (url, html, new_rank) VALUES ( ?, NULL, 1.0 )",
            (starturl,),
        )
        conn.commit()

# --------------------------------------------------#
# Enter the history of our crawl in Webs table

cur.execute("SELECT url FROM Webs")
webs = list()
for row in cur:
    webs.append(str(row[0]))
print(webs)

# --------------------------------------------------#
# Start of the crawl loop

# Asking for how many pages we want to crawl. When all is done (many = 0) the loop breaks
many = 0
while True:
    if many < 1:
        sval = input("How many pages: ")
        if len(sval) < 1:
            break
        many = int(sval)
    many = many - 1

    # -- Selecting a random url to crawl from the database. We entered one in the previous section
    cur.execute(
        "SELECT id,url FROM Pages WHERE html is NULL and error is NULL ORDER BY RANDOM() LIMIT 1"
    )
    try:
        row = cur.fetchone()
        fromid = row[0]
        url = row[1]
    except:
        print("No unretrieved HTML pages found")
        many = 0
        break

    print(fromid, url, end=" ")

    # -- If we are retrieving this page, there should be no entry in Links table
    cur.execute("DELETE from Links WHERE from_id=?", (fromid,))

    try:
        document = urlopen(url, context=ctx)
        html = document.read()

        # ------ If there is an error, update the table with the error code to the corresponding site
        if document.getcode() != 200:
            print("Error on page: ", document.getcode())
            cur.execute(
                "UPDATE Pages SET error=? WHERE url=?", (document.getcode(), url)
            )

        # ------ Ignoring all the non-html pages and deleting them from the database
        if "text/html" != document.info().get_content_type():
            print("Ignore non text/html page")
            cur.execute("DELETE FROM Pages WHERE url=?", (url,))
            conn.commit()
            continue

        print("(" + str(len(html)) + ")", end=" ")

        soup = BeautifulSoup(html, "html.parser")

    # -- Displaying a mild message instead of blowing up when pressing ^C
    except KeyboardInterrupt:
        print("")
        print("Program interrupted by user...")
        break

    # -- Except block if there's any problem retrieving or parsing the webpage
    except:
        print("Unable to retrieve or parse page")
        cur.execute("UPDATE Pages SET error=-1 WHERE url=?", (url,))
        conn.commit()
        continue

    # -- Inserting the html content in the database
    cur.execute(
        "INSERT OR IGNORE INTO Pages (url, html, new_rank) VALUES ( ?, NULL, 1.0 )",
        (url,),
    )
    cur.execute("UPDATE Pages SET html=? WHERE url=?", (memoryview(html), url))
    conn.commit()

    # --------------------------------------------------#
    # -- Retrieve all of the anchor tags and looping through them to get the href values (links)
    tags = soup("a")
    count = 0

    for tag in tags:
        href = tag.get("href", None)
        if href is None:
            continue

        # ------ Resolve relative references like href="/contact" urlparse results in breaking the url into individual components like http, www.eg.com, /in/path. So, .scheme is the protocol used by the link (http, https) Here as relative links won't contain scheme, we'll add the main url to the path converting relative url into an absolute one. [1]
        up = urlparse(href)
        if len(up.scheme) < 1:
            href = urljoin(url, href)

        # ------ Removing the in-page references
        ipos = href.find("#")
        if ipos > 1:
            href = href[:ipos]

        # ------ Ignoring image links
        if href.endswith(".png") or href.endswith(".jpg") or href.endswith(".gif"):
            continue

        # ------ Removing the end slash
        if href.endswith("/"):
            href = href[:-1]

        # ------ Skip the empty href values
        if len(href) < 1:
            continue

        # ------ Check if the URL is in any of the webs
        found = False
        for web in webs:
            if href.startswith(web):
                found = True
                break
        if not found:
            continue

        # ------ Entering the retrieved link from the webpage in the table
        cur.execute(
            "INSERT OR IGNORE INTO Pages (url, html, new_rank) VALUES ( ?, NULL, 1.0 )",
            (href,),
        )
        count = count + 1
        conn.commit()

        # ------ Select the id of the new page entered and assign it to 'toid'
        cur.execute("SELECT id FROM Pages WHERE url=? LIMIT 1", (href,))
        try:
            row = cur.fetchone()
            toid = row[0]
        except:
            print("Could not retrieve id")
            continue

        # ------ Enter the relation between the crawl page and new page (From - to)
        cur.execute(
            "INSERT OR IGNORE INTO Links (from_id, to_id) VALUES ( ?, ? )",
            (fromid, toid),
        )

    print(count)
    print("Remaining:", many)

cur.close()

# --------------------------------------------------#
# Recording the time for the session

stop_time = time.time()
seconds = stop_time - start_time
minutes = int(seconds // 60)
remSec = int(seconds % 60)
remMin = int(minutes % 60)
hours = int(minutes // 60)
print(f"Time for the current session [hh:mm:ss]: {hours:02}:{remMin:02}:{remSec:02}")

# -------------------- NOTES -----------------------#
# [1] u = 'http://www.example.com/page1/this-is-an-example-link.html/'
# >>> e = urlparse(u)
# ParseResult(scheme='http', netloc='www.example.com', path='/page1/this-is-an-example-link.html/', params='', query='', fragment='')
# https://docs.python.org/3/library/urllib.parse.html#url-parsing
