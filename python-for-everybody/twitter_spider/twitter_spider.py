import urllib.request, urllib.error
import twurl
import json
import sqlite3
import ssl

# ------------------------------------------------------#
TWITTER_URL = "https://api.twitter.com/1.1/friends/list.json"

conn = sqlite3.connect("tspider.sqlite")
cur = conn.cursor()

# ------------------------------------------------------#
cur.executescript(
    """
CREATE TABLE IF NOT EXISTS People (
id INTEGER PRIMARY KEY,
name TEXT UNIQUE,
retrieved INTEGER
);

CREATE TABLE IF NOT EXISTS Follows (
from_id INTEGER,
to_id INTEGER,
UNIQUE (from_id, to_id)
);

"""
)

# ------------------------------------------------------#
ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

# ------------------------------------------------------#
while True:
    acc = input('Enter a twitter account or "exit" : ')
    if acc == "exit":
        exit()

    if len(acc) < 1:
        cur.execute("SELECT id, name FROM People WHERE retrieved = 0 LIMIT 1")
        try:
            (id, acc) = cur.fetchone()
            print(acc)
        except:
            print("No unretrieved Twitter accounts found")
            continue
    else:
        cur.execute("SELECT id FROM People WHERE name = ? LIMIT 1", (acc,))
        try:
            id = cur.fetchone()[0]
        except:
            cur.execute(
                """INSERT OR IGNORE INTO People
                        (name, retrieved) VALUES (?, 0)""",
                (acc,),
            )
            conn.commit()
            if cur.rowcount != 1:
                print("Error inserting account:", acc)
                continue
            id = cur.lastrowid

    # ------------------------------------------------------#
    url = twurl.augment(TWITTER_URL, {"screen_name": acc, "count": "100"})

    try:
        urlConnect = urllib.request.urlopen(url, context=ctx)
    except Exception as err:
        print("[Failed to Retrieve]", err)
        break

    urlRead = urlConnect.read().decode()
    headers = dict(urlConnect.getheaders())
    print("Remaining ==>", headers["x-rate-limit-remaining"])

    try:
        urlTree = json.loads(urlRead)
    except:
        print("Unable to parse JSON")
        print(urlRead)
        break

    if "users" not in urlTree:
        print("Incorrect JSON received")
        print(json.dumps(urlTree, indent=4))
        continue
    # Debugging
    # print(json.dumps(urlTree,indent=4))

    # ------------------------------------------------------#
    cur.execute("UPDATE People SET retrieved = 1 WHERE name = ?", (acc,))

    countNew = 0
    countOld = 0

    for i in urlTree["users"]:
        friend = i["screen_name"]
        print(friend)
        cur.execute("SELECT id FROM People WHERE name = ? LIMIT 1", (friend,))
        try:
            friend_id = cur.fetchone()[0]
            countOld = countOld + 1
        except:
            cur.execute(
                """INSERT OR IGNORE INTO People (name, retrieved)
                        VALUES (?, 0)""",
                (friend,),
            )
            conn.commit()
            if cur.rowcount != 1:
                print("Error inserting account:", friend)
                continue
            friend_id = cur.lastrowid
            countNew = countNew + 1
        cur.execute(
            """INSERT OR IGNORE INTO Follows (from_id, to_id)
                    VALUES (?, ?)""",
            (id, friend_id),
        )

    print("New accounts =", countNew, " Revisited =", countOld)
    print("Remaining ==>", headers["x-rate-limit-remaining"])
    conn.commit()

# ------------------------------------------------------#
cur.close()
