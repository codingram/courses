# This application will read the mailbox data (mbox.txt) and count the number
# of email messages per organization (i.e. domain name of the email address)
# using a database with the following schema to maintain the counts.
# CREATE TABLE Counts (org TEXT, count INTEGER)

import sqlite3

# Connect the database to python and give a cursor (handle in files) to the connection.
conn = sqlite3.connect("orgcount.sqlite")
cur = conn.cursor()

# Delete the table and data if the file exists and create a new table.
# Commiting the changes to the database (NOTE: use conn instead of cur for commit())
cur.execute("DROP TABLE IF EXISTS Counts")
cur.execute("CREATE TABLE Counts (org TEXT, count INTEGER)")
conn.commit()

fname = input("Enter the file name: ")
fhand = open(fname)

# Getting the email host name from matching lines.
for line in fhand:
    if not line.startswith("From: "):
        continue
    line = line.strip()
    orgMail = line.split("@")[1]

    # Selecting the count value where orgMail matches
    cur.execute("SELECT count FROM Counts WHERE org = ?", (orgMail,))

    # Fetching the count value and assigning it to mailRow.
    mailCount = cur.fetchone()

    # If mailCount is None i.e., no entry has been made for that mail,
    # create the entry with orgMail and set count to 1.
    if mailCount is None:
        cur.execute("INSERT INTO Counts (org, count) VALUES (?, 1)", (orgMail,))

    # If mailCount is not None i.e., entry has been made and count value
    # is present, then update the count value by adding 1 to it.
    else:
        cur.execute("UPDATE Counts SET count = count + 1 WHERE org = ?", (orgMail,))
    conn.commit()

# Getting the orlMail, count tuple in descending order (Default is ascending) and
# printing it out on the terminal.
for row in cur.execute("SELECT org,count FROM Counts ORDER BY count DESC"):
    print(f"{row[0]:20} ==> {row[1]:5}")

cur.close()
