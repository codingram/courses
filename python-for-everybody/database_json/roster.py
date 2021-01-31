import sqlite3
import json

conn = sqlite3.connect("roster_db.sqlite")
cur = conn.cursor()

cur.executescript(
    """
DROP TABLE IF EXISTS User;
DROP TABLE IF EXISTS Course;
DROP TABLE IF EXISTS Member;

CREATE TABLE User (
id INTEGER NOT NULL PRIMARY KEY UNIQUE,
name TEXT UNIQUE
);

CREATE TABLE Course (
id INTEGER NOT NULL PRIMARY KEY UNIQUE,
title TEXT UNIQUE
);

CREATE TABLE Member (
user_id INTEGER,
course_id INTEGER,
role INTEGER,
PRIMARY KEY (user_id, course_id)
);

"""
)

fileName = input("Enter file name: ")
if len(fileName) < 1:
    fileName = "roster_data.json"
fileRead = open(fileName).read()
dataTree = json.loads(fileRead)

# print(json.dumps(dataTree, indent = 4)) # To check the retrieval of data

for data in dataTree:
    name = data[0]
    title = data[1]
    role = data[2]
    # print(f'{name:20}{title:5}{role:5}')

    cur.execute("INSERT OR IGNORE INTO User (name) VALUES (?)", (name,))
    cur.execute("SELECT id FROM User WHERE name = ?", (name,))
    user_id = cur.fetchone()[0]

    cur.execute("INSERT OR IGNORE INTO Course (title) VALUES (?)", (title,))
    cur.execute("SELECT id FROM Course WHERE title = ?", (title,))
    course_id = cur.fetchone()[0]

    cur.execute(
        "INSERT OR REPLACE INTO Member (user_id, course_id, role) VALUES (?, ?, ?)",
        (user_id, course_id, role),
    )

conn.commit()

selectData = (
    "SELECT User.name, Member.role, Course.title "
    + "FROM User JOIN Member JOIN Course "
    + "ON Member.user_id = User.id AND Member.course_id = Course.id "
    + "ORDER BY Course.title, Member.role DESC, User.name"
)

for data in cur.execute(selectData):
    print(f"{data[0]:18}{data[1]}{data[2]:>10}")

cur.close()
