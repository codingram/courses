#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import xml.etree.ElementTree as ET
import sqlite3


def create_table():
    cur.executescript(
        """
    CREATE TABLE IF NOT EXISTS Track (
    id INTEGER NOT NULL PRIMARY KEY UNIQUE,
    title TEXT UNIQUE,
    length INTEGER,
    rating INTEGER,
    count INTEGER,
    album_id INTEGER,
    genre_id INTEGER
    );

    CREATE TABLE IF NOT EXISTS Album (
    id INTEGER NOT NULL PRIMARY KEY UNIQUE,
    title TEXT UNIQUE,
    artist_id INTEGER
    );

    CREATE TABLE IF NOT EXISTS Genre (
    id INTEGER NOT NULL PRIMARY KEY UNIQUE,
    name TEXT UNIQUE
    );

    CREATE TABLE IF NOT EXISTS Artist (
    id INTEGER NOT NULL PRIMARY KEY UNIQUE,
    name TEXT UNIQUE
    );
    """
    )


def file_data():
    fileName = input("Enter file name: ")

    try:
        dataTree = ET.parse(fileName)
    except:
        print("Error, file doesn't exist or filename incorrect")
        return

    trackData = dataTree.findall("dict/dict/dict")
    print("Count:", len(trackData))
    return trackData


def lookup(data, keyText):
    found = False
    for d in data:
        if found == True:
            return d.text
        if d.tag == "key" and d.text == keyText:
            found = True
    return None


def main():
    create_table()
    trackData = file_data()

    if not trackData:
        return

    for track in trackData:
        if lookup(track, "Track ID") is None:
            continue

        name = lookup(track, "Name")
        artist = lookup(track, "Artist")
        album = lookup(track, "Album")
        genre = lookup(track, "Genre")
        length = lookup(track, "Total Time")
        rating = lookup(track, "Rating")
        count = lookup(track, "Play Count")

        if name == None or artist == None or album == None or genre == None:
            continue

        cur.execute("INSERT OR IGNORE INTO Artist(name) VALUES (?)", (artist,))
        cur.execute("SELECT id FROM Artist WHERE name = ?", (artist,))
        artist_id = cur.fetchone()[0]

        cur.execute("INSERT OR IGNORE INTO Genre(name) VALUES (?)", (genre,))
        cur.execute("SELECT id FROM Genre WHERE name = ?", (genre,))
        genre_id = cur.fetchone()[0]

        cur.execute(
            "INSERT OR IGNORE INTO Album(title, artist_id) VALUES (?,?)",
            (album, artist_id),
        )
        cur.execute("SELECT id FROM Album WHERE title = ?", (album,))
        album_id = cur.fetchone()[0]

        cur.execute(
            "INSERT OR REPLACE INTO Track(title,length,rating,count,album_id,genre_id) VALUES(?,?,?,?,?,?)",
            (name, length, rating, count, album_id, genre_id),
        )

    conn.commit()


if __name__ == "__main__":
    conn = sqlite3.connect("trackdb.sqlite")
    cur = conn.cursor()
    main()
    cur.close()
