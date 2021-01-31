import sqlite3

conn = sqlite3.connect("tspider.sqlite")
cur = conn.cursor()
cur.execute("SELECT * FROM Twitter")
count = 0

print("{0:20}{1:15}{2:15}".format("Name", "Retrieved", "Friends"))
for i in cur:
    print("{0:20}{1:5}{2:14}".format(i[0], i[1], i[2]))
    count += 1
print("=" * 50)
print(count, "rows")
cur.close()
