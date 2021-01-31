# Exercise 1:
#
# Change the socket program socket1.py to prompt the user for the URL so it can
# read any web page. You can use split('/') to break the URL into its component
# parts so you can extract the host name for the socket connect call. Add error
# checking using try and except to handle the condition where the user enters
# an improperly formatted or non-existent URL.

# Exercise 2:
#
# Change your socket program so that it counts the number of characters it has
# received and stops displaying any text after it has shown 3000 characters.
# The program should retrieve the entire document and count the total number of
# characters and display the count of the number of characters at the end of the
# document.

# Exercise 5: (Advanced)
#
# Change the socket program so that it only shows data after the headers and a
# blank line have been received. Remember that recv receives characters
# (newlines and all), not lines.

# http://data.pr4e.org/intro-short.txt

import socket

print("Enter 'e' to exit the program.")

while True:
    while True:
        url = input("Enter URL: ")
        if url == "e":
            exit()
        urlList = url.split("/")
        try:
            host = urlList[2]
            break
        except:
            print("Error, an improperly formatted URL or non-existant URL.")
            continue
    mysock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        mysock.connect((host, 80))
        break
    except:
        print("Error, an improperly formatted URL or non-existant URL.")
        continue

cmd = "GET " + url + " HTTP/1.0\r\n\r\n"
ecmd = cmd.encode()
mysock.send(ecmd)

count = 0
pdata = b""

while True:
    data = mysock.recv(3000)
    if len(data) < 1:
        break
    count += len(data)
    pdata += data

pos = pdata.find(b"\r\n\r\n")
print(pdata[pos + 4 : pos + 3004].decode() + "......")

print("\nTotal number of characters:", count, "\n")
mysock.close()
