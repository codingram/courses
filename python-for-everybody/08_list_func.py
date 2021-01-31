# Exercise 1:
#
# Taking the input of elements of the list, asking the user whether they want to
# chop the first and the last elements of the list and then putting it in a chop
# function. Now, if the user says no then they will be asked whether they want to
# remove the middle elements so that the new list will only contain the first and
# the last elements. If no the program will print 'No functions taken on the list'
# NOTE: I've added a lot of additional program in the code


def chop(tlist):
    tlen = len(tlist)
    del tlist[tlen - 1]
    del tlist[0]
    return


def middle(tlist):
    tlen = len(tlist)
    t1 = tlist[0]
    t2 = tlist[tlen - 1]
    t3 = [t1, t2]
    return t3


lexp = []
while True:
    ldata = input("Enter list element: ")
    if ldata == "done":
        break
    lexp.append(ldata)

print("Entered list:", lexp)

prompt1 = input(
    "Do you want to remove the first and last elements of the list? [y/n]: "
)

if prompt1 == "y":
    chop(lexp)
else:
    prompt2 = input(
        "Do you want the list to contain just the first and the last elements? [y/n]: "
    )
    if prompt2 == "y":
        lexp = middle(lexp)
    else:
        print("No alterations called by the user.")
        exit()

print("Altered list:", lexp)
