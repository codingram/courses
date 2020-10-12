#!/usr/bin/env bash
# All the assignments for the missing semester of your CS education course

echo -e "\nExercise: 1"
curl -Is https://missing.csail.mit.edu | # I is used to get just the headers
grep -i last-modified |                  # Ignore case (i) and get the 'last-modified' field
cut -d : -f2 |                           # Use delimiter ':' and get the second field
tee last-modified.txt                    # Write it to stdout and in a file 


echo -e "\nExercise: 2.1"
ls -laGth
# l - in list format
# a - all files including the hidden ones
# G - colorized format
# t - sort by recency
# h - sizes are in human readable format


echo -e "\nExercise: 2.2"
function marco() {
    curdir=$(pwd)
}

function polo() {
    cd "$curdir" || echo "No directory saved"
}
echo "Use 'source solutions.sh' to source marco and polo"


echo -e "\nExercise: 2.3"
count=0
while true; do
    if ! ./test.sh &> debug.txt ; then
        break
    fi
    count=$((count+1))
done

echo "Total run: $count"
cat debug.txt


echo -e "\nExercise: 2.4:"
echo "Test folder: ziptest"
# Find all files having extension html
# Separate search results by NULL character (useful for xargs)
# Convert stdout to arguments with NULL character as the delimiter
# Create an archive of the files (arguments from xargs)
fd -t f -e html -p ./ziptest/ -0 | xargs -0 tar -cf zipped.tar
echo "List of files inside zipped.tar:"
tar -tf zipped.tar


echo -e "\nExercise: 4.2"
grep -E "(.*a){3,}.*" /usr/share/dict/words | # Extracting all the words which contains atleast 3 a's
grep -Ev "'s$" |                              # Removing the 's if there are any
tr "[:upper:]" "[:lower:]" |                  # Translating uppercase to lowercase
sed -E "s/.*(..)$/\1/" |                      # Substituting the word with the last two characters 
# grep -o "..$" |                             # Get the output as the last two characters
sort |                                        # Sort it (Need to do it for uniq)
uniq -c |                                     # Get all the unique character combination and its count
sort -nk1,1 |                                 # Sort it on count
tail -n3                                      # Get the last three data


echo -e "\nExercise 4.6.1"
curl -s https://ucr.fbi.gov/crime-in-the-u.s/2016/crime-in-the-u.s.-2016/topic-pages/tables/table-1 | # s - silent the output
pup "div#table-data-container table tbody td.group1 text{}" | # Extract only the table data
grep -Ev '^$' |                                               # Remove all the blank lines
# sed "s/,//g" |                                                # Remove all the commas from the data
tr -d , |
datamash min 1 max 1                                          # Get the max and min for column 1


echo -e "\nExercise 4.6.2"
curl -s https://ucr.fbi.gov/crime-in-the-u.s/2016/crime-in-the-u.s.-2016/topic-pages/tables/table-1 | # s - silent the output
pup "div#table-data-container table tbody td.group1, td.group2 text{}" | # Extract data from two columnsa
# grep -Ev "^$" |                     # Remove all the blank lines
# sed 's/,//g' |                      # Remove all the commas from the data
pr -2 -t |                          # Print format with two columns, remove the headers and footers
tr -ds ',\t' ' \n' |                # First delete all the commas and tabs, squeeze mutltiple spaces and newline 
# tr -d '\t' |                        # Remove all the tabs
# As awk is a programming language, we can do more things with it:
# BEGIN matches only the start of the pattern, here it assigns 0 to variable tot
# Then for each line it subtracts second data from first and checks if it's not equal to 0
# Why? Well, for tr we removed all unnecessary newlines but we couldn't remove the first one 
# so that evaluates to 0. Here it won't make a difference but better safe than sorry
# If it's not 0 then add it to tot and once all lines are over END matches the last line
# and prints the value of tot.
awk 'BEGIN { tot = 0 } 
     { $1 = $1 - $2 } $1 != 0 { tot += $1 } 
     END { print tot }'
# awk '{$1 = $1 - $2} $1 != 0 {print $1}' | # Subtract the two data, assign it to first column and get that column if it's not 0
# paste -sd+ - |                      # Concatenate all lines into a single line seperated by +
# bc -l                               # Evaluate the arithmetic expression (l - use predefined mathlib)
# datamash sum 1                      # (alt) Sum it up!

echo -e "\nExercise 6.2.1"
git --git-dir=gitrepo/.git --work-tree=./gitrepo log --all --oneline --graph -n 10

echo -e "\nExercise 6.2.2"
git --git-dir=gitrepo/.git --work-tree=./gitrepo log -n 1 --format=%an -p -s -- README.md

echo -e "\nExercise 6.2.3"
git --git-dir=gitrepo/.git --work-tree=./gitrepo blame _config.yml |
grep collections: |
grep -Eo "^\w+" |
xargs -I{} git --git-dir=gitrepo/.git --work-tree=./gitrepo show -s --format=%B {}


echo -e "\nExercise 7.1"
log show --last 1d --predicate "process CONTAINS[c] 'sudo' && message CONTAINS[c] 'dhruvmanilawala'" |
grep -Eio "command=.*"


