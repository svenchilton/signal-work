import random

# Read in the JSON file
f = open('reviews_Video_Games_5.json')

# Our JSON file contains 231780 lines
# Use random.sample() to extract 10% of them
# More accurately, extract 10% of the line numbers
rs = random.sample(range(231780), 23178)

f2 = open('subset.json', 'a')

# Check each line of the JSON file to see if it is in the random sample.
# If so, append it to subset.json.
for i, line in enumerate(f):
    if i in rs: 
        f2.write(line)
