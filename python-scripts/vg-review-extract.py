import json

f = open('subset.json')

f2 = open('vidreviews.txt', 'a')

for i, l in enumerate(f):
    d = json.loads(l)
    f2.write(d['reviewText'])
    f2.write('\n')

f.close()
f2.close()
