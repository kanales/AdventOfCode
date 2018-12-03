import numpy as np

with open('input.txt') as f:
    inp = f.read()
    
def parse(row):
    (s0,_,s1,s2) = row.split()
    pos = tuple(map(int,s1[:-1].split(',')))
    size = tuple(map(int,s2.split('x')))
    i = int(s0[1:])
    return i, pos,size

maxtups = lambda tups: map(max,zip(*tups))

rects = list(map(parse, inp.split('\n')))
ids,locs, sizes = zip(*rects)
n,m = map(sum,zip(maxtups(locs),maxtups(sizes)))

M = np.zeros((n,m))
for rec in rects:
    i,(x,y),(h,w) = rec
    for k in range(x,x+h):
        for j in range(y,y+w):
            # Mark as overlapping
            M[k,j] = i if M[k,j] == 0 else -1
print('Task 1:',(M == -1).sum())


for rec in rects:
    i,(x,y),(h,w) = rec
    # Find one that is not ovelapped
    if all(M[k,j] == i for k in range(x,x+h) for j in range(y,y+w)):
        print('Task 2:', i)
        break