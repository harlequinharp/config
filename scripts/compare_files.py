#!/usr/bin/env python

import hashlib, argparse
import os

class TwoWayDict(dict):
    def __len__(self):
        return dict.__len__(self) / 2

    def __setitem__(self,key,value):
        dict.__setitem__(self,key,value)
        dict.__setitem__(self,value,key)

def md5_of_file(filename,blocksize=2**20):
	md5=hashlib.md5()
	if not os.path.exists(filename): return None
	with open(filename,mode='rb') as f:
		while True:
			data=f.read(blocksize)
			if not data:
				break
			md5.update(data)
		return md5.digest()

def enumerate_files(path):
    files=[]
    for file_tuple in os.walk(path):
        for filename in file_tuple[2]:
            filepath=os.path.join(file_tuple[0],filename)
            if os.path.isfile(filepath) and os.path.getsize(filepath) > 0:
                files.append(filepath)
    return files

def get_hashes(files):
    hashes=TwoWayDict()
    for filename in files:
        hashes[filename]=md5_of_file(filename)
    return hashes



parser = argparse.ArgumentParser(description='compare the files contained in two different folders')

parser.add_argument('path1',type=str,
		help='path to the first folder of files')
parser.add_argument('path2',type=str,
		help='path to the second folder of files')

args = parser.parse_args()

path1,path2=args.path1,args.path2

print("enumerating files...")

files_1=enumerate_files(path1)
files_2=enumerate_files(path2)

print("calculating hashes...")

hashes_1=get_hashes(files_1)
hashes_2=get_hashes(files_2)

print("comparing hashes...")

mutual=[]
unique_1=[]
unique_2=[]

for filename in files_1:
    h=hashes_1[filename]
    if h in hashes_2:
        mutual.append((filename,hashes_2[h]))
    else:
        unique_1.append(filename)

for filename in files_2:
    if hashes_2[filename] not in hashes_1:
        unique_2.append(filename)

mutual.sort()
unique_1.sort()
unique_2.sort()
                
print("files in both: ")
for tup in mutual: print("\tpath 1 name: {0}\n\tpath 2 name: {1}\n".format(tup[0],tup[1]))

print("\n\nfiles just in path 1: ")
for filename in unique_1: print("\t{0}".format(filename))

print("\n\nfiles just in path 2: ")
for filename in unique_2: print("\t{0}".format(filename))


