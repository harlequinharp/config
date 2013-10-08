#!/usr/bin/env python

import os,re,zipfile

files=os.listdir()
files.sort()
pub=[f for f in files if re.search(".epub$",f)]
pdf=[f for f in files if re.search(".pdf$",f)]
for p,f in zip(pub,pdf):
    with zipfile.ZipFile(f[:-4]+".zip",mode='w',compression=zipfile.ZIP_LZMA) as z:
        print("writing {0} and {1} to {2}".format(p,f,z.filename))
        z.write(p)
        z.write(f)

