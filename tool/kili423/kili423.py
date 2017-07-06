#!/usr/bin/python3.1
import sys

src = sys.argv[1]
dst = src.replace(".kicad_mod","old.kicad_mod")
with open(src,'r') as f:
    srcMod = f.read();
f.closed
srcMod = srcMod.replace('F.Fab','Eco1.User')
srcMod = srcMod.replace('B.Fab','Eco1.User')
srcMod = srcMod.replace('F.CrtYd','Eco2.User')
srcMod = srcMod.replace('B.CrtYd','Eco2.User')
print(srcMod)
with open(dst,'w') as f:
    f.write(srcMod)
f.closed
