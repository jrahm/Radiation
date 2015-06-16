import os
import subprocess
import sys

radiation_binary = "radiation"
args = []
if len(sys.argv) < 3:
    print("not enough args")
    exit(1)

args = sys.argv[3:]
radiation_binary = sys.argv[1]
ftype = sys.argv[2]

for (root, _, files) in os.walk('.'):
    for f in files:
        fname = os.path.join(root, f)
        fname = os.path.abspath(fname)

        print("%s %s %s" % (radiation_binary, fname, ftype))
        proc = subprocess.Popen([radiation_binary, fname, ftype] + args)
        proc.wait()

        
