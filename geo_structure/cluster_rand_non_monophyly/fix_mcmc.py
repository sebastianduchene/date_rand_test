import sys, os, subprocess, re

print sys.argv

xml_files = [i for i in os.listdir(sys.argv[1]) if not re.match('.+xml$', i) is None]


for i in xml_files:
    call_cmd = "sed -i.bak 's/15000000/50000000/g' "+sys.argv[1]+"/"+i
    subprocess.call(call_cmd, shell = True)


subprocess.call("rm "+sys.argv[1]+"/"+"*bak", shell = True)

