from shutil import get_terminal_size as term_size
from sys import stdout as o
from time import sleep
w=o.write
f=o.flush

while True:
    size = f"{(size := term_size((1,1))).columns: >5} {size.lines: >5}"
    w(size + "\b"*len(size))
    f()
    sleep(0.01)