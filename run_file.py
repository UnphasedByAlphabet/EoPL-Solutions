from subprocess import run
from sys import argv
import os.path
import shutil

def main(argv: list[str]):
    if len(argv) < 2:
        print("Usage: <exe> <source>")
        return 1
    source = argv[1]
    target = os.path.join(os.path.dirname(source), 'compiled', os.path.basename(source).replace('.', '_') + '.zo')
    output = run(['c:\\\\program files\\racket\\raco.exe', 'make', source], capture_output=True)
    output = run(['c:\\\\program files\\racket\\racket.exe', target], capture_output=True).stdout.decode()
    shutil.rmtree(os.path.join(os.path.dirname(source), 'compiled'))
    print(output, end='')
    return 0

if __name__ == '__main__':
    exit(main(argv))

