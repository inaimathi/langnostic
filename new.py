#!/usr/bin/python
import os.path, json

from subprocess import check_output
from optparse import OptionParser

def wc(fname):
    return int(check_output(["wc", "-l", fname]).split(" ")[0])

def slugFromFname(fname):
    return os.path.basename(os.path.splitext(fname)[0])

def titleFromFname(fname):
    return " ".join(slugFromFname(fname).split("-")).title()

def main(fname, title, tags):
    slug = slugFromFname(fname)
    ed = int(os.path.getctime(fname))
    mod = int(os.path.getmtime(fname))
    newId = wc("posts.json")
    rec = {'id': newId, 'title': title, 'file': slug, 'edited': ed, 'posted': mod, 'tags': tags}
    with open("posts.json", 'a') as f:
        f.write(json.dumps(rec))

if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-t", "--title", dest="title", default=False,
                      help="Manually specify the title (it'll otherwise be generated from the slug)")
    (options, args) = parser.parse_args()
    if len(args) >= 2:
        main(args[0], options.title or titleFromFname(args[0]), args[1:])
    else:
        parser.print_help()
