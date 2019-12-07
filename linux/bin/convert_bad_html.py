#!/usr/sbin/python
# Convert the div-ridden html into structured html that pandoc can read.

import sys
from lxml import html
orig_path=sys.argv[1]
path=sys.argv[2]

with open(orig_path, 'r') as my_file:
    conversion= [
        ["bookTitle", "h1"],
        ["authors", "p"],
        ["notebookFor", ""] ,
        ["sectionHeading", "h2"],
        ["noteHeading", "h3"],
        ["noteText", "p"],
    ]

    my_html = html.parse(my_file)
    r = my_html.getroot()
    for c in conversion:
        div_class=c[0]
        new_tag=c[1]
        headers=r.find_class(div_class)
        for h in headers:
            h.attrib.pop("class")
            if new_tag:
                h.tag=new_tag
            else:
                h.getparent().remove(h)

with open(path, 'wb') as my_file:
    my_file.seek(0)
    my_file.write(html.tostring(my_html, pretty_print=True))
