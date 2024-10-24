#!/usr/bin/python3
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


    # This is for getting rid of the annoying spans that appear like this:
    # Highlight (<span class="highlight_yellow">yellow</span>) -  Page 12

    # We want to achieve this:
    # Highlight (yellow) -  Page 12
    all_spans = r.findall(".//span")
    for span in all_spans:
        keep_text = span.text + span.tail
        span.getparent().text += keep_text
        span.getparent().remove(span)

with open(path, 'wb') as my_file:
    my_file.seek(0)
    my_file.write(html.tostring(my_html, pretty_print=True))
