#!/usr/bin/python3

import os
import re
import sys
from lookup_word import lookup_word

def transform_quotes(input_file_path):
    # Generate output file name based on input file name
    OUTPUT_DIR=os.path.expanduser('~/org/book-notes/shortcut_friendly/')

    base_name = os.path.basename(input_file_path)
    name_without_ext = os.path.splitext(base_name)[0]
    formatted_name = name_without_ext.lower().replace("_", " ").replace(" notebook", "").title().strip()
    output_file_path = f"{OUTPUT_DIR}{formatted_name}.txt"

    # Open the input file and read content
    with open(input_file_path, 'r') as file:
        content = file.read()

    # preprocessing
    # Remove all text until the first three asterisks
    content = re.sub(r'^.*?\*{3}', '***', content, flags=re.DOTALL)
    # Remove headers starting with a single or double asterisk
    content = re.sub(r'^\*{1,2} .*', '', content, flags=re.MULTILINE).strip()
    content = re.sub(r'^\*{3} Bookmark .*', '', content, flags=re.MULTILINE).strip()

    # Split the content into blocks of quotes based on *** separator
    quotes = re.split(r'\*{3}', content)

    # Initialize transformed content
    transformed_content = []

    for quote in quotes:
        # Remove "Location" and "Highlight" information
        quote = re.sub(r'Quote|Location \d+|Highlight\s?\(.*?\)', '', quote)

        # Extract page number if available
        page_match = re.search(r'Page \d+', quote)
        page_number = page_match.group(0) if page_match else ''

        # quote = re.sub(r'[-]?\s?Page \d+\ ?·?|\n', '', quote).strip()
        # Remove the page content from the quote.
        # if page_match:
        #     print(quote)

        # must check before deleting topline.
        is_kindle_note_match = re.search(r'^\s?Note ', quote)
        quote = re.sub(r'^.*Page \d+.*$', '', quote, flags=re.MULTILINE).strip()

        # quote = re.sub(r"\n", '', quote).strip()

        if not quote:
            continue  # Skip empty quotes

        # We're either dealing with Note, Highlight, or Definition.
        if is_kindle_note_match:
            print("Noted: " + quote)
            quote = '[note: ' + quote + ']'

        # If the quote is a single word, add a definition
        if len(quote.split()) == 1:
            definition = lookup_word(quote)
            if definition:
                # Format definition (take first definition)
                print("adding quote for word: " + quote)
                quote = f"{quote}: {definition}"

        if is_kindle_note_match:
            pass # we don't add back anything.
        # Add the page number back if available
        elif page_number:
            quote = f"*** {page_number}\n\n{quote}"
        else:
            quote = f"***\n\n{quote}"

        # Add transformed quote to the list
        transformed_content.append(quote)

    # Join all transformed quotes with ***
    final_content = '\n'.join(transformed_content)

    # Write to the output file
    with open(output_file_path, 'w') as file:
        file.write(final_content)

    print(f"Transformed content saved to {output_file_path}")

# File path
# input_file_path = os.path.expanduser('~/books/notes/reading_like_a_writer_(p.s.)_notebook.org')
# Transform the quotes
# transform_quotes(input_file_path)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script.py <input_file_path>")
    else:
        input_file_path = os.path.expanduser(sys.argv[1])
        transform_quotes(input_file_path)
