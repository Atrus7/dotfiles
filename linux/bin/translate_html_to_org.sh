#!/usr/bin/env zsh

if [ $# -lt 1 ] ; then
    echo "bad usage"
    exit 1
fi


readonly ORIG_FILE_PATH=$1
if [ ! -f "$ORIG_FILE_PATH" ] ; then
    echo "no such file $ORIG_FILE_PATH"
    exit 1
fi
FILE=`basename "$ORIG_FILE_PATH"`
HTML_PATH=`mktemp`

NEW_FILENAME=`echo $FILE | tr '[:upper:]' '[:lower:]' | tr -d "-" | sed -e "s/ \+/_/g"  -e "s/.html/.org/"`
NOTES_PATH="/home/atrus/books/notes"
NEW_FILENAME=$NOTES_PATH/$NEW_FILENAME

convert_bad_html.py "$ORIG_FILE_PATH" "$HTML_PATH"
pandoc --from html "$HTML_PATH" --html-q-tags --wrap=none --to org -o "$NEW_FILENAME" --filter ~/bin/no-attrs-filter.hs

echo "$ORIG_FILE_PATH   --------->"
echo "$NEW_FILENAME"

format_org_to_ios_quotes.py $NEW_FILENAME
