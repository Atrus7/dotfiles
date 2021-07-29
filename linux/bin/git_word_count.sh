#!/bin/bash
# Calculate writing word diff between revisions. Cribbed / modified from:
# https://stackoverflow.com/questions/2874318/quantifying-the-amount-of-change-in-a-git-diff
function git_words_added {
    revision=${1:-master}

    git diff --word-diff=porcelain $revision | \
        grep -e "^+[^+]" | \
        wc -w | \
        xargs
}

function git_words_removed {
    revision=${1:-master}

    git diff --word-diff=porcelain $revision | \
        grep -e "^-[^-]" | \
        wc -w | \
        xargs
}

function git_words_diff {
    revision=${1:-master}

    echo $(($(git_words_added $1) - $(git_words_removed $1)))
}

# can call the function from the script like script.sh git_words_added
if [ ! -z "$1" ]; then
   if declare -f "$1" > /dev/null
   then
       # call arguments verbatim
       "$@"
   else
       # Show a helpful error
       echo "'$1' is not a known function name" >&2
       exit 1
   fi
fi
