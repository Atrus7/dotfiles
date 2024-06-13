#!/bin/bash
# Convenience for use with emacs...
path=$1

pushd ${path} >/dev/null
. ${path}/build/envsetup.sh >/dev/null

if [ "$2" ] ; then
    lunch $2 | grep ^[A-Z]
else
    for choice in ${LUNCH_MENU_CHOICES[@]}
    do
        echo "$choice"
    done
fi

popd >/dev/null
