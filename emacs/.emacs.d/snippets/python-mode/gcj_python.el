# -*- mode: snippet; require-final-newline: nil -*-
# name: gcj-python
# key: gcj-s
# binding: direct-keybinding
# --
import math
import codejamhelpers

# Solve here
def solve(input):
    return input



if __name__ == "__main__":
    testcases = eval(input())
    for case_num in range(1, testcases+1):
        S = str(input())
        print("Case #%i: %s" % (case_num, solve(S)))


def format_list_of_nums(l):
    """Returns a space separated list of numbers as a string"""
    if(l[0] != str(l[0])):
        l = map(str, l)
    final = " ".join(new_str)
    return final

def format_list_of_chars():
    """Returns a string with the chars concatenated"""
    final = "".join(new_str)
    return final
