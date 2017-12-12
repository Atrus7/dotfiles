#!/usr/bin/python
# Call with position or dims

import argparse
import re
import subprocess

from operator import itemgetter

def resize():
    pass

def xrandr_get_monitors():
    command_args = ['xrandr','--listactivemonitors']
    res = subprocess.check_output(command_args).decode().split('\n')
    res.pop(0)
    if(res[-1] == ""):
        res.pop()
    sizes = []
    for x_monitor_str in res:
        dims = re.findall('[0-9]+/',  x_monitor_str)
        if(len(dims) == 2):
            # strips off the / that matched
            dims[0] = dims[0].rstrip('/')
            dims[1] = dims[1].rstrip('/')

        else:
            raise(Exception)
        sizes.append((dims[0], dims[1]))
    return sizes

def get_bigger_monitor():
    sizes = xrandr_get_monitors()
    bigger = sorted(sizes, key=itemgetter(1), reverse=True)[0]
    return list(map(float, bigger))

def get_right_offset(offset):
    width = get_bigger_monitor()[0]
    return int(width - offset)

def get_bottom_offset(offset):
    height = get_bigger_monitor()[1]
    return int(height - offset)

def i3_action(i3_cmd):
    full_cmd = ['i3-msg', i3_cmd]
    subprocess.call(full_cmd)

def i3_percent_resize(w, h):
    resize_dims = get_resized_value(w,h)
    i3_cmd = ' resize set {0} {1}'.format(resize_dims[0], resize_dims[1])
    i3_action(i3_cmd)

def i3_prep_floating_window():
    i3_action("mark floating_video; fullscreen disable; floating enable; sticky enable;")

def i3_percent_move(w, h, pos):
    win_width, win_height = get_resized_value(w,h)
    move_to = [0,0]
    if pos == "tr":
        move_to = [get_right_offset(win_width), 0]
    elif pos == "br":
        move_to = [get_right_offset(win_width), get_bottom_offset(win_height)]
    elif pos == "tl":
        move_to = [0, 0]
    elif pos == "bl":
        move_to = [0, get_bottom_offset(win_height)]
    else:
        raise(Exception)
    i3_cmd = ' move window to position {0} {1}'.format(move_to[0], move_to[1])
    print(i3_cmd)
    i3_action(i3_cmd)

def get_resized_value(w,h):
    if w > 1 and h > 1:
        w = float(w) / float(100)
        h = float(h) / float(100)
    dims = get_bigger_monitor()

    min_chromium_w = 800
    min_chromium_h = 500

    scaled_w = dims[0] * w
    if scaled_w < min_chromium_w:
        scaled_w = min_chromium_w
    scaled_h = dims[1] * h
    if scaled_h < min_chromium_h:
        scaled_h = min_chromium_h

    return int(scaled_w), int(scaled_h)

def parse_arguments():
    parser = argparse.ArgumentParser()
    parser.add_argument("width", help="numeric value, treated as a percent")
    parser.add_argument("height", help="numeric value, treated as a percent")
    parser.add_argument("pos", help="[tl,bl,br,tr]")
    parser.add_argument("--prepare_window", help="sets up window", action="store_true")
    return parser.parse_args()

def main():
    print('he')
    args = parse_arguments()
    args.width = float(args.width)
    args.height = float(args.height)
    if args.prepare_window:
        i3_prep_floating_window()

    i3_percent_resize(args.width, args.height)
    i3_percent_move(args.width, args.height, args.pos)

if __name__ == '__main__':
    main()
