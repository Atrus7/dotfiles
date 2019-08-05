#!/usr/bin/env python
import subprocess
import time
import threading

import sys
import telnetlib

class VideoInfo():
    def __init__(self, v_id, v_time):
        self.v_id = v_id
        self.v_time = v_time

class VLCTelnet():

    HOST = "localhost"
    VIDEO_INFO_FILE = "/home/pi/video_info"

    def __init__(self):
        "Signs in"
        self.open_telnet_buffer()

    def write(self, message):
        self.tn.write(message.encode('ascii') + b"\n")

    def open_telnet_buffer(self):
        self.tn = telnetlib.Telnet(VLCTelnet.HOST, 4212, 30)
        self.tn.read_until(b"Password: ")
        self.write("admin")
        self.tn.read_until(b"> ")

    def clear_telnet_buffer(self):
        """hacky...just close and reopen"""
        self.tn.close()
        self.open_telnet_buffer()

    def print_video_time(self):
        self.tn.read_until(b"\n", .5)
        self.write("get_time")
        tmp_time = self.tn.read_until(b"\n", .5)
        try:
            total_seconds=int(tmp_time)

            minutes = total_seconds // 60
            seconds = total_seconds % 60
            print("%2d:%02d" % (minutes, seconds))

        except ValueError:
            print("Invalid current time " + tmp_time.decode('ascii'))

    def __del__(self):
        self.write("quit")

TELNET = VLCTelnet()

TELNET.write("pause")
TELNET.print_video_time()
