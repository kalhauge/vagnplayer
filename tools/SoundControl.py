import os
from time import sleep

import Rpi.GPIO as GPIO


GPIO.setmode(GPIO.BCM)

GPIO.setup(23, GPIO.IN)
GPIO.setup(24, GPIO.IN)
GPIO.setup(25, GPIO.IN)

while True:
        if(GPIO.input(23) == False):
                os.system('amixer -q sset Master 3%+')
        if(GPIO.input(24) == False):
                os.system('amixer -q sset Master 3%-')
        if(GPIO.input(25) == False):
                os.system('mpc clear')
                
        sleep(0.1)
