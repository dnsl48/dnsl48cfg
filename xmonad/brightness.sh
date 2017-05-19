#!/bin/sh

if [ "$2" == "up" ]
then
    echo $(($(cat "/sys/class/backlight/$1/brightness") + $3)) | tee "/sys/class/backlight/$1/brightness"

elif [ "$2" == "toggle" ]
then
    echo $(cat /sys/class/backlight/$1/brightness) | awk --non-decimal-data '{ ($1 == 0 ? system ("cat /sys/class/backlight/$1/max_brightness") : system ("echo 0")) }' | tee "/sys/class/backlight/$1/brightness"

else
    echo $(($(cat "/sys/class/backlight/$1/brightness") - $3)) | tee "/sys/class/backlight/$1/brightness"

fi