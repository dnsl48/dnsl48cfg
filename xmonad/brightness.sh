#!/bin/sh

if [ "$2" == "up" ]
then
    echo $(($(cat "/sys/class/backlight/$1/brightness") + $3)) | tee "/sys/class/backlight/$1/brightness"
else
    echo $(($(cat "/sys/class/backlight/$1/brightness") - $3)) | tee "/sys/class/backlight/$1/brightness"
fi