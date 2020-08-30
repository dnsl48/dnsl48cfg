xsetroot -cursor_name left_ptr
setxkbmap -layout us,ru -variant dvp,winkeys -option grp:rshift_toggle, grp_led:scroll, compose:rctrl, ctrl:nocaps, lv3:ralt_switch, esperanto:dvorak
eval ~/.screenlayout/default.sh
eval $(ssh-agent)
exec xmonad
