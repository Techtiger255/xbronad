#!/bin/bash
setxkbmap -query | grep variant \
&& setxkbmap us \
|| setxkbmap us colemak
