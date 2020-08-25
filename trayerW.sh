#!/bin/bash
width=`xprop -name panel | grep 'program specified minimum size' | cut -d ' ' -f 5`
for _ in $(seq 1 $(( (width / 8) + 2)) );do
	echo -n ' '
done
