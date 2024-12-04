#!/bin/bash

echo "Hello"
cut -d " " -f 1 input.txt | sort >/tmp/first.txt
cut --complement -d " " -f 1 input.txt  | sort >/tmp/second.txt

