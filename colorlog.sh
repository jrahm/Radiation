#!/bin/bash
sed 's/\[Error\]/[1;31m[Error]/g' | \
sed 's/\[Info\]/[1;37m[Info]/g' | \
sed 's/\[Debug\]/[1;34m[Debug]/g' | \
sed 's/g:\w*/[1;33m\0/g'
echo -ne "\033[0m"
