#!/bin/sh
mkdir -p zmac
cd zmac
curl -o zmac.zip http://48k.ca/zmac.zip
unzip -o zmac zmac.exe
./zmac --doc >zmac.html
start zmac.html
cd ..
read -p "Press ENTER: "
