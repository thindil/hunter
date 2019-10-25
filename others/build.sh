#!/bin/bash

gprclean -P hunter.gpr
gprbuild -P hunter.gpr -XMode=release
mkdir -p usr/share/doc/hunter
mkdir -p usr/share/locale
cp -r bin usr/
cp CHANGELOG.md usr/share/doc/hunter/
cp COPYING usr/share/doc/hunter/
cp README.md usr/share/doc/hunter/
cp CONTRIBUTING.md usr/share/doc/hunter/
./translations.sh generate
for directory in $(find po/* -maxdepth 0 -type d)
do
   cp -r $directory usr/share/locale/
   rm usr/share/locale/${directory:3}/hunter.po
done
gprclean -P hunter.gpr
