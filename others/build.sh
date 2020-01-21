#!/bin/bash

releasedir=usr

gprclean -P hunter.gpr
gprbuild -P hunter.gpr -XMode=release
mkdir -p $releasedir/share/doc/hunter
mkdir -p $releasedir/share/locale
mkdir -p $releasedir/share/metainfo/
cp -r bin $releasedir/
cp CHANGELOG.md $releasedir/share/doc/hunter/
cp COPYING $releasedir/share/doc/hunter/
cp README.md $releasedir/share/doc/hunter/
cp CONTRIBUTING.md $releasedir/share/doc/hunter/
cp others/hunter.appdata.xml $releasedir/share/metainfo
others/translations.sh generate
for directory in $(find po/* -maxdepth 0 -type d)
do
   cp -r $directory $releasedir/share/locale/
   rm $releasedir/share/locale/${directory:3}/hunter.po
done
gprclean -P hunter.gpr
