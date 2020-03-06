#!/bin/bash

releasedir=usr

gprclean -P hunter.gpr
gprbuild -P hunter.gpr -XMode=release
mkdir -p $releasedir/share/doc/hunter
mkdir -p $releasedir/share/metainfo/
cp -r bin $releasedir/
cp CHANGELOG.md $releasedir/share/doc/hunter/
cp COPYING $releasedir/share/doc/hunter/
cp README.md $releasedir/share/doc/hunter/
cp CONTRIBUTING.md $releasedir/share/doc/hunter/
cp others/hunter.appdata.xml $releasedir/share/metainfo
cp -r share $releasedir
gprclean -P hunter.gpr
