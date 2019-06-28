#!/bin/sh

xgettext --keyword=translatable -s -o po/hunter.pot bin/ui/hunter.glade
xgettext --keyword=Gettext -sj -o po/hunter.pot src/*.*
