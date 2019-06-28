#!/bin/sh

xgettext --keyword=translatable -sj -o po/hunter.pot bin/ui/hunter.glade
xgettext --keyword=Gettext -sj -o po/hunter.pot src/*.*
