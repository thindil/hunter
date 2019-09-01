#!/bin/bash

case $1 in
   createpot)
      xgettext --keyword=translatable -s -o po/hunter.pot bin/ui/hunter.glade
      xgettext --package-name=Hunter --package-version=0.6 --msgid-bugs-address=thindil@laeran.pl --copyright-holder="Bartek thindil Jasicki" --keyword=Gettext -sj -o po/hunter.pot src/*.*
      echo "Translation template was regenerated."
      ;;
   createlocale)
      mkdir -p po/$2/LC_MESSAGES
      msginit -i po/hunter.pot -l $2 -o po/$2/hunter.po
      echo "Empty locale $2 was created."
      ;;
   update)
      for directory in $(find po/* -maxdepth 0 -type d)
      do
         msgmerge -U $directory/hunter.po po/hunter.pot
         echo "Locale ${directory:3} was updated."
      done
      ;;
   generate)
      for directory in $(find po/* -maxdepth 0 -type d)
      do
         /usr/bin/msgfmt -o $directory/LC_MESSAGES/hunter.mo $directory/hunter.po
         echo "Locale ${directory:3} was generated."
      done
      ;;
   help)
      echo "createpot                 - Regenerate translations template file"
      echo "createlocale [localename] - Create new localization file for selected locale"
      echo "update                    - Updated existing localization files with new strings from template file"
      echo "generate                  - Generate localizations files for the program"
      echo "help                      - This screen"
      ;;
   *)
      echo "Unknown command, possible options are: createpot, createlocale, update, generate, help"
      ;;
esac
