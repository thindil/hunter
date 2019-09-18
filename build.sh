#!/bin/bash

case $1 in
   release)
      gprclean -P hunter.gpr
      gprbuild -P hunter.gpr -XMode=release
      mkdir -p usr/share/docs
      mkdir -p usr/share/locale
      cp -r bin usr/
      cp CHANGELOG.md usr/share/docs/
      cp COPYING usr/share/docs/
      cp README.md usr/share/docs
      ./translations.sh generate
      for directory in $(find po/* -maxdepth 0 -type d)
      do
         cp -r $directory usr/share/locale/
         rm usr/share/locale/${directory:3}/hunter.po
      done
      gprclean -P hunter.gpr
      ;;
   debug)
      gprclean -P hunter.gpr
      gprbuild -P hunter.gpr
      ;;
   analyze)
      gprclean -P hunter.gpr
      gprbuild -P hunter.gpr -XMode=analyze
      ;;
   createtests)
      gnattest -P hunter.gpr
      ;;
   tests)
      gprbuild -P tests/driver/test_driver.gpr
      ;;
   docs)
      ./generatedocs.py
      ;;
   gcov)
      mkdir -p gcov
      cd gcov
      ~/gnat/bin/gcov -f ../obj/*.o
      cd ..
      ;;
   gprof)
      gprof bin/hunter bin/gmon.out > gprofreport.txt
      ;;
   help)
      echo "release       - Build the program in release mode"
      echo "debug         - Build the program in debug mode"
      echo "analyze       - Build the program in analyze mode"
      echo "createtests   - Regenerate unit tests"
      echo "tests         - Build unit tests"
      echo "docs          - Generate code documentation"
      echo "gcov          - Generate gcov reports for each file in gcov directory. You may need to change gcov path in this script to work"
      echo "gprof         - Generate gprof report in main directory"
      echo "help          - This screen"
      ;;
   *)
      echo "Unknown command, possible options are: release, debug, analyze, createtests, tests, docs, gcov, gprof, help"
      ;;
esac
