#!/bin/sh

prefix=`dirname $0`
cd "$prefix"
prefix=`pwd`

if [ -d lib ]; then
   if [ -z XDG_DATA_DIRS ]; then
      export XDG_DATA_DIRS=/usr/share
   fi
   export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$prefix/lib
   export GDK_PIXBUF_MODULE_FILE=$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache
   export GDK_PIXBUF_MODULEDIR=$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders/
   export FONTCONFIG_FILE=$prefix/etc/fonts/fonts.conf
   export GSETTINGS_BACKEND=memory
else
   eval `gtkada-env.sh --print-only`
fi

cd bin
export RUNFROMSCRIPT=1
./hunter "$@"
