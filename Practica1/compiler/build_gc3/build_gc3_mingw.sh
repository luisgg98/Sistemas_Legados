#!/bin/bash
###########################################################################
# build_gc3_mingw.sh - Build GnuCobol 3x and 4x for MinGW in Windoze		
# -------------------------------------------------------------------------
# This script should work for current and future versions of GnuCobol, 
# with only minor modification to the parameters and script.
#
# The script MUST be invoked from Windoze by "build_gc3_mingw.bat". Please
# see that script for additional and essential procedural documentation.
# -----------------------------------------------------------------------
# Author: Carl D. Goldin <cdgoldin@gmail.com> - copyright 2019-2020
# Source: Based on Arnold Trembly's GnuCobol Build guide
#         (https://www.arnoldtrembley.com/GnuCOBOL.htm)
#         and very helpful information from Anon, Simon Sobisch, David Wall,
#         Ken Martin, and others on the GnuCobol forum:
#         https://sourceforge.net/p/open-cobol/discussion/help/
#
# Date-written:  12/26/2019
# Last-revision:  7/03/2020
#
# Please report any bugs and/or suggestions to cdgoldin@gmail.com. Thank you.
#
###########################################################################
# About a dozen environmental variables are set by the Windoze script, and
# and passed to this script via "EXPORT" commands. If you don't see a 
# variable defined below, it SHOULD be defined in the Windoze script. A
# list of exported variables is printed to the screen at program start time.
###########################################################################
msysfolder="/mingw/msys" #do NOT change this variable!

printf "\nBegin MSYS bash script to build $gcversion\n" 

###########################################################################
# global functions
###########################################################################
function pause ()
{ read -p "Press any key to continue" pause
  echo "";
}

function press-any-key ()
{ echo "";
  read -p "Press any key to continue - Control C to cancel" pause
  echo "";
}

function check-for-errors ()
{ echo "";
  read -p "Check for errors - Press any key to continue - Control C to cancel" pause
  echo "";
}

function yes_or_no () #messagetext
{ echo " " 
  reply="no";
  while [[ "$reply" != "y" && "$reply" != "n" ]]
  do 
    read -p "$1? y/n " reply;
    if [ "$reply" == "Y" ]; then reply = "y"; fi;
    if [ "$reply" == "N" ]; then reply = "n"; fi;
  done
}

another="a";

function grab_a_beer ()
{ printf "grab $another beer";
  another="another";
} 

function checkpoint ()
{ echo "Creating checkpoint folder $MinGWfolder-$1 for restarting and/or alternate builds - please wait"
  if [ -d $MinGWfolder-$1 ]; then rm -Rf $MinGWfolder-$1; fi
  mkdir $MinGWfolder-$1
  cp -R $MinGWfolder/* $MinGWfolder-$1
  echo " "
}

##########################################################################
# Download and install unzip.exe (needed for unzipping VBISAM...zip)
##########################################################################
uzpversion="unzip-6.0-1"
url="https://sourceforge.net/projects/mingw/files/MSYS/Extension/unzip/$uzpversion/$uzpversion-msys-1.0.13-bin.tar.lzma/download"

if [ ! -f $msysfolder/bin/unzip.exe ]; then 
   printf "\nDownloading $uzpversion\n\n" 
   wget -O "$tarfolder/$uzpversion.tar.lzma" "$url"
   if [ $? -gt 0 ]; then 
       echo " "
       echo "Download of $uzpversion.tar.lzma failed - correct problem and restart\n"
       pause; exit 8
   fi

   cd $msysfolder 

   printf "\nUnpacking $uzpversion\n"
   tar --lzma -xpf $tarfolder/$uzpversion.tar.lzma bin/unzip.exe
 
   if [ $? -gt 0 ]; then
      printf "\nUnpack of $uzpversion.tar.lzma failed - correct problem and restart\n\n"
      pause; exit 8
   fi
       
   rm -i $tarfolder/$uzpversion.tar.lzma
fi

##########################################################################
# Install GMP (GNU Multiple-Precision Arithmetic Library) 
##########################################################################
if [ "$MATHLIB" == "GMP" ]; then
   ls /mingw/bin/libgmp*.dll > /dev/null
   if [ $? -gt 0 ]; then 
      reply="y";
   else   
      yes_or_no "Re-install GMP (GNU Multiple-Precision Arithmetic Library)";
   fi
else
   reply="n";   
fi   

if [ "$reply" == "y" ]; then
   printf "\nUnpacking $gmpversion tarball - this should take 1-2 minutes\n"
   cd $msysfolder      
   tar xf $tarfolder/$gmpversion.tar.xz  
   if [ $? -gt 0 ]; then
      echo "Unpack of $tarfolder/$gmpversion.tar.xz failed - correct problem and restart\n"
      pause; exit 8; fi
      
   rm -i $tarfolder/$gmpversion.tar.xz     

   if [ ! -d $msysfolder/$gmpversion ]; then
      echo "$msysfolder/$gmpversion does not exist - cancelling"
      pause; exit 8
   fi

   echo " "
   echo "Installing GMP"
   echo "configure will take about 5 minutes and produce hundreds of messages"
   echo "'make' will take about 10 minutes and produce thousands of messages"
   echo "'make check' will take about 15 minutes and produce thousands of messages"
   printf "'make install' runs very quickly --- so "; grab_a_beer
   printf " and wait.\n"
   press-any-key
   
   cd $msysfolder/$gmpversion           
   ./configure --prefix=/mingw --disable-static --enable-shared  
   if [ $? -gt 0 ]; then check-for-errors; fi  
   make;         if [ $? -gt 0 ]; then check-for-errors; fi
   make check;   if [ $? -gt 0 ]; then check-for-errors; fi
   make install; if [ $? -gt 0 ]; then check-for-errors; fi

   ls /mingw/bin/libgmp*.dll > /dev/null
   if [ $? -gt 0 ]; then 
      echo "/mingw/bin/libgmp*.dll was not created - cancelling" 
      pause; exit 8
   fi
   
   echo " "
   checkpoint "GMP"
fi

##########################################################################
# Install MPIR (Multiple Precision Integers and Rationals)
##########################################################################
if [ "$MATHLIB" == "MPIR" ]; then
   ls /mingw/bin/libmpir*.dll > /dev/null
   if [ $? -gt 0 ]; then 
      reply="y"; 
   else   
      yes_or_no "Re-install MPIR (Multiple Precision Integers and Rationals)"
   fi
else
   reply="n"; 
fi   
    
if [ "$reply" == "y" ]; then
   printf "\nUnpacking $mpirversion tarball - this may take a while - "; grab_a_beer
   printf ".\n"
   cd $msysfolder
   tar xjf $tarfolder/$mpirversion.tar.bz2
   if [ $? -gt 0 ]; then
      echo "Unpack of $tarfolder/$mpirversion.tar.bz2 failed  - correct problem and restart\n"
      pause; exit 8; fi 
   
   rm -i $tarfolder/$mpirversion.tar.bz2
 
   if [ ! -d $msysfolder/$mpirversion ]; then
      echo "$msysfolder/$mpirversion does not exist - cancelling"
      pause; exit 8
   fi    

   echo " "
   echo "Installing MPIR"
   echo "configure will take about 5 minutes and produce hundreds of messages"
   echo "'make' will take about 10 minutes and produce thousands of messages"
   echo "'make check' will take about 15 minutes and produce thousands of messages"
   printf "'make install' runs very quickly --- so "; grab_a_beer
   printf ".\n"
   press-any-key
 
   cd $msysfolder/$mpirversion      
   ./configure --prefix=/mingw --disable-static --enable-shared --with-yasm=$msysfolder/$yasmversion
   # removed "--enable-gmpcompat" here and added "--with-math=mpir" below on 7/2/2020 as per Simon's suggestion
   if [ $? -gt 0 ]; then check-for-errors; fi  
   make;         if [ $? -gt 0 ]; then check-for-errors; fi      
   make check;   if [ $? -gt 0 ]; then check-for-errors; fi
   make install; if [ $? -gt 0 ]; then check-for-errors; fi

   ls /mingw/bin/libmpir*.dll > /dev/null
   if [ $? -gt 0 ]; then 
      echo "/mingw/bin/libmpir*.dll was not created - cancelling" 
      pause; exit 8
   fi
   
   echo " "
   checkpoint "MPIR"
fi
   
##########################################################################
# Install Berkely Database (BDB) 
##########################################################################
if [ "$ISAM" == "BDB" ]; then
   if [ ! -f $MinGWfolder/bin/libdb-6.2.dll ]; then 
      reply="y"
   else
      yes_or_no "Re-install Berkely Database (BDB)"
   fi   
else
   reply="n";   
fi   
    
if [ "$reply" == "y" ]; then
   echo " "
   echo "Unpacking $BDBversion tarball - this should take a few minutes"
   cd $msysfolder       
   tar xzf $tarfolder/$BDBversion.tar.gz   
   if [ $? -gt 0 ]; then
      echo "Unpacking of $tarfolder/$BDBversion.tar.gz failed  - correct problem and restart\n"
      pause; exit 8; fi
   
   rm -i $tarfolder/$BDBversion.tar.gz        
   
   printf "\nEdit os_stat.c, replacing '_tcsclen' with 'strlen'\n"
   sed -i 's/_tcsclen/strlen/g' $msysfolder/$BDBversion/src/os_windows/os_stat.c 
   printf "\ndouble-check that the change was made\n"
   grep "strlen" $msysfolder/$BDBversion/src/os_windows/os_stat.c
   pause 

   printf "\nInstalling BDB\n"
   echo "The 'configure' step runs fairly quickly and produces hundreds of messages."
   echo "The 'make' step runs for a very long time. Grab a six pack."
   echo "The 'make install' step runs very quickly."
   echo "If the BDB build appears to hang at "Installing documentation", let it run"
   echo "You should have time for a beer or two"
   press-any-key
   
   cd $msysfolder/$BDBversion/build_unix               
   ../dist/configure --enable-mingw --prefix=/mingw --enable-compat185 LIBCSO_LIBS=-lwsock32
   if [ $? -gt 0 ]; then check-for-errors; fi 
   make;         if [ $? -gt 0 ]; then check-for-errors; fi            
   make install; if [ $? -gt 0 ]; then check-for-errors; fi
 
   if [ ! -f $MinGWfolder/bin/libdb-6.2.dll ]; then 
      echo $MinGWfolder/bin/libdb-6.2.dll was not created - cancelling
      pause; exit 8; fi
    
   echo " "
   if [ $MATHLIB == "GMP" ]; then checkpoint "GMP-BDB"
                             else checkpoint "MPIR-BDB"; fi
fi

##########################################################################
# Install VBISAM database (VBI)
##########################################################################
if [ "$ISAM" == "VBISAM" ]; then
   if [ ! -f $MinGWfolder/bin/libvbisam-1.dll ]; then
      reply="y"
   else     
      yes_or_no "Re-install VBISAM database (VBI)"
   fi   
else
   reply="n";   
fi

if [ "$reply" == "y" ]; then
  echo "Unpacking $VBIversion tarball - this should take a few minutes"
  cd $msysfolder       
  bin/unzip.exe -q -o $tarfolder/$VBIversion.zip
  if [ $? -gt 0 ]; then
      printf "\nUnzip of $tarfolder/$VBIversion.zip failed - correct problem and restart\n"
      pause; exit 8; fi
  
   rm -i $tarfolder/$VBIversion.zip        

   printf "\nInstalling VBISAM\n"
   echo "The 'configure' step runs fairly quickly and produces about 100 messages"
   echo "The 'make' step runs longer and produces more messages."
   echo "The 'make check' step runs very quickly."
   echo "The 'make install' step should also run very quickly."
   echo "So no time for a beer :-("
   press-any-key
   
   cd $msysfolder/$VBIversion             
  ./configure --prefix=/mingw     
   if [ $? -gt 0 ]; then check-for-errors; fi
   make;         if [ $? -gt 0 ]; then check-for-errors; fi             
   make check;   if [ $? -gt 0 ]; then check-for-errors; fi
   make install; if [ $? -gt 0 ]; then check-for-errors; fi

   if [ ! -f $MinGWfolder/bin/libvbisam-1.dll ]; then
      printf "\n$MinGWfolder/bin/libvbisam-1.dll was not created - cancelling\n"
      pause; exit 8; fi
    
   echo " "
   if [ $MATHLIB == "GMP" ]; then checkpoint "GMP-VBISAM"
                             else checkpoint "MPIR-VBISAM"; fi
fi

##########################################################################
# Remove previous curses installation(s)
##########################################################################
function remove_previous_curses ()
{ printf "\nRemoving previous curses installation(s)\n"
     
  for file in /mingw/bin/pdcurses.dll    \
              /mingw/include/curses.h    \
              /mingw/include/PDCurses*.h \
              /mingw/include/pdcurses.h  \
              /mingw/lib/libpdcurses.a   \
              /mingw/msys/PDCurses*      \
              /mingw/bin/libcob-4.dll    \
              /mingw/bin/libcob-5.dll    
#             /mingw/lib/libcob.a        \
#             /mingw/lib/libcob.dll.a    \
#             /mingw/lib/libcob.la       \
  do
    if [[ -f "$file" || -d "$file" ]]; then rm -R "$file"; fi 
  done
}

##########################################################################
# Install PDCurses
##########################################################################
function install_PDCurses ()
{ 
if [ ! -d $msysfolder/$curses ]; then
   reply="y"
else     
   #yes_or_no "Re-install $curses"
   reply="y" #always reinstall it!
fi   

if [ "$reply" == "y" ]; then
   remove_previous_curses
    
   printf "\nUnpacking $curses tarball - this should only take a few seconds\n"
   
   cd $msysfolder
   folder=$tarfolder/$curses.tar.gz
   tar xzf $folder
   if [ $? -gt 0 ]; then
      printf "\nUnpack of $folder failed  - correct problem and restart\n"
      pause; exit 8; fi
      
   rm -i $folder
        
   #if [[ "$curses" == "PDCurses-4.1.0" && -f "$(dirname "$0")/newtest.c" ]]; then
   #   cp "$(dirname "$0")/newtest.c" "$msysfolder/$curses/demos/newtest.c"
   #fi # but we still get errors, so add "libs" to makeparms    
   
   echo " "
   echo "Building $curses"
   echo "'make' will take a few more seconds"
   echo "'install' runs very quickly --- so no time for a beer."
   #press-any-key

   cd $msysfolder

   subdir=""

   if [ $curses == "PDCurses-3.4" ];   then subdir="/win32";  fi
   if [ $curses == "PDCurses-3.9" ];   then subdir="/wincon"; fi
   if [ $curses == "PDCurses-4.1.0" ]; then
      if [ $wingui == "no" ];            then subdir="/wincon"; fi
      if [ $wingui == "yes" ];           then subdir="/wingui"; fi
   fi   

   cd $curses$subdir

   makeparms=""

   if [ $curses == "PDCurses-3.4" ]; then makeparms="gccwin32.mak DLL=Y"; fi
   if [ $curses == "PDCurses-3.9" ]; then makeparms="Makefile INFOEX=N DLL=Y"; fi
   if [ $curses == "PDCurses-4.1.0" ]; then
      makeparms="Makefile.mng libs INFOEX=N DLL=Y";
      if [ $wide == "yes" ];      then makeparms="$makeparms WIDE=Y"; fi  
      if [ $CHTYPE_32 == "yes" ]; then makeparms="$makeparms CHTYPE_32=Y"; fi
      if [ $UTF8 == "yes" ];      then makeparms="$makeparms UTF8=Y"; fi 
   fi    
   
   echo "makeparms=$makeparms" #debug, but no reason not to leave permanently
   
   make -f $makeparms; if [ $? -gt 0 ]; then check-for-errors; fi
   
   ls pdcurses.dll > /dev/null
   if [ $? -gt 0 ]; then 
      echo "pdcurses.dll was not created - cancelling"  
      pause; exit 8; fi

   install pdcurses.dll /mingw/bin/.  
   if [ $? -gt 0 ]; then check-for-errors; fi
   install pdcurses.a /mingw/lib/libpdcurses.a
   if [ $? -gt 0 ]; then check-for-errors; fi
   cd ..
   install *.h /mingw/include/.              
   if [ $? -gt 0 ]; then check-for-errors; fi    
   install curses.h /mingw/include/$curses.h
   if [ $? -gt 0 ]; then check-for-errors; fi 
   
   echo " " 
fi
}

##########################################################################
# Install ncurses 6.0.2
##########################################################################
function install_ncurses_6.0 ()
{
if [ ! -d $msysfolder/$curses ]; then # WRONG TEST!!!
   reply="y"
else     
   #yes_or_no "Re-install $curses"
   reply="y" #always reinstall it!   
fi   

if [ "$reply" == "y" ]; then
   remove_previous_curses
    
   printf "\nUnpacking $curses tarballs - this should only take a few minutes\n"
   
   cd $MinGWfolder
   for folder in    ncurses-6.0-2-mingw32-bin.tar.lzma   \
                   terminfo-6.0-2-mingw32-data.tar.lzma  \
                 libncurses-6.0-2-mingw32-dev.tar.lzma   \
                 libncurses-6.0-2-mingw32-dll-6.tar.lzma
   do
     printf "\nUnpacking $folder\n"
     tar --lzma -xpf $tarfolder/$folder
     if [ $? -gt 0 ]; then
        printf "\nUnpack of $tarfolder/$folder failed  - correct problem and restart\n"
        pause; exit 8; fi
     rm -i $tarfolder/$folder
   done  
  
   echo " " 
fi
}

##########################################################################
# Install ncurses 6.1
##########################################################################
function install_ncurses_6.1 ()
{ 
if [ ! -d $msysfolder/$curses ]; then   #Wrong test?
   reply="y"
else     
   #yes_or_no "Re-install $curses"
   reply="y" #always reinstall it!
fi   

if [ "$reply" == "y" ]; then
   remove_previous_curses
    
   printf "\nUnpacking $curses tarball - this should only take a few minutes\n"
   
   cd $msysfolder
   folder=$tarfolder/ncurses-snapshots-master.zip
   bin/unzip.exe -q -o $folder
   if [ $? -gt 0 ]; then
      printf "\nUnzip of $folder failed  - correct problem and restart\n"
      pause; exit 8; fi
      
   rm -i $folder
        
   echo " "
   echo "Building $curses"
   echo "'configure' runs fairly quickly, but "
   echo "'make' will take a while  - so grab a beer"
   echo "'install' runs very quickly --- so no time for a beer."
   #press-any-key

   cd $msysfolder/ncurses-snapshots-master
   
   export PATH_SEPARATOR=";"

   ./configure \
	--prefix=/mingw \
	--with-cxx \
	--without-ada \
	--enable-warnings \
	--enable-assertions \
	--disable-home-terminfo \
	--enable-database \
	--enable-sp-funcs \
	--enable-term-driver \
	--enable-interop \
	--disable-termcap \
	--with-progs \
	--with-libtool \
	--enable-pc-files \
	--mandir=/mingw/share/man

                 if [ $? -gt 0 ]; then check-for-errors; fi  
   make;         if [ $? -gt 0 ]; then check-for-errors; fi
   make check;   if [ $? -gt 0 ]; then check-for-errors; fi
   make install; if [ $? -gt 0 ]; then check-for-errors; fi

#   ls /mingw/bin/libxml2*.dll > /dev/null
#   if [ $? -gt 0 ]; then 
#      echo "/mingw/bin/libxml2*.dll was not created - cancelling" 
#      exit 8
#   fi
   
   echo " " 
fi
}

##########################################################################
# Install PDCurses or ncurses
##########################################################################
if [ "${curses:0:3}" == "PDC" ];     then install_PDCurses;   fi
if [ "$curses" == "ncurses-6.0-2" ]; then install_ncurses_6.0; fi
if [ "$curses" == "ncurses-6.1" ];   then install_ncurses_6.1; fi

##########################################################################
# Build libXML2
##########################################################################
xml2version="libxml2-2.9.10"  #has 4 errors during configure/make/install

if [ "$XML2" == "yes" ]; then
   ls /mingw/bin/libxml2*.dll > /dev/null
   if [ $? -gt 0 ]; then
      reply="y"
   else    
      yes_or_no "Re-install XML2"
   fi   
else
   reply="n";   
fi   

if [ "$reply" == "y" ]; then
   if [ -d $msysfolder/$xml2version ]; then 
      echo "removing existing $xml2version folder - this may take a while"
      rm -R $msysfolder/$xml2version
   fi
   mkdir $msysfolder/$xml2version #start with a clean folder
   cd $msysfolder/$xml2version

   printf "\nDownloading $xml2version.tar.gz - this should take less than a minute\n\n"
   wget -O "$tarfolder/$xml2version.tar.gz" "ftp://xmlsoft.org/libxml2/$xml2version.tar.gz"
   if [ $? -gt 0 ]; then 
      printf "\nPlease download $tarfolder/$xml2version.tar.gz manually and restart\n"
      exit 8
   fi

   printf "\nUnpacking $tarfolder/$xml2version.tar.gz - this should take 1-2 minutes\n\n"
   echo " "
   tar xf $tarfolder/$xml2version.tar.gz  
   if [ $? -gt 0 ]; then
      printf "\nPlease download $tarfolder/$xml2version.tar.gz manually\n"
      exit 8
   fi
       
   rm -i $tarfolder/$xml2version.tar.gz
   
   printf "\nInstalling XML2 - This may take a while --- so "; grab_a_beer
   
   press-any-key
   
   cd $msysfolder/$xml2version/$xml2version #sic! this is how it got unpacked by tar!
   ./configure --prefix=/mingw  
   if [ $? -gt 0 ]; then check-for-errors; fi  
   make;         if [ $? -gt 0 ]; then check-for-errors; fi
   make check;   if [ $? -gt 0 ]; then check-for-errors; fi
   make install; if [ $? -gt 0 ]; then check-for-errors; fi

   ls /mingw/bin/libxml2*.dll > /dev/null
   if [ $? -gt 0 ]; then 
      echo "/mingw/bin/libxml2*.dll was not created - cancelling" 
      exit 8
   fi
   
   echo " "
fi

##########################################################################
# Install the $gcversion compiler
##########################################################################
echo " "

if [ ! -d $msysfolder/$gcversion ]; then
   reply="y"
else     
   yes_or_no "Re-install the $gcversion compiler"
fi   

if [ "$reply" == "y" ]; then
   printf "\nRemoving previous GnuCOBOL installation(s)\n"
   
   for folder in $msysfolder/gnucobol*
   do
     if [ -f $folder ]; then 
       echo $folder
       cd $folder
       make uninstall
       make distclean
     fi  
   done
   
   printf "\nUnpacking $gcversion - this should only take a few minutes\n"

   cd $msysfolder       
   tar xf $tarfolder/$gcversion.tar.gz
   if [ $? -gt 0 ]; then
      echo "Unpacking of $tarfolder/$gcversion.tar.gz failed  - correct problem and restart\n"
      pause; exit 8; fi 
   
   rm -i $tarfolder/$gcversion.tar.gz
   
   #if newcob.val is corrupt, remove it and let "make test" download and unpack it
   if [ -f $msysfolder/$gcversion/tests/cobol85/newcob.val ]; then 
      grep "IDENTIFICATION DIVISION." $msysfolder/$gcversion/tests/cobol85/newcob.val > /dev/null
      if [ $? -gt 0 ]; then rm        $msysfolder/$gcversion/tests/cobol85/newcob.val; fi
   fi   
      
   if [ "$cJSON" == "yes" ]; then
      echo " "
      echo "Downloading cJSON.c and cJSON.h"
      wget -O $msysfolder/$gcversion/libcob/cJSON.c https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.c 
      if [ $? -gt 0 ]; then
         rm $msysfolder/$gcversion/libcob/cJSON.c #because wget creates an empty file 
         printf "\nDownload of $cJson.c failed - correct problem and restart\n"
         pause; exit 8
      fi
      wget -O $msysfolder/$gcversion/libcob/cJSON.h https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.h 
      if [ $? -gt 0 ]; then
         rm $msysfolder/$gcversion/libcob/cJSON.h #because wget creates an empty file 
         printf "\nDownload of $cJson.h failed - correct problem and restart\n"
         pause; exit 8
      fi
   fi        
        
   echo " "
   echo "Installing $gcversion"
   echo "The 'configure' step runs for less than one minute, producing 1-2 screens of messages."
   echo "The 'make' step runs for about 2 minutes, generating hundreds of messages."
   echo "The 'make check' step perform 1015 basic tests, and takes 10-15 minutes to run"
   echo "The 'make test' step generates hundreds of messages, and runs for 5-10 minutes"
   echo "The 'make install' step should run in a minute or less"
   echo "You should have time for a six pack"
   press-any-key
   
                                    options="--without-db"
   if [ "$ISAM" == "BDB" ];    then options="--with-db"; fi
   if [ "$ISAM" == "VBISAM" ]; then options="--with-vbisam"; fi
   
   if [ "${curses:0:3}" == "PDC" ]; then options="$options --with-curses=pdcurses"
                                    else options="$options --with-curses=ncurses"; fi
   
   if [ "$cJSON" == "yes" ]; then options="$options --with-cjson"
   else                           options="$options --without-cjson"; fi
   
   if [ "$XML2" == "yes" ]; then options="$options --with-xml2"
   else                          options="$options --without-xml2"; fi
   
   if [ "$MATHLIB" == "MPIR" ]; then options="$options --with-math=mpir";fi
   # removed "--enable-gmpcompat" above; added "--with-math=mpir" here on 7/2/2020 as per Simon's suggestion
   echo "$options"
   
   cd $msysfolder/$gcversion
   
   ./configure --prefix=/mingw $options --disable-rpath
   if [ $? -gt 0 ]; then check-for-errors; fi 
   make;         if [ $? -gt 0 ]; then check-for-errors; fi            
   make check;   if [ $? -gt 0 ]; then check-for-errors; fi
   make test;    if [ $? -gt 0 ]; then check-for-errors; fi
   make install; if [ $? -gt 0 ]; then check-for-errors; fi
   
   echo " "
fi

##########################################################################
# Just when you thought it would never end ...
##########################################################################
printf "Please close the MSYS window, and return to the Windoze script "
printf "to complete the installation\n"

exit
