#!/bin/bash
#####################################################################
# build_gc3_linux.sh - Build GnuCobol 3x and 4x in Linux
# ------------------------------------------------------------------
# This script should work for current and future versions of GnuCobol,
# with only minor modification to the parameters defined below, and 
# possible modifications to the "apt install" list.
#
# Syntax: build_gc3_linux.sh [-ccobfolder]
#                            [-ttarfolder]
#                            [-llibfolder]
#                            [-ssourcefolder]
#                            [BDB | VBISAM | NODB] [GMP | MPIR]
#                            [WIDE] [cJSON] [XML2] [GC31 | GC3DEV | GC4DEV]
#
# Defaults:
#      cobfolder=/home/$USER/cob   #folder where tarballs will be unpacked 
#      tarfolder=/home/$USER/cob"  #folder where tarballs will be downloaded
#      libfolder=/usr/local        #folder where libraries will be installed
#      sourcefolder=;              #folder where source (test) programs reside
#
#      specifying different libfolders allows different installations to co-exist 
#       
#      BDB MPIR GC31
#
# All command-line parameters are optional. Folder names are case sensitive.
# The parameters may be entered in any order. 
#
# Command-line parameters override the script variables defined below. In
# general, command-line parameters are used to override variables that are
# likely to change for each user, and for users who do multiple builds. 
# Variables that are more static (such as program versions and download
# urls) must be changed in the script.
# ------------------------------------------------------------------
# Author: Carl D. Goldin <cdgoldin@gmail.com> - copyright 2019-2020.
# Source: Based on very helpful information from Anon, Simon Sobisch, David Wall,
#         Ken Martin, and others on the GnuCobol forum:
#         https://sourceforge.net/p/open-cobol/discussion/help/
#
# Date-written:  12/25/2019
# Last-revision: 07/03/2020
#
# The script has been thoroughly tested with Debian 10.2, Kali Linux 2019.4,
# Linux Lite 4.6, Linux Mint 19.3 cinnamon, mate and XFCE, Linux Mint 20 mate, 
# Ubuntu 18.04 Server, Ubuntu 18.04.03 Gnome, Ubuntu 19.10 Mate, Ubuntu 19.10 
# server, Ubuntu Focal 20.04, Xubuntu 19.10 and Zorin 15.1 Lite.
#
# [Please note that Kali users must add
#      deb https://http.kali.org/kali kali-rolling main main
#      deb-src https://http.kali.org/kali kali-rolling main main
#      to the end of /etc/apt/sources.list. and then execute "sudo apt update"
#      For 'sudo apt' to work correctly.]
#
# Please report any bugs and/or suggestions to cdgoldin@gmail.com.
# Thank you.
#
#########################################################################
# If you need/want to change the script, keep in mind that:
# All parameter variables are case-sensitive.
# Make sure you don't set any to NUL (i.e. set variable=), and make sure 
# you don't have any leading or trailing spaces (which will be treated as
# part of the variable), creating very difficult-to-debug problems.
#########################################################################

# Parameters that can be overridden on the command line
# -----------------------------------------------------
cobfolder="/home/$USER/cob";    #folder for unpacking tarballs
tarfolder="/home/$USER/cob";    #folder to receive tarballs
                    #Do NOT use "~/folder" - it won't work with this script!
libfolder="/usr/local";         #folder to install libraries
#This allows different library folders for different versions of Gnu Cobol
#Note - unlike the Windoze installation, the actual applications and their
#libraries do NOT get installed in "cobfolder", but rather in "libfolder"

sourcefolder=;  #folder in which Cobol programs for testing reside          
         
ISAM="BDB";                     #BDB, VBISAM or NODB
MATHLIB="MPIR";                 #MPIR or GMP

WIDE="no";                      #no=don't use WIDE, yes=use WIDE
                                #not supported with ncurses yet
                                    
cJSON="no";                     #no=don't install cJSON, yes=install cSJON
     
XML2="no";                      #no=don't install XML2, yes=install XML2

GCV="GC31";                     #GNU Cobol version to install
        
# Parameters that can only be changed by editing the script
# ---------------------------------------------------------
  gc31_version="gnucobol-3.1-rc1"
gc3dev_version="gnucobol-3.1-dev"
gc4dev_version="gnucobol-4.0-early-dev"

mpirversion="mpir-3.0.0";       #mpir version to install
gmpversion="gmp-6.1.2";         #GMP  version to install
BDBversion=db-6.2.38.NC         #BDB version to install
VBIversion=vbisam-2.0.1         #VBISAM version to install

#####################################################################
# global functions
#####################################################################
function pause ()
{ read -p "Press any key to continue" pause
  echo "";
}

function press_any_key ()
{ echo "";
  read -p "Press any key to continue - Control C to cancel " pause
  echo "";
}

function check_for_errors ()
{ echo "";
  read -p "Check for errors - Press any key to continue - Control C to cancel" pause
  echo "";
}

function yes_or_no () #messagetext
{ reply="no";
  while [[ "$reply" != "y" && "$reply" != "n" ]]
  do 
    read -p "$1? y/n " reply;
    if [ "$reply" == "Y" ]; then reply="y"; fi;
    if [ "$reply" == "N" ]; then reply="n"; fi;
  done
}

function get_parms () 
{ if [ ! "$parm" ]; then return; fi #no parameter

  rawparm="$parm"; parm=${parm^^} #upshift parameter
  
  if [ "$parm" == "BDB" ];    then ISAM="BDB";     return; fi
  if [ "$parm" == "VBISAM" ]; then ISAM="VBISAM";  return; fi
  if [ "$parm" == "NODB" ];   then ISAM="NODB";    return; fi
  
  if [ "$parm" == "MPIR" ];   then MATHLIB="MPIR"; return; fi
  if [ "$parm" == "GMP" ];    then MATHLIB="GMP";  return; fi
  
  if [ "$parm" == "WIDE" ];   then WIDE="yes";     return; fi
  
  if [ "$parm" == "CJSON" ];  then cJSON="yes"; return; fi
    
  if [ "$parm" == "XML2" ];   then XML2="yes";  return; fi
       
  if [ "$parm" == "GC31" ];   then GCV=GC31;    return; fi
  if [ "$parm" == "GC3DEV" ]; then GCV=GC3DEV; return; fi
  if [ "$parm" == "GC4DEV" ]; then GCV=GC4DEV; return; fi
     
  if [ ${parm:0:2} == -C ]; then cobfolder=${rawparm:2}; return; fi
  if [ ${parm:0:2} == -T ]; then tarfolder=${rawparm:2}; return; fi
  if [ ${parm:0:2} == -L ]; then libfolder=${rawparm:2}; return; fi
  if [ ${parm:0:2} == -S ]; then sourcefolder=${rawparm:2}; return; fi
    
  printf "\nInvalid parameter $rawparm\n"; 
  return 1
}

another="a";   # this is a global, not local, variable!

function grab_a_beer ()
{ printf "\nThis will take some time - grab $another beer";
  another="another\n";
} 

function mkdir_error () #directory name
{ 
  #if [ "$DISTRIB_ID" == "LinuxMint" ]; then
     echo "Trying 'sudo mkdir $1'"
     sudo mkdir "$1"
     if [ $? -eq 0 ]; then return 0; fi
  #fi   
  echo " "
  echo "Error creating $1 - Correct problem manually and restart"
  echo "If the directory is in root, you need to use 'sudo mkdir'"
  echo " "
  pause; exit 8
}
 
#####################################################################
# Create set_env#.sh desktop file
#####################################################################
file="nul"; #define file here, so it is global

function echo_to_file () # (ev, value, switch)
{ 
echo "if [ \"\$$1\" ]; then colon=\":\"; else colon=\"\";fi" >> $file
echo "echo \$$1 | grep $2 > /dev/null"                       >> $file
echo "if [ \$? -eq 1 ]; then export $1=$2\$colon\$$1; fi"    >> $file
echo " "                                                     >> $file 
}

function Create_set_env_file
{
seqno=0; file="/home/$USER/Desktop/set_env"
while [ -f $file$seqno.sh ]; do seqno=$((seqno + 1)); done
file=$file$seqno.sh

echo "#!/bin/bash"                                                > $file
echo "# Set environment for $gcversion installed in $libfolder"  >> $file
echo "#     MATHLIB=$MATHLIB ISAM handler=$ISAM"                 >> $file
echo "#--------------------------------------------------------" >> $file 
echo "# Usage: open a terminal window, and enter 'source $file'" >> $file
echo "# When you close the terminal window, it will 'unsource'"  >> $file
echo "# the four variables. If you want to be able to 'unsource'">> $file
echo "# without closing the terminal window, start a new (sub)shell" >> $file
echo "# first and source the script in the subshell. Then to reset"  >> $file
echo "# the environment to what it was before, just exit the subshell." >> $file
echo "# Feel free to rename the script to something more meaningful," >> $file
echo "# or move it to a different folder, if you wish"           >> $file
echo " "                                                         >> $file
       
echo "if [ \"\$0\" != \"bash\" ]; then"                          >> $file
echo "   echo \"This script must be run by entering 'source $file'\""  >> $file
echo "   read -p \"press any key \""                             >> $file
echo "   exit 1"                                                 >> $file
echo "fi"                                                        >> $file
echo " "                                                         >> $file

# As counter-intuitive as this seems, if the Desktop script is clicked on,
# $0 will be the script path. If you type the script name in a terminal,
# $0 will be the script name. If you type "bash scriptname" in a terminal,
# $0 will still be (just) the script name. But if you type "source scriptname",
# $0 will be "bash"!  
       
echo "export COB_CONFIG_DIR=$libfolder/share/gnucobol/config"    >> $file
echo "export COB_COPY_DIR=$libfolder/share/gnucobol/copy"        >> $file
echo "export COBCPY=; # [add path to your copylib files]"        >> $file
echo " "                                                         >> $file
echo_to_file "LD_LIBRARY_PATH" "$libfolder/lib"                  
echo_to_file "LD_RUN_PATH"     "$libfolder/lib"                  
echo_to_file "LIBDIR"          "$libfolder/lib"                  
echo_to_file "PATH"            "$libfolder/bin"                  

echo " "                                                         >> $file
echo "printf \"\\nenvironment updated for $libfolder\\n\\n\""    >> $file

chmod 777 $file
                   
echo " "
echo "Please 'source $file' before compiling or executing your"
echo "GnuCOBOL programs in a terminal window. If you wish, you can add" 
echo "it to ~/.bashrc so that it will be executed automatically when you"
echo "open a terminal window. You can also rename it to something more" 
echo "meaningful, or move it to a different folder, if you wish."
          
press_any_key
} 
#####################################################################
# Prelimaries
#####################################################################
for i in {1..9}
 do parm="$1"
    get_parms
    if [ $? -gt 0 ]; then pause; exit 8; fi 
    shift  
 done

if [ "$1" ]; then 
  printf "\nInvalid parameter $1\n"; 
  pause; exit 8
fi

if [ "$GCV" == "GC31" ];   then gcversion=$gc31_version; fi
if [ "$GCV" == "GC3DEV" ]; then gcversion=$gc3dev_version; fi
if [ "$GCV" == "GC4DEV" ]; then gcversion=$gc4dev_version; fi

DISTRIB_ID=$(cat /etc/*-release 2>/dev/null | grep "DISTRIB_ID")

if [ "$DISTRIB_ID" ]; then
   DISTRIB_ID=${DISTRIB_ID:11} #get the DISTRIB_ID value from  DISTRIB_ID=xxx 
else
   PRETTY_NAME=$(cat /etc/*-release 2>/dev/null | grep "PRETTY_NAME")
   PRETTY_NAME="${PRETTY_NAME:13}"      #get the PRETTY_NAME value
   DISTRIB_ID="${PRETTY_NAME%% *}"      #extract the first word only
   if [ "${PRETTY_NAME:0:7}" == "Red Hat" ]; then DISTRIB_ID="RedHat"; fi
   if [ "${PRETTY_NAME:0:10}" == "Linux Mint" ]; then DISTRIB_ID="LinuxMint"; fi
fi   

#To the best of my knowledge:
#CentOS:    DISTRIB_ID=;          PRETTY_NAME="CentOS..."
#Debian:    DISTRIB_ID=;          PRETTY_NAME="Debian..."
#Fedora:    DISTRIB_ID=Fedora;    PRETTY_NAME="Fedora...""
#Kali:      DISTRIB_ID=;          PRETTY_NAME="Kali ..."
#LinuxLite: DISTRIB_ID=Ubuntu;    PRETTY_NAME="Ubuntu ..."
#LinuxMint: DISTRIB_ID=LinuxMint; PRETTY_NAME="Linux Mint ..."
#Lubuntu:   DISTRIB_ID=Ubuntu;    PRETTY_NAME="Ubuntu ..."
#RedHat:    DISTRIB_ID=RedHat;    PRETTY_NAME="Red Hat"
#Ubuntu:    DISTRIB_ID=Ubuntu;    PRETTY_NAME="Ubuntu ..."
#Xubuntu:   DISTRIB_ID=Ubuntu;    PRETTY_NAME="Ubuntu ..."
#Zorin:     DISTRIB_ID=Zorin;     PRETTY_NAME="Zorin ..."

echo " "
echo "***************************************************************************"
printf "Install $gcversion to $cobfolder --> $libfolder\n"
echo " "
echo "with $ISAM $MATHLIB WIDE=$WIDE" cJSON=$cJSON XML2=$XML2
echo " "
echo "tarfolder=$tarfolder"
echo "cobfolder=$cobfolder"
echo "libfolder=$libfolder" 
echo "sourcefolder=$sourcefolder"
echo "distribution=$DISTRIB_ID"
echo "***************************************************************************"
echo "Are these parameters correct? If not, cancel, edit parameters, and re-run."

press_any_key

#make the tar and cob directories if not already there

if [ ! -d "$tarfolder" ]; then            
    mkdir "$tarfolder"
    if [ $? -gt 0 ]; then mkdir_error "$tarfolder"; fi
fi  

if [ ! -d "$cobfolder" ]; then
    mkdir "$cobfolder"
    if [ $? -gt 0 ]; then mkdir_error "$cobfolder"; fi
fi
 
# we store all the builds here, not just the final GnuCobol build!
# so DON'T delete it and mkdir anew!

if [ ! -d "$libfolder" ]; then
    mkdir "$libfolder"
    if [ $? -gt 0 ]; then mkdir_error "$libfolder"; fi
fi

if [ "$(ls -A $libfolder)" ]; then
   echo "If you are reinstalling to the same library folder, and changing from"
   echo "GMP to MPIR (or the inverse), or from GC3x to GC4x (or the inverse),"
   echo "you MUST remove the previous installation!"
   echo "If you are simply rebuilding the same installation, it isn't necessary."
   echo " "
   yes_or_no "Remove previous $libfolder installation"
else
   reply="n"
fi      

if [ "$reply" == "y" ]; then
   cd $cobfolder/$gmpversion
   sudo make uninstall
   sudo make distclean

   cd $cobfolder/$mpirversion
   sudo make uninstall
   sudo make distclean

   cd $cobfolder/$gcversion
   sudo make uninstall
   sudo make distclean
   
   echo " "
fi   
             
#####################################################################
# Install dependency packages required for GnuCOBOL 3.x
#####################################################################
printf "\nIt's a good idea to install the dependency packages every time that"
printf "\nyou build a new installation, because different packages are installed"
printf "\ndepending on the parameters you specify --- and it doesn't take long\n\n"

yes_or_no "Install dependency packages required for GnuCOBOL 3.x"

if [ "$reply" == "y" ]; then
      if [[ "$DISTRIB_ID" == "CentOS" || "$DISTRIB_ID" == "Fedora" ]]; then
         apt_yum="yum"; apt_get_yum="yum"
      else
         apt_yum="apt"; apt_get_yum="apt-get"
      fi   
              
#  packages that are required (according to Simon)
   sudo $apt_yum install gcc libc-dev libdb-dev libgmp-dev libncurses-dev
   sudo $apt_yum install libtool-bin libxml2-dev make perl wget
 
#  If you want to tinker with GnuCOBOL you may add (according to Simon) 
   sudo $apt_yum install help2man icu-devtools
   sudo $apt_yum install libasan
   if [ $? -gt 0 ]; then sudo $apt_yum install libasan3; fi
   if [ $? -gt 0 ]; then sudo $apt_yum install libasan5; fi
   sudo $apt_yum install zlib1g-dev
   sudo $apt_yum install git subversion automake
   
#  Special cases
   if [ "$MATHLIB" == "MPIR" ]; then sudo $apt_yum install yasm; fi
   
   if [ "$ISAM" == "VBISAM" ]; then sudo $apt_yum install unzip; fi
   
   sudo $apt_get_yum install -f
   sudo $apt_get_yum autoremove #because Package Manager says to run it!
                               #althought it SHOULD be unnecessary with Mint 19
   press_any_key
fi 

#####################################################################
# Install GMP (GNU Multiple-Precision Arithmetic Library) 
#####################################################################
if [ "$MATHLIB" == "GMP" ]; then
   yes_or_no "Install GMP (GNU Multiple-Precision Arithmetic Library)";
else
   reply="n";
fi

if [ "$reply" == "y" ]; then 
   grab_a_beer
   cd $tarfolder
   wget -O $gmpversion.tar.xz https://gmplib.org/download/gmp/$gmpversion.tar.xz
   if [ $? -gt 0 ]; then
      rm $gmpversion.tar.xz #because wget created an empty file 
      printf "\nDownload of $gmpversion.tar.xz failed - correct problem and restart\n"
      pause; exit 8
   fi  
     
   cd "$cobfolder"
   tar xf "$tarfolder/$gmpversion.tar.xz"
   if [ $? -gt 0 ]; then
      printf "\nUnpacking of $gmpversion.tar.xz failed - correct problem and restart\n"
      pause; exit 8; fi
   
   cd "$gmpversion"
   ./configure --prefix=$libfolder --disable-static --enable-shared
   if [ $? -gt 0 ]; then check_for_errors; fi
   make
   if [ $? -gt 0 ]; then check_for_errors; fi
   make check
   if [ $? -gt 0 ]; then check_for_errors; fi
   sudo make install
   if [ $? -gt 0 ]; then check_for_errors; fi
   libtool --finish $libfolder/lib
   if [ $? -gt 0 ]; then check_for_errors; fi
   press_any_key
fi

##################################################################### 
# Install MPIR (Multiple Precision Integers and Rationals)
#####################################################################
if [ "$MATHLIB" == "MPIR" ]; then
   yes_or_no "Install MPIR (Multiple Precision Integers and Rationals)"
else
   reply="n";
fi

if [ "$reply" == "y" ]; then 
   grab_a_beer
   cd "$tarfolder"
   wget -O "$mpirversion.tar.bz2" http://mpir.org/$mpirversion.tar.bz2
   if [ $? -gt 0 ]; then
      rm $mpirversion.tar.xz #because wget created an empty file 
      printf "\nDownload of $mpirversion.tar.bz2 failed - correct problem and restart\n"
      pause; exit 8
   fi   
       
   cd "$cobfolder"
   tar xvjf "$tarfolder/$mpirversion.tar.bz2"
   if [ $? -gt 0 ]; then
      printf "\nUnpacking of $mpirversion.tar.xz failed - correct problem and restart\n"
      pause; exit 8; fi

   cd "$mpirversion"
   ./configure --prefix=$libfolder --disable-static --enable-shared
   # 07/03/2020 - removed "--enable-gmpcompat" here, and added "--with-math=mpir" below
   
   if [ $? -gt 0 ]; then check_for_errors; fi
   make
   if [ $? -gt 0 ]; then check_for_errors; fi
   make check
   if [ $? -gt 0 ]; then check_for_errors; fi
   sudo make install
   if [ $? -gt 0 ]; then check_for_errors; fi
   press_any_key
fi

#####################################################################
# BDB is already installed - Install VBISAM database (VBI) 
#####################################################################
if [ "$ISAM" == "VBISAM" ]; then
   yes_or_no "Install VBISAM database (VBI)"
else
   reply="n";
fi

if [ "$reply" == "y" ]; then 
   printf "\nDownloading $VBIversion.zip\n\n"
   wget -O $tarfolder/$VBIversion.zip https://www.arnoldtrembley.com/$VBIversion.zip
   if [ $? -gt 0 ]; then 
       printf "\nDownload of $tarfolder/$VBIversion.zip failed - correct problem and restart\n"
       pause; exit 8
   fi

   printf "\nUnpacking tarball - this should take a few minutes\n\n"
   cd "$cobfolder"       
   unzip -q -o $tarfolder/$VBIversion.zip
   if [ $? -gt 0 ]; then
      printf "\nUnzip of $tarfolder/$VBIversion.zip failed - correct problem and restart\n"
      pause; exit 8; fi
  
   rm -i $tarfolder/$VBIversion.zip        

   printf "\nInstalling VBISAM\n"

   cd $cobfolder/$VBIversion
   chmod 777 ./configure
   ./configure --prefix=$libfolder
   if [ $? -gt 0 ]; then check_for_errors; fi
   make                          
   if [ $? -gt 0 ]; then check_for_errors; fi
   make check                    
   if [ $? -gt 0 ]; then check_for_errors; fi
   sudo make install             
   if [ $? -gt 0 ]; then check_for_errors; fi
   press_any_key
fi

#####################################################################
# Install the $gcversion compiler
#####################################################################
yes_or_no "Install the $gcversion compiler";

if [ "$reply" == "y" ]; then
   grab_a_beer

   if [ "$GCV" == "GC31" ];   then url="https://alpha.gnu.org/gnu/gnucobol/gnucobol-3.1-rc1.tar.gz"; fi
   if [ "$GCV" == "GC3DEV" ]; then url="https://ci.appveyor.com/api/projects/GitMensch/gnucobol-3-x/artifacts/gnucobol-3.1-dev.tar.gz?job=Image:%20Ubuntu1804"; fi
   if [ "$GCV" == "GC4DEV" ]; then url="https://ci.appveyor.com/api/projects/GitMensch/gnucobol-trunk/artifacts/gnucobol-4.0-early-dev.tar.gz?job=Image:%20Ubuntu1804"; fi
   
   cd "$tarfolder"   
   echo "Downloading $gcversion"
   wget -O $gcversion.tar.gz $url
   if [ $? -gt 0 ]; then 
      rm $gcversion.tar.xz #because wget creates an empty file 
      printf "\nDownload of $gcversion.tar.xz failed - correct problem and restart\n"
      pause; exit 8
   fi    

   cd "$cobfolder"
   tar xzf "$tarfolder/$gcversion.tar.gz"
   if [ $? -gt 0 ]; then
      printf "\nUnpacking of $gmpversion.tar.xz failed - correct problem and restart\n";
      pause; exit 8; fi 
   rm -i $tarfolder/$gcversion.tar.gz

   #if newcob.val is corrupt, remove it and let "make test" download and unpack it
   if [ -f $cobfolder/$gcversion/tests/cobol85/newcob.val ]; then 
      grep "IDENTIFICATION DIVISION." $cobfolder/$gcversion/tests/cobol85/newcob.val > /dev/null
      if [ $? -gt 0 ]; then rm        $cobfolder/$gcversion/tests/cobol85/newcob.val; fi
   fi   
      
   if [ "$cJSON" == "yes" ]; then
      echo "Downloading cJSON.c and cJSON.h"
      wget -O $cobfolder/$gcversion/libcob/cJSON.c https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.c 
      if [ $? -gt 0 ]; then
         rm $cobfolder/$gcversion/libcob/cJSON.c #because wget creates an empty file 
         printf "\nDownload of $cJson.c failed - correct problem and restart\n"
         pause; exit 8
      fi
      wget -O $cobfolder/$gcversion/libcob/cJSON.h https://raw.githubusercontent.com/DaveGamble/cJSON/master/cJSON.h 
      if [ $? -gt 0 ]; then
         rm $cobfolder/$gcversion/libcob/cJSON.h #because wget creates an empty file 
         printf "\nDownload of $cJson.h failed - correct problem and restart\n"
         pause; exit 8
      fi
   fi        

   cd "$gcversion";
   export LD_LIBRARY_PATH=$libfolder/lib:$LD_LIBRARY_PATH  #this was swapped, and not in ming!

                                    options="--without-db";
   if [ "$ISAM" == "BDB" ];    then options="--with-db"; fi
   if [ "$ISAM" == "VBISAM" ]; then options="--with-vbisam"; fi
   
   if [ "$cJSON" == "no" ];  then options="$options --without-cjson"; fi
   if [ "$cJSON" == "yes" ]; then options="$options --with-cjson"; fi
   
   if [ "$XML2" == "no" ];  then options="$options --without-xml2"; fi
   if [ "$XML2" == "yes" ]; then options="$options --with-xml2"; fi
   
   # if [ "$MATHLIB" == "MPIR" ]; then options="$options --with-math=mpir"; fi
   # 07/03/2020 - removed "--enable-gmpcompat" above, and added "--with-math=mpir" here
   # but removed here, because it causes gnucobol make errors (as per KenUnix)

   printf "\nconfiguring\n"
   ./configure --prefix=$libfolder $options "CPPFLAGS=-I$libfolder/include" "LDFLAGS=-L$libfolder/lib"
   if [ $? -gt 0 ]; then check_for_errors; fi 
   make
   if [ $? -gt 0 ]; then check_for_errors; fi
   make check
   if [ $? -gt 0 ]; then check_for_errors; fi
   make test
   if [ $? -gt 0 ]; then check_for_errors; fi
   sudo make install
   if [ $? -gt 0 ]; then check_for_errors; fi
   sudo ldconfig
   if [ $? -gt 0 ]; then check_for_errors; fi
fi

echo " "
yes_or_no "Create a 'set_env.sh' file on your desktop";

if [ "$reply" == "y" ]; then Create_set_env_file; fi

#####################################################################
# Run a test program (optional)
#####################################################################
cd "$cobfolder"

export COB_CONFIG_DIR="$libfolder/share/gnucobol/config"
export LD_LIBRARY_PATH="$libfolder/lib:$LD_LIBRARY_PATH"
export LD_RUN_PATH="$libfolder/lib:$LD_RUN_PATH"        
export LIBDIR="$libfolder/lib:$LIBDIR"         
export PATH="$libfolder/bin:$PATH"
if [ -d "/media/MYDATA/GNU-copylib" ]; then 
   export COBCPY="/media/MYDATA/GNU-copylib"; fi                   

echo "PROGRAM-ID.HELLO.PROCEDURE DIVISION." > temp.cob
echo "DISPLAY 'Hello from GnuCOBOL'."      >> temp.cob
echo "DISPLAY ' '. DISPLAY 'The compiler works. Congratulations'." >> temp.cob
cobc -xj -free temp.cob
rm temp.cob

if [ ! "$sourcefolder" ]; then
   printf "\nsourcefolder not specified on command line"
   printf "\nyou will need to enter the full path of your test files\n"
else
   if [ -d "$sourcefolder" ]; then
      cd "$sourcefolder"
   else   
      printf "\n$sourcefolder is not a valid folder"
      printf "\nyou will need to enter the full path of your test files\n"
   fi
fi   

testfile="?"

until [ ! "$testfile" ]; do
   echo " "
   echo "Please enter the pathname of a Cobol source program to test"
   read -p "or just press Enter to skip test: " testfile

   if [ "$testfile" ]; then 
      echo " "
      if [ -f "$testfile.cob" ]; then
         testfile="$testfile.cob"
      else   
         if [ -f "$testfile.cbl" ]; then
            testfile="$testfile.cbl";
         #else
            # testfile is unaltered
         fi
      fi
      
      rp=$(realpath "$testfile")      
      grep -i "PROGRAM-ID." $rp > /dev/null
      case $? in
         0) printf "compiling and executing $rp\n\n";
            cobc -x "$rp" -o $cobfolder/testfile -lncurses -D USE-NCURSES
            if [ $? -eq 0 ]; then 
               "$cobfolder/testfile"     
               rm "$cobfolder/testfile" 
            fi
            ;;
         1) printf "\n'$rp' does not appear to be a valid Cobol program"
            ;;
         *) # grep will tell you there is no such file or directory
            # or other error message (if applicable)
            ;;
      esac
   fi
done    

#####################################################################
# All good things come to an END
#####################################################################

printf "\nThe end - La fin - Das Ende - Το τέλος - הסוף -  Fine - O fim - Конец\n\n"; 

######################### end of build_gc3 script ####################
