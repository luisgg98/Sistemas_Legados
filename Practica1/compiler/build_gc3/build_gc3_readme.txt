"Readme" file for build_gc3_mingw.bat, build_gc3_mingw.sh, build_gc3_linux.sh - 07/03/2020
===========================================================================================
build_gc3_mingw.bat and build_gc3_mingw.sh are (both) needed for a Windoze 7 or Windows 10 installation. The scripts have been thoroughly tested on Windows 10.

build_gc3_linux.sh is needed for a Linux installation. The script has been thoroughly tested with Debian 10.2, Kali Linux 2019.4, Linux Lite 4.6, Linux Mint 19.3 cinnamon, mate and XFCE, Linux Mint 20 mate, Ubuntu 18.04 Server, Ubuntu 19.10 Mate, Ubuntu 19.10 server, Ubuntu Focal 20.04, Xubuntu 19.10 and Zorin 15.1 Lite.

[Please note that Kali users must add
      deb https://http.kali.org/kali kali-rolling main main
      deb-src https://http.kali.org/kali kali-rolling main main
      to the end of /etc/apt/sources.list. and then execute "sudo apt update"
      For 'sudo apt' to work correctly.]

It should work with other Linux installations with no (or minor) modification. 

All three installation scripts and their applicable command-line parameters are described at the beginning of each script. If the script defaults are satisfactory, you can run them "as is". If they are not, you need to override the defaults by specifying the appropriate command line parameters (described in the beginning of each program). The two Windoze scripts MUST be located in the same folder, and must be "run as administrator".

Assuming that all the defaults are satisactory, you should be able to install GnuCOBOL-3x as follows:

For Windows: [Run as Administrator] [path\]build_gc3_mingw.bat

For Linux Mint: [/path/]build_gc3_linux.sh

The pathnames of the folder in which to install GnuCOBOL, the folder in which to download the various installation tarballs and executables, and the folder in which to unpack them, can be specified on the command line -- if the defaults are not satisfactory. Similarly you can override the MATHLIB, ISAM handler, and (for Windoze) version of PDCurses to be used, on the command line. And, if necessary, you can make changes to the script itself to customize it --- although that shouldn't be necessary.

Please report any problems and/or suggestions to cdgoldin@gmail.com. We will try to address them as soon as possible. Please make sure that you are using the latest script before reporting a problem, as it may have already been addressed.






 