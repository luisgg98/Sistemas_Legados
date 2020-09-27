@echo off
@rem **************************************************************************
@rem * build_gc3_mingw.bat - Build GnuCobol 3x and 4x for MinGW in Windoze
@rem * ------------------------------------------------------------------------
@rem * This script should work for current and future versions of GnuCobol, 
@rem * with only minor modification to the parameters and script.
@rem *
@rem * Syntax: build_gc3_mingw.bat [-ccobfolder]
@rem *                             [-tarfolder]
@rem *                             [-mmingwfolder]
@rem *                             [-ssourcefolder]
@rem *                             [BDB | VBISAM | NODB] [GMP | MPIR]
@rem *                             [PDC3.4 | PDC3.9 | PDC4.1 | NC6.0 | NC6.1]
@rem *                             [WINGUI] [WIDE] [CHTYPE_32 | UTF8]
@rem *                             [cJSON] [XML2]
@rem *                             [GC31 | GC3DEV | GC4DEV]
@rem *
@rem * Defaults:
@rem *   cobfolder=c:\GNUCOBOL  #folder for the GnuCOBOL installation
@rem *   tarfolder=%USERPROFILE%\Downloads #folder where tarballs will be downloaded
@rem *   mingwfolder=c:\MinGW  #folder where MinGW is installed
@rem *   srcfolder=;           #folder where source programs for testing reside
@rem *       
@rem *      BDB GMP PDC3.9 GC31
@rem *
@rem * All command-line parameters are optional. Folder names are passed to
@rem * the bash script, and therefore are case sensitive. The parameters
@rem * may be entered in any order, except that WINGUI must follow PDC4.1,
@rem * and WIDE, CHTYPE_32 and UTF8 must follow WINGUI.     
@rem *
@rem * Command-line parameters override the script variables defined below. In
@rem * general, command-line parameters are used to override variables that are
@rem * likely to change for each user, and for users who do multiple builds. 
@rem * Variables that are more static (such as program versions and download
@rem * urls) must be changed in the script.
@rem *
@rem * The batch file must be "Run as Administrator"!
@rem *
@rem * The script runs in a Windoze command prompt. It opens additional windows
@rem * to perform certain functions, including an MinGW/MSYS shell. The script
@rem * creates a bash file to pass on environmental variables, and to invoke 
@rem * build_gc3_mingw.sh for most other operations. The user must enter the
@rem * following command in the MSYS shell to execute the bash script when 
@rem * prompted: "build_gc3". The "build_gc3_mingw.sh" script must be located
@rem * in the same folder as this script (build_gc3_mingw.bat). 
@rem *
@rem * In general, you can cancel the script at any stage and restart it without
@rem * losing anything. Just reply "no" to running the steps that have already
@rem * completely successfully, and "yes" to the ones you need to rerun.
@rem *
@rem * However, when changing between GMP and MPIR, or between BDB and VBISAM,
@rem * or between PDCurses and ncurses, or in case of other difficulties, it
@rem * may be necessary to restart from a "checkpoint". Even when unnecessary,
@rem * in the aforementioned situations, restarting from a "checkpoint" will 
@rem * result in a smaller binary installation folder. To this end, the script
@rem * creates three separate checkpoint folders:
@rem *
@rem * 1) %mingwfolder%-Bare = %mingwfolder% just after MinGW/MSYS is installed
@rem *
@rem * 2) %mingwfolder%-GMP  = %mingwfolder% just after GMP is installed
@rem * or %mingwfolder%-MPIR = %mingwfolder% just after MPIR is installed
@rem *
@rem * 3) %mingwfolder%-GMP-BDB     = %mingwfolder% just after BDB is installed
@rem * or %mingwfolder%-GMP-VBISAM  = %mingwfolder% just after VBISAM is installed
@rem * or %mingwfolder%-MPIR-BDB    = %mingwfolder% just after BDB is installed
@rem * or %mingwfolder%-MPIR-VBISAM = %mingwfolder% just after VBISAM is installed
@rem *
@rem * If you do not delete these folders, you can restart at any of the check
@rem * points, or at the very beginning of the build, in case of need.        
@rem * -----------------------------------------------------------------------
@rem * Author: Carl D. Goldin <cdgoldin@gmail.com> - copyright 2019-2020.
@rem * Source: Based on Arnold Trembly's GnuCobol Build guide
@rem *         (https://www.arnoldtrembley.com/GnuCOBOL.htm)
@rem *         and very helpful information from Anon, Simon Sobisch, David
@rem *         Wall, Ken Martin, and others on the GnuCobol forum:
@rem *         https://sourceforge.net/p/open-cobol/discussion/help/
@rem *
@rem * Date-written:  12/26/2019
@rem * Last-revision: 07/02/2020
@rem *
@rem * Please report any bugs and/or suggestions to cdgoldin@gmail.com.
@rem * Thank you.
@rem *
@rem **************************************************************************
@rem * If you need/want to change the script, keep in mind that:
@rem * All parameter variables are case-sensitive.
@rem * Make sure you don't set any to NUL (i.e. set variable=), and make sure 
@rem * you don't have any leading or trailing spaces (which will be treated as
@rem * part of the variable), creating very difficult-to-debug problems.
@rem **************************************************************************

@rem Parameters that can be overridden on the command line
@rem -----------------------------------------------------
set cobfolder=C:\GNUCOBOL
    rem folder for Cobol installation (this allows different versions)
    
set tarfolder=%USERPROFILE%\Downloads
    rem folder for downloading tarball and .exe files
    
set MinGWfolder=c:\MinGW
    rem folder for Building MinG
    
set sourcefolder=;
    rem folder where your source programs reside (for testing after the build)    

set ISAM=BDB
    rem BDB, VBISAM or NODB; you MUST specify one or the other

Set MATHLIB=GMP
    rem MPIR or GMP; you MUST specify one or the other

set curses=PDCurses-3.9
    rem curses version to install = PDCurses-3.4, PDCurses-3.9, PDCurses-4.1.0,
    rem                             ncurses-6.0-2, ncurses-6.1
        
set wingui=no
    rem no=install wincon, yes=install wingui (only valid with PDCurses-4.1.0)

set wide=no
    rem no=don't use PDC_WIDE, yes=use PDC_WIDE (only valid with WINGUI)

set CHTYPE_32=no
    rem no=don't use CHTYPE_32=Y, yes=use CHTYPE_32 (only valid with WINGUI)
    rem default if WINGUI is specifed, overrides UTF8    

set UTF8=no
    rem no=don't use UTF8=Y, yes=use UTF8=Y (only valid with WINGUI)
    rem overrides CHTYPE_32 
    
set cJSON=no
    rem no=don't install cJSON, yes=install cSJON

set XML2=no
    rem no=don't install XML2, yes=install XML2
    
set GCV=GC31
    rem GNU Cobol version to install

@rem Parameters that can only be changed by editing the script
@rem ---------------------------------------------------------

set gc31_version=gnucobol-3.1-rc1
set gc3dev_version=gnucobol-3.1-dev
set gc4dev_version=gnucobol-4.0-early-dev
 
set mpirversion=mpir-3.0.0
    rem mpir version to install

set yasmversion=yasm-1.3.0-win32.exe
    rem yasm version to install (only used with MPIR)

set gmpversion=gmp-6.1.2
    rem gmp version to install

set BDBversion=db-6.2.38.NC
    rem BDB version to install

set VBIversion=vbisam-2.0.1
    rem BDB version to install

set bashscript=%~dp0build_gc3_mingw.sh
    rem drive and directory path of this script (including final \) + bash script

@rem *************************************************************************
@rem * Initialization routines
@rem *************************************************************************
set newinstall=0

openfiles > NUL 2>&1 
@IF %ERRORLEVEL% NEQ 0 (
    echo=
    ECHO This batch file MUST be run as administrator!
    pause
    exit /b 8)
  
 set /a count=1
:parm-loop
 set parm=%1
 call :GET-PARMS
 if %errorlevel% gtr 0 exit /b 8
 shift /1
 set /a count+=1
 if %count% leq 14 goto :parm-loop
 if %1. neq . (
   echo=
   echo Invalid parameter %1
   exit /b 8)
   
if %GCV%. equ GC31.   set gcversion=%gc31_version%
if %GCV%. equ GC3DEV. set gcversion=%gc3dev_version%
if %GCV%. equ GC4DEV. set gcversion=%gc4dev_version%
             
rem Trim trailing spaces from the following variables, because $cobfolder% is
rem picking up a trailing space somewhere, and the others might.             
call :Trim-trailing-spaces %cobfolder% cobfolder
call :Trim-trailing-spaces %tarfolder% tarfolder
call :Trim-trailing-spaces %MinGWfolder% MinGWfolder
call :Trim-trailing-spaces %sourcefolder% sourcefolder

set MSYSfolder=%MinGWfolder%\MSYS
set userfolder=%MSYSfolder%\1.0\home\%USERNAME%
    rem do NOT change these two variables or relocate them!
echo=
echo ***************************************************************************
echo Install %gcversion% to %cobfolder% folder
echo= 
echo with %ISAM% %MATHLIB% %curses% WINGUI=%WINGUI% WIDE=%WIDE% CHTYPE_32=%CHTYPE_32% UTF8=%UTF8% 
ECHO      cJSON=%cJSON% XML2=%XML2%
echo=
echo tarfolder=%tarfolder% MinGWfolder=%Mingwfolder% sourcefolder=%sourcefolder%  
echo *************************************************************************** 
echo Are these parameters correct? If not, cancel, edit parameters, and re-run.

call :PRESS-ANY-KEY

@rem **************************************************************************
@rem * Download and install wget for Windoze and MSYS
@rem **************************************************************************
:STEP1

if not exist c:\Windows\System32\wget.exe (
   echo Please download wget.exe to your desktop from the webpage I'm opening now.
   echo Make sure to download the correct version [32 or 64 bit] for your system! 
   call :PRESS-ANY-KEY
   start /wait "Download wget" https://eternallybored.org/misc/wget/
   echo when you've done so...
   call :PRESS-ANY-KEY
   echo Moving wget.exe to c:\Windows\System32\ and %MinGWfolder%\msys\1.0\bin\wget.exe 
   move %userprofile%\desktop\wget.exe c:\Windows\System32\wget.exe > nul
)
@rem **************************************************************************
@rem * Install MinGW and MSYS bash shell
@rem **************************************************************************
:STEP2
 choice /c YN /n /m  "Install MinGW and MSYS bash shell? Y/N? "
 if %errorlevel% equ 2 goto :STEP3

 @rem Download MinGW-get-setup.exe
 
 set url=https://sourceforge.net/projects/mingw/files/Installer/mingw-get-setup.exe/download
 set tar=mingw-get-setup.exe 

 setlocal EnableDelayedExpansion 
 if not exist %tarfolder%\%tar% (
    echo=
    echo Downloading %tar%
    wget -O %tarfolder%\%tar% %url%
    if !errorlevel! gtr 0 (
       echo=
       echo Please download %tar% to %tarfolder% manually
       echo=
       echo Download 32-bit version of MinGW - Minimalist GNU for Windows2.GNU
       echo Save the download in %tarfolder% and close the download window
       rem call :PRESS-ANY-KEY
       start /wait "Download MinGW " %url% 
       echo when you've done so.. 
       rem call :PRESS-ANY-KEY
       exit /b 8)
 )
 setlocal DisableDelayedExpansion
 endlocal
 
 @rem Install MinGW
 
 echo *************************************************************************
 if %MinGWfolder%. neq C:\MinGW. (
 echo When you run mingw-get-setup.exe it will default to installing in C:\MinGW
 echo and you should override this with %MinGWfolder%!) 
 echo=
 echo Use the GUI interface to select the 'mingw32-base' and 'msys-base' packages
 echo from the 'Basic Setup' menu (right-click ^> click 'Mark for installation')
 echo+
 echo Then click "All Packages", and select each of the following (note that many
 echo of them will already be selected.)
 echo=
 rem Unfortunately, "mingw-get install" will NOT install any of the following
 rem so we have to do it manually. :-(
 rem 
 echo mingw32-bzip2.bin   
 echo mingw32-dos2unix.bin
 echo mingw32-gettext.bin 
 echo mingw32-gettext.dev 
 echo mingw32-gettext.dll 
 echo mingw32-libtool.bin 
 echo mingw32-make.bin    
 echo msys-bash.bin       
 echo msys-m4.bin         
 echo msys-make.bin   
 echo msys-perl.bin       
 echo msys-wget.bin       
 echo=
 echo Then click the 'Installation" tab ^> 'Apply changes'.
 echo= 
 echo Then close the the installer when the installation is completed.
 echo=
 echo The installation may take 5 to 15 minutes. Grab a beer.
 echo *************************************************************************
 call :Press-any-key
  
 rem start with fresh MinGWfolder and delete any checkpoint folders
  
 for /d %%f in (%minGWfolder%*) do rd /s /q %%f 
 
 md %minGWfolder%
 
 %tarfolder%\%tar%   %=run the mingw setup and installer=%
 
 rem install the more recent version of wget so we can access SSL sites
  
 copy c:\Windows\System32\wget.exe %MinGWfolder%\msys\1.0\bin\wget.exe >nul
 
 echo=
 echo If you have more than one MinGW installation, right-click on each of the 
 echo 'MinGW Installer' icons that were created, and rename them to something
 echo meaningful that will allow you to distinguish between the different icons;
 echo and insure that their 'Properties ^> Shortcut ^> Target' points to the 
 echo correct installation! 
 echo **********************************************************************
 call :press-any-key

 if not exist %MinGWfolder%\bin\mingwm10.dll (
    echo %MinGWfolder%\bin\mingwm10.dll does not exist - cancelling.
    pause & exit /b 8)

 if not exist %MSYSfolder%\1.0\etc\fstab (
    echo %MSYSfolder%\1.0\etc\fstab does not exist - cancelling.
    pause & exit /b 8)

 echo Insure that fstab contains "%MinGWfolder% /mingw"
 Notepad %MSYSfolder%\1.0\etc\fstab 
 call :press-any-key
 
 echo If installation was successful, you can delete the setup file
 echo but you may want to keep it for future modifications.
 echo=
 
 del %tarfolder%\mingw-get-setup.exe /p 
 
 echo Backing up %MinGWfolder% to %MinGWfolder%-Bare for restarting and alternate builds  
 xcopy %MinGWfolder%\*.* %MinGWfolder%-Bare\ /E /I /Q /H /R /Y /O 
 set newinstall=1
 
@rem **************************************************************************
@rem * Configure the Bash Shell
@rem **************************************************************************
:STEP3
 echo=
 choice /c YN /n /m  "Configure the bash shell? Y/N? "
 if %errorlevel% equ 2 goto :STEP4

 set msys=
 setlocal enableDelayedExpansion
 powershell /? > nul 2>nul
 if %errorlevel% equ 0 (
   set target=%msysfolder%\1.0\msys.bat\
   set arguments=-norxvt
   set startin=%msysfolder%\1.0
   
   :msys-loop
    set shortcut=%userprofile%\Desktop\MSYS!msys!.lnk
    if exist !shortcut! (set /a msys=msys+1 &goto :msys-loop)
    powershell "$s=(New-Object -COM WScript.Shell).CreateShortcut('!shortcut!');"^
    "$s.TargetPath='!target!';$s.Arguments='!arguments!';"^
    "$s.WorkingDirectory='!startin!';$s.Save()"
    goto :step3a %=because else doesn't work here for some reason!=%
) 
    echo .  
    echo Create an MSYS shortcut on your windows desktop as follows:
    echo   -Browse to %MSYSfolder%\1.0\msys.bat with Windows Explorer
            explorer %MSYSfolder%\1.0\
    echo   -Right click %MSYSfolder%\1.0\msys.bat
    echo   -Click "Send to > Desktop (create shortcut)"

:step3a
setlocal disableDelayedExpansion
endlocal
 echo=
 echo Right-click on the MSYS%msys% shortcut and click "Properties" to change:
 echo   General: Change shortcut name to something beginning with "MSYS". If you
 echo            have more than one MinGW/MSYS installation, create a shortcut
 echo            for each of them with a unique name beginning with MSYS. (This
 echo            is essential!). (You can set up the first MSYS shortcut, and then 
 echo            just copy it to any additional MSYS shortcuts, changing only the 
 echo            shortcut name and target.)
 echo  Shortcut: Target:      "%MSYSfolder%\1.0\msys.bat -norxvt"
 echo                          -norxvt is essential!
 echo            Start in:    "%MSYSfolder%\1.0
 echo            Change icon: "%MSYSfolder%\1.0\msys.ico" (optional)
 echo            Advanced:    "Run as Administrator" and click "Apply"
 echo            This is necessary for Windoze Vista and Windows 7. It is also
 echo            necessary for W10 in order for certain commands in the script to work.
 echo   Options: Enable Quick Edit Mode and Insert Mode
 echo      Font: Change the font to something that enables that window to fit
 echo            on your screen, e.g. "Lucida Console" with a font size of "10".
 echo    Layout: Screen Buffer Size: Width: 190  Height: (at least) 2000 lines
 echo            Window Size:        Width: 190  Height: at least 60 lines
 echo            This better allows us to view the voluminous output of "make",
 echo            "make test", and "make install".
 echo  Security: Edit ^> Administrators ^> Full Control
 echo=
 call :press-any-key
 
@rem **************************************************************************
@rem * Download and install all (?) other required tarball and .exe files
@rem * ------------------------------------------------------------------------
@rem * Automatically download every required tarball (based on the specified 
@rem * parameters) that is not in the %tarfolder% folder. To force a download, 
@rem * simply delete the tarball from the folder before running the script. 
@rem *
@rem * If we do "wget ...", there is no progress indicator!
@rem * If we do "start /wait wget...", there IS a progress indicator, but wget
@rem * closes the separate window before we can see any error message or stats.
@rem * wget --show-progress doesn't work
@rem * wget --progress=bar doesn't work
@rem * wget --progress=bar:force:noscroll doesn't work
@rem * wget --progress=dot works, so we'll use it, albeit bar would be better
@rem * wget --progress=dot:force:noscroll doesn't work
@rem * curl is installed in W10 and MSYS, but not W7, so it won't work here
@rem **************************************************************************
:STEP4

setlocal EnableDelayedExpansion
 
@rem Download GMP if needed
 
if %MATHLIB%. equ GMP. if not exist %tarfolder%\%gmpversion%.tar.xz (
   echo=
   echo Downloading %gmpversion%
   wget -O %tarfolder%\%gmpversion%.tar.xz https://gmplib.org/download/gmp/%gmpversion%.tar.xz
   if !errorlevel! gtr 0 (
       echo=
       echo Please download %gmpversion%.tar.xz to %tarfolder% manually
       exit /b 8)
)

@rem Download MPIR if needed
 
if %MATHLIB%. equ MPIR. if not exist %tarfolder%\%mpirversion%.tar.bz2 (
   echo=
   echo Downloading %mpirversion%
   wget -O %tarfolder%\%mpirversion%.tar.bz2 http://mpir.org/%mpirversion%.tar.bz2
   if !errorlevel! gtr 0 (
       echo=
       echo Please download %mpirversion%.tar.bz2 to %tarfolder% manually
       exit /b 8)   
   echo Downloading %yasmversion%
   wget -O %tarfolder%\%yasmversion% http://www.tortall.net/projects/yasm/releases/%yasmversion%
   if !errorlevel! gtr 0 (
       echo=
       echo Please download %yasmversion% to %tarfolder% manually
       exit /b 8)
)

@rem Download PDCurses if needed

if %curses:~0,3% equ PDC if not exist %tarfolder%\%curses%.tar.gz (
   echo=

   if %curses% equ PDCurses-3.4 ( 
      set url="https://sourceforge.net/projects/pdcurses/files/pdcurses/3.4/"
   ) else if %curses% equ PDCurses-3.9 (
      set url="https://fossies.org/linux/misc/PDCurses-3.9.tar.gz"
   ) else if %curses% equ PDCurses-4.1.0 ( 
      set url="https://github.com/Bill-Gray/PDCurses/archive/v4.1.0.tar.gz"
   ) else (echo %curses% is not implemented yet
     exit /b 8)
     
   echo Downloading %curses%
 
   wget -O %tarfolder%\%curses%.tar.gz ^"!url!^"   
   if !errorlevel! gtr 0 (
      echo=
      del %tarfolder%\%curses%.tar.gz
      echo Please download %curses%.tar.gz to %tarfolder% manually
      exit /b 8)
)

@rem Download ncurses 6.0-2 if needed

if %curses% equ ncurses-6.0-2 if not exist %tarfolder%\%curses%-mingw32-bin.tar.lzma (
   echo=
   call :get_ncurses ncurses-6.0-2-mingw32-bin.tar.lzma
   if !errorlevel! gtr 0 exit /b !errorlevel!
   call :get_ncurses terminfo-6.0-2-mingw32-data.tar.lzma
   if !errorlevel! gtr 0 exit /b !errorlevel!   
   call :get_ncurses libncurses-6.0-2-mingw32-dev.tar.lzma
   if !errorlevel! gtr 0 exit /b !errorlevel!
   call :get_ncurses libncurses-6.0-2-mingw32-dll-6.tar.lzma
   if !errorlevel! gtr 0 exit /b !errorlevel!
)

goto :ncurses-6.1

:get_ncurses
 echo Downloading %1
 echo=
 set url="https://sourceforge.net/projects/mingw/files/MinGW/Contributed/ncurses/ncurses-6.0-2/%1/download"
 wget -O %tarfolder%\%1 ^"%url%^"   
 if %errorlevel% gtr 0 (
    echo=
    del %tarfolder%\%1
    echo Please download %1 to %tarfolder% manually
    exit /b 8)
 exit /b
       
:ncurses-6.1     
@rem Download ncurses 6.1 if needed

if %curses% equ ncurses-6.1 if not exist %tarfolder%\ncurses-snapshots-master.zip (
   echo=
   set url="https://github.com/ThomasDickey/ncurses-snapshots/archive/master.zip"
   echo Downloading %curses%
 
   wget -O %tarfolder%\ncurses-snapshots-master.zip ^"!url!^"   
   if !errorlevel! gtr 0 (
      echo=
      %tarfolder%\ncurses-snapshots-master.zip
      echo Please download %tarfolder%\ncurses-snapshots-master.zip to %tarfolder% manually
      exit /b 8)
)
                     
@rem Download BDB if needed

if %ISAM%. equ BDB. if not exist %tarfolder%\%BDBversion%.tar.gz (
   start /wait "Oracle Download " https://www.oracle.com/technetwork/database/database-technologies/berkeleydb/downloads/index-082944.html
   echo=
   echo [Create an Oracle user account if you don't already have one], log on, and download
   echo %BDBversion%.tar.gz to %tarfolder%\
   @rem We can't use wget, because it results in 'username/password authentication
   @rem failed with HTTP'. 
   call :PRESS-ANY-KEY
)

@rem Download VBISAM if needed

if %ISAM%. equ VBISAM. if not exist %tarfolder%\%VBIversion%.zip (
   echo=
   echo Downloading %VBIversion%.zip
   wget -O %tarfolder%\%VBIversion%.zip https://www.arnoldtrembley.com/%VBIversion%.zip
   if !errorlevel! gtr 0 (
       echo=
       echo Please download %vbiversion%.zip to %tarfolder% manually
       exit /b 8)
)  
@rem Download the GnuCOBOL compiler if needed

if exist %tarfolder%\%gcversion%.tar.gz (
   echo=
   choice /c YN /n /m  "Download the latest nightly snapshot of %gcversion%? Y/N "
   if !errorlevel! equ 1 set reply=yes
   if !errorlevel! equ 2 set reply=no
) else                   set reply=yes

if %reply%. equ yes. (
   echo=
   echo Downloading %gcversion%
   
   if %GCV%. equ GC31.   set url="https://alpha.gnu.org/gnu/gnucobol/gnucobol-3.1-rc1.tar.gz"
   if %GCV%. equ GC3DEV. set url="https://ci.appveyor.com/api/projects/GitMensch/gnucobol-3-x/artifacts/gnucobol-3.1-dev.tar.gz?job=Image:%%20Ubuntu1804"
   if %GCV%. equ GC4DEV. set url="https://ci.appveyor.com/api/projects/GitMensch/gnucobol-trunk/artifacts/gnucobol-4.0-early-dev.tar.gz?job=Image:%%20Ubuntu1804"
      
   wget -O %tarfolder%/%gcversion%.tar.gz ^"!url!^"
   
   if !errorlevel! gtr 0 (
       echo=
       echo Please download %gcversion%.tar.gz to %tarfolder% manually
       exit /b 8)
   
   rem set the file date/time to the creation date/time (instead of 12/23/2019)
   set file="%tarfolder%/%gcversion%.tar.gz"
   powershell ^(Get-Item !file!^).LastWriteTime=^(Get-Item !file!^).creationtime
   set file=
)
setlocal DisableDelayedExpansion
endlocal
     
@rem newcob.val and cJSON are downloaded in the bash script, because they need
@rem to be downloaded (or moved) into folders that aren't built yet.

@rem **************************************************************************
@rem * Process checkpoint restarts
@rem **************************************************************************
:STEP5
if %newinstall% equ 1 goto :STEP6
echo=
echo When changing between GMP and MPIR, or between BDB and VBISAM, or between
echo PDCurses and ncurses, or in case of other difficulties, it may be necessary
echo to restart from a "checkpoint". Even when unnecessary, in the aforementioned
echo situations, restarting from a "checkpoint" will result in a smaller binary
echo installation folder.
 
set choice1=%MinGWfolder%-Bare
set choice2=%MinGWfolder%-GMP
set choice3=%MinGWfolder%-MPIR
set choice4=%MinGWfolder%-GMP-BDB
set choice5=%MinGWfolder%-GMP-VBISAM
set choice6=%MinGWfolder%-MPIR-BDB
set choice7=%MinGWfolder%-MPIR-VBISAM
echo=
echo Available checkpoints are:
if exist %choice1% echo 1. After initial MinGW/MSYS installation - %choice1%
if exist %choice2% echo 2. After GMP installation - %choice2%
if exist %choice3% echo 3. After MPIR installation - %choice3%
if exist %choice4% echo 4. After GMP+BDB installation - %choice4%
if exist %choice5% echo 5. After GMP+VBISAM installation - %choice5%
if exist %choice6% echo 6. After MPIR+BDB installation - %choice6%
if exist %choice7% echo 7. After MPIR+VBISAM installation - %choice7%
                   echo N = Continue without restarting at checkpoint
echo=
:restore-loop
 CHOICE /C 1234567N /N /M "Restart at checkpoint number: "

 if %errorlevel% equ 1 (set checkpoint=%choice1%& goto :restore)  
 if %errorlevel% equ 2 (set checkpoint=%choice2%& goto :restore)  
 if %errorlevel% equ 3 (set checkpoint=%choice3%& goto :restore)  
 if %errorlevel% equ 4 (set checkpoint=%choice4%& goto :restore)  
 if %errorlevel% equ 5 (set checkpoint=%choice5%& goto :restore)  
 if %errorlevel% equ 6 (set checkpoint=%choice6%& goto :restore)  
 if %errorlevel% equ 7 (set checkpoint=%choice7%& goto :restore)  
 goto :STEP6  

:restore 
 echo=
 if not exist %checkpoint% goto :restore-loop
 echo Restoring %minGWfolder% from %checkpoint%
 echo on
 if exist %minGWfolder%\nul rd /s /q %minGWfolder% 
 xcopy %checkpoint%\*.* %minGWfolder% /E /I /Q /H /R /Y /O
@echo off 

@rem **************************************************************************
@rem * Invoke the bash script to do the rest of the job
@rem **************************************************************************
:STEP6
 set choice1=
 set choice2=
 set choice3=
 set choice4=
 set choice5=
 set choice6=
 set choice7=
 set checkpoint=
  
 echo=
 choice /c YN /n /m  "Invoke the bash script to build GnuCOBOL with MSYS? Y/N "
 if %errorlevel% equ 2 goto :STEP7

 setlocal EnableDelayedExpansion
 call :CONVERT_FILENAME %bashscript%  & set bashscriptx=!lfn!
 call :CONVERT_FILENAME %MinGWfolder% & set MinGWfolderx=!lfn!
 call :CONVERT_FILENAME %tarfolder%   & set tarfolderx=!lfn!
 setlocal DisableDelayedExpansion
 endlocal

 if not exist %userfolder% md  %userfolder%
 
 rem Note that folder and file names are in Windoze format in this script,
 rem but are converted to Linux format when passed to MSYS
 
 echo #!/bin/bash                          > %userfolder%\build_gc3
 echo set -x #echo on                     >> %userfolder%\build_gc3
 echo export BDBversion="%BDBversion%"    >> %userfolder%\build_gc3
 echo export CHTYPE_32="%CHTYPE_32%"      >> %userfolder%\build_gc3
 echo export cJSON="%cJSON%"              >> %userfolder%\build_gc3
 echo export curses="%curses%"            >> %userfolder%\build_gc3
 echo export gcversion="%gcversion%"      >> %userfolder%\build_gc3
 echo export gmpversion="%gmpversion%"    >> %userfolder%\build_gc3
 echo export ISAM="%ISAM%"                >> %userfolder%\build_gc3
 echo export MATHLIB="%MATHLIB%"          >> %userfolder%\build_gc3
 echo export MinGWfolder="%MinGWfolderx%" >> %userfolder%\build_gc3
 echo export mpirversion="%mpirversion%"  >> %userfolder%\build_gc3
 echo export tarfolder="%tarfolderx%"     >> %userfolder%\build_gc3
 echo export UTF8="%UTF8%"                >> %userfolder%\build_gc3
 echo export VBIversion="%VBIversion%"    >> %userfolder%\build_gc3
 echo export wide="%wide%"                >> %userfolder%\build_gc3
 echo export wingui="%wingui%"            >> %userfolder%\build_gc3
 echo export XML2="%XML2%"                >> %userfolder%\build_gc3
 echo export yasmversion="%yasmversion%"  >> %userfolder%\build_gc3
 echo %bashscriptx%                       >> %userfolder%\build_gc3
 
 rem type %userfolder%\build_gc3 %=display the build_gc3 file for debugging=%
 
 rem find the MSYS desktop shortcut for *this* MinGW installation
 set test-string=%msysfolder:~3%
 set test-string=%test-string:\=\\%
 set test-string=%test-string%\\1\.0\\msys\.bat
 set msyslink=???
 setlocal enableDelayedExpansion
 for %%f in (%USERPROFILE%\Desktop\msys*.lnk) do (
     findstr /i %test-string% %%f >nul 2>nul
     if !errorlevel! equ 0 set msyslink=%%f
 )
  
 if !msyslink!. equ . (
    echo=
    echo no MSYS shortcut found for %msysfolder%
    echo please correct problem and restart!
    exit /b 8)
     
 start "MSYS" !msyslink!
 
 setlocal disableDelayedExpansion
 endlocal
 
 echo **********************************************************************
 echo Please [double] click the MSYS icon on your desktop to start MSYS
 echo if it's not already started. Then type "build_gc3" in the MSYS window.
 echo **********************************************************************
 echo Wait for the MSYS build procedure to finish, then ...
 call :Press-any-key
    
@rem **************************************************************************
@rem * Prepare the %cobfolder% folder
@rem **************************************************************************
:STEP7
 echo=
 choice /c YN /n /m  "Prepare the %cobfolder% folder? Y/N? "
 if %errorlevel% equ 2 goto :STEP8

 if exist %cobfolder% rd /s /q %cobfolder% %=so we start with an empty folder=%
 md %cobfolder%

 xcopy %mingwfolder%\bin\*.*                   %cobfolder%\bin\ /s /e /q
                                           del %cobfolder%\bin\auto*.*
 xcopy %mingwfolder%\share\gnucobol\config\*.* %cobfolder%\config\ /s /e /q
 xcopy %mingwfolder%\share\gnucobol\copy\*.*   %cobfolder%\copy\ /s /e /q
 xcopy %mingwfolder%\lib\*.*                   %cobfolder%\lib\ /q 
 xcopy %mingwfolder%\lib\gcc\*.*               %cobfolder%\lib\gcc\ /s /e /q
 xcopy %mingwfolder%\libexec\gcc\*.*           %cobfolder%\libexec\gcc\ /s /e /q
 xcopy %mingwfolder%\include                   %cobfolder%\include\ /s /e /q
                                           del %cobfolder%\include\autosp*.*
 xcopy %MSYSfolder%\%gcversion%\extras\*.*     %cobfolder%\extras\ /s /e /q
 
 echo=
 echo strip out unneeded GnuCOBOL components
 echo=
 cd /d %cobfolder%
 copy bin\strip* . 
 copy bin\libiconv* . 
 echo stripping - this might take a while
 strip -p --strip-debug --strip-unneeded bin\*.dll bin\*.exe lib\*.a
 del strip* libiconv*
 echo=

 echo Please download and add any files you wish (from the repository or
 echo elsewhere) to the %cobfolder% folder, now or any time in the future.
 
@rem **************************************************************************
@rem * Create a "set_env.cmd" file (note that we create and use %COB_MAIN_DIR% 
@rem * instead of %cobfolder%, so that we can change the name of the folder
@rem * later without having to edit the set_env.cmd file!)
@rem * WARNING: IF YOU EDIT THIS PROCEDURE, NOTE THAT THERE CAN BE NO SPACES
@rem * BETWEEN THE VALUE IN AN "ECHO SET" COMMAND AND ">>set_env.cmd", even
@rem * if this makes things harder to read.
@rem **************************************************************************
:STEP8
echo=
echo creating set_env.cmd file

cd /d %cobfolder%

echo @echo off> set_env.cmd
echo echo=>>set_env.cmd

set param=Setting environment for %gcversion% (%date%) with MinGW binaries
set param=%param:gnucobol-=GnuCOBOL %

echo echo %param%>>set_env.cmd

set param=(GCC 6.3.0, %curses%
if %mathlib%. equ GMP.  set param=%param%, %gmpversion%
if %mathlib%. equ MPIR. set param=%param%, %mpirversion%
if %ISAM%.  equ BDB.    set param=%param%, b%bdbversion%
if %ISAM%.  equ VBISAM. set param=%param%, %vbiversion%
if %ISAM%.  equ NODB.   set param=%param%, NO ISAM
if %cJSON%. equ yes.    set param=%param%, cJSON
if %cJSON%. equ no.     set param=%param%, no cJSON
if %XML2%.  equ yes.    set param=%param%, XML2)
if %XML2%.  equ no.     set param=%param%, no XML2)

rem cosmetic changes to match Simon's version (which looks better)
set param=%param:pdcurses-=PDCurses %
set param=%param:gmp-=GMP %
set param=%param:mpir-=MPIR %
set param=%param:bdb-=BDB %
set param=%param:vbisam-=VBISAM %

echo echo %param%>>set_env.cmd
echo echo=>>set_env.cmd
echo=>>set_env.cmd
echo rem Get the main dir from the batch's position>>set_env.cmd
echo rem (only works in Windows NT environments or higher)>>set_env.cmd
echo set COB_MAIN_DIR=%%~dp0>>set_env.cmd
echo=>>set_env.cmd
echo rem settings for cobc>>set_env.cmd
echo set COB_CONFIG_DIR=%%COB_MAIN_DIR%%config>>set_env.cmd
echo set COB_COPY_DIR=%%COB_MAIN_DIR%%copy>>set_env.cmd
echo=>>set_env.cmd

set include=include
if %curses:~0,7% equ ncurses set include=include\ncurses

echo set COB_CFLAGS ^>temp 2^>nul>>set_env.cmd
echo findstr /i "-I"%%COB_MAIN_DIR%%%include%"" temp ^>nul>>set_env.cmd 
echo if %%errorlevel%% neq 0 set COB_CFLAGS=-I"%%COB_MAIN_DIR%%%include%" %%COB_CFLAGS%%>>set_env.cmd
echo=>>set_env.cmd
echo set COB_LDFLAGS ^>temp 2^>nul>>set_env.cmd
echo findstr /i "-L"%%COB_MAIN_DIR%%lib"" temp ^>nul>>set_env.cmd 
echo if %%errorlevel%% neq 0 set COB_LDFLAGS=-L"%%COB_MAIN_DIR%%lib" %%COB_LDFLAGS%%>>set_env.cmd
echo=>>set_env.cmd
echo set COB_CC=%%COB_MAIN_DIR%%bin\gcc.exe>>set_env.cmd
echo=>>set_env.cmd
echo rem settings for libcob>>set_env.cmd
echo rem the following won't work in GnuCOBOL 3.0 if there are spaces in COB_MAIN_DIR>>set_env.cmd
echo set COB_LIBRARY_PATH=%%COB_MAIN_DIR%%extras>>set_env.cmd
echo=>>set_env.cmd   
echo rem Add the bin path of GnuCOBOL (including GCC) to PATH for further references>>set_env.cmd
echo path ^> temp>>set_env.cmd
echo findstr /i "%%COB_MAIN_DIR%%bin" temp ^>nul>>set_env.cmd
echo if %%errorlevel%% neq 0 set path=%%COB_MAIN_DIR%%bin;%%path%%>>set_env.cmd
echo=>>set_env.cmd
echo del temp>>set_env.cmd
echo=>>set_env.cmd
echo rem Compiler version output>>set_env.cmd
echo cobc --i>>set_env.cmd
echo echo=>>set_env.cmd
echo path>>set_env.cmd
echo echo=>>set_env.cmd

@rem **************************************************************************
@rem * Run a test program (optional)
@rem **************************************************************************
:STEP9
 cd /d %cobfolder%
 call set_env.cmd > nul
 set COB_CONFIG_DIR=%COB_MAIN_DIR%config
   
 echo=
 echo PROGRAM-ID.HELLO.PROCEDURE DIVISION. > temp.cob
 echo DISPLAY 'Hello from GnuCOBOL'.      >> temp.cob
 echo DISPLAY ' '. DISPLAY 'The compiler works. Congratulations'. >> temp.cob
 cobc -xj -free temp.cob
 del temp.cob
  
 if %sourcefolder%. neq . cd /d %sourcefolder% 2>nul
 if %cd%. neq %sourcefolder%. (
    echo=
    if %sourcefolder%. equ . (
       echo sourcefolder not specified on command line
    ) else (echo %sourcefolder% is not a valid folder)
    echo -- you will have to enter the full path of your test files
    pause) 
  
:STEP8-LOOP
 set testfile=
 echo=
 echo Please enter the pathname of a Cobol source program to test
 set /p testfile="or just press Enter to skip test: "
  
 if %testfile%. equ . goto :END
 
 echo=

 if exist %testfile%.cob (
    set testfile=%testfile%.cob
  ) else if exist %testfile%.cbl (
            set testfile=%testfile%.cbl)
 echo=
 
 set libcurses=
 if %curses:~0,7% equ ncurses  (set libcurses=-lncurses -D USE-NCURSES)
 if %curses:~0,8% equ PDCurses (set libcurses=-lpdcurses)
 if exist d:\GNU-copylib set COBCPY=d:\GNU-copylib
 
 findstr /I "PROGRAM-ID." %testfile% >nul 2>nul

 if %errorlevel% equ 0 (
    echo compiling and executing %testfile%
    echo=
    cobc -xj %testfile% -o tempfile.exe %libcurses%
    del tempfile.exe    
  ) else if exist %testfile% (
            echo %testfile% does not appear to be a valid Cobol program
            ) else echo %testfile% does not exist
 echo= 
 goto :STEP8-LOOP %=repeat until no program name entered=%

@rem **************************************************************************
@rem * All good things come to an END
@rem **************************************************************************
:END
 del %cobfolder%\temp*.* >nul
 rem do NOT del %userfolder%\build_gc3 so we can test w/o running the entire script
 echo=
 echo The end - la fin - das Ende - to telos - hasof - fine - O fim - konets
 exit /b

@rem **************************************************************************
@rem * Internal subroutines
@rem **************************************************************************
:PRESS-ANY-KEY
 echo=
 echo Press any key to continue, Ctrl-C or Ctrl-Break to cancel
 pause > nul
 echo=
 exit /b

:GET-PARMS &rem parm is set above
 if %parm%. equ . exit /b %=no parameter=%
 
 set rawparm=%parm%
 
 call :UPSHIFT
     
 if %parm% equ BDB     (set ISAM=BDB
                        exit /b)
 if %parm% equ VBISAM  (set ISAM=VBISAM
                        exit /b)
 if %parm% equ NODB    (set ISAM=NODB
                        exit /b)

 if %parm% equ MPIR    (set MATHLIB=MPIR
                        exit /b)
 if %parm% equ GMP     (set MATHLIB=GMP
                        exit /b)

 if %parm% equ PDC3.4  (set curses=PDCurses-3.4
                        exit /b)
 if %parm% equ PDC3.9  (set curses=PDCurses-3.9
                        exit /b)
 if %parm% equ PDC4.1  (set curses=PDCurses-4.1.0
                        exit /b)
 if %parm% equ NC6.0   (set curses=ncurses-6.0-2
                        exit /b)
 if %parm% equ NC6.1   (set curses=ncurses-6.1
                        exit /b)                        
                                                       
 if %parm% equ WINGUI  if %curses% equ PDCurses-4.1.0 (
                       set WINGUI=yes
                       set CHTYPE_32=yes
                       set UTF8=no
                       exit /b)
                     
 if %parm% equ WIDE    if %WINGUI% equ yes (
                       set WIDE=yes
                       set CHTYPE_32=yes
                       set UTF8=no
                       exit /b)
                       
 if %parm% equ CHTYPE_32 if %WINGUI% equ yes (
                       set CHTYPE_32=yes
                       set UTF8=no
                       exit /b)
                       
 if %parm% equ UTF8    if %WINGUI% equ yes (
                       set CHTYPE_32=no
                       set UTF8=yes
                       exit /b)                       
                                         
 if %parm% equ CJSON  (set cJSON=yes
                       exit /b)     
                       
 if %parm% equ XML2   (set XML2=yes
                       exit /b)
                                          
 if %parm% equ GC31   (set GCV=GC31
                       exit /b)
                      
 if %parm% equ GC3DEV (set GCV=GC3DEV
                       exit /b)
                            
 if %parm% equ GC4DEV (set GCV=GC4DEV
                       exit /b)
                     
 if %parm:~0,2% equ -C (set cobfolder=%rawparm:~2% 
                        exit /b)
                        
 if %parm:~0,2% equ -T (set tarfolder=%rawparm:~2%
                        exit /b)
                        
 if %parm:~0,2% equ -M (set mingwfolder=%rawparm:~2%
                        exit /b)

 if %parm:~0,2% equ -S (set sourcefolder=%rawparm:~2%
                        exit /b)
  
 echo=
 echo Invalid parameter %rawparm%
 echo=
 exit /b 8 

:CONVERT_FILENAME &rem windoze_format_name --> linux_format_name (lfn)
 set wfn=%1%
 set lfn=%wfn:c:=/c%
 set lfn=%lfn:d:=/d%
 set lfn=%lfn:e:=/e%
 set lfn=%lfn:f:=/f%
 set lfn=%lfn:g:=/g%
 set lfn=%lfn:h:=/h%
 set lfn=%lfn:i:=/i%
 set lfn=%lfn:j:=/j%
 set lfn=%lfn:\=/%
 exit /b
 
:UPSHIFT &rem field_to_upshift (parm) is set above
SET parm=%parm:a=A%
SET parm=%parm:b=B%
SET parm=%parm:c=C%
SET parm=%parm:d=D%
SET parm=%parm:e=E%
SET parm=%parm:f=F%
SET parm=%parm:g=G%
SET parm=%parm:h=H%
SET parm=%parm:i=I%
SET parm=%parm:j=J%
SET parm=%parm:k=K%
SET parm=%parm:l=L%
SET parm=%parm:m=M%
SET parm=%parm:n=N%
SET parm=%parm:o=O%
SET parm=%parm:p=P%
SET parm=%parm:q=Q%
SET parm=%parm:r=R%
SET parm=%parm:s=S%
SET parm=%parm:t=T%
SET parm=%parm:u=U%
SET parm=%parm:v=V%
SET parm=%parm:w=W%
SET parm=%parm:x=X%
SET parm=%parm:y=Y%
SET parm=%parm:z=Y%
exit /b

:Trim-trailing-spaces &rem (%variable_to_be_trimmed% variable_to_be_trimmed) 
SET %2=%1
exit /b
