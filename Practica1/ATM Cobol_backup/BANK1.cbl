       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM
           FILE STATUS IS FST.

           SELECT INTENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS INUM
           FILE STATUS IS FSI.


       DATA DIVISION.
       FILE SECTION.
       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TAJETAREG.
           02 TNUM      PIC 9(16).
           02 TPIN      PIC  9(4).

       FD INTENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "intentos.ubd".
       01 INTENTOSREG.
           02 INUM      PIC 9(16).
           02 IINTENTOS PIC 9(1).


       WORKING-STORAGE SECTION.
       77 FST                      PIC  X(2).
       77 FSI                      PIC  X(2).

       78 BLACK   VALUE 0.
       78 BLUE    VALUE 1.
       78 GREEN   VALUE 2.
       78 CYAN    VALUE 3.
       78 RED     VALUE 4.
       78 MAGENTA VALUE 5.
       78 YELLOW  VALUE 6.
       78 WHITE   VALUE 7.

       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO              PIC  9(4).
               10 MES              PIC  9(2).
               10 DIA              PIC  9(2).
           05 HORA.
               10 HORAS            PIC  9(2).
               10 MINUTOS          PIC  9(2).
               10 SEGUNDOS         PIC  9(2).
               10 MILISEGUNDOS     PIC  9(2).
           05 DIF-GMT              PIC S9(4).

       01 KEYBOARD-STATUS           PIC 9(4).
           88 ENTER-PRESSED          VALUE 0000.
           88 PGUP-PRESSED        VALUE 2001.
           88 PGDN-PRESSED        VALUE 2002.
           88 UP-ARROW-PRESSED    VALUE 2003.
           88 DOWN-ARROW-PRESSED  VALUE 2004.
           88 ESC-PRESSED         VALUE 2005.
           88 F1-PRESSED         VALUE  1001.
           88 F2-PRESSED         VALUE  1002.

       77 PRESSED-KEY  BLANK WHEN ZERO  PIC  9(4).
       77 PIN-INTRODUCIDO          PIC  9(4).
       77 CHOICE BLANK WHEN ZERO   PIC  9(1).


       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 DATA-ACCEPT.
           05 TARJETA-ACCEPT BLANK ZERO AUTO LINE 08 COL 50
               PIC 9(16) USING TNUM.
           05 PIN-ACCEPT BLANK ZERO SECURE LINE 09 COL 50
               PIC 9(4) USING PIN-INTRODUCIDO.



       PROCEDURE DIVISION.
       IMPRIMIR-CABECERA.
           *>MUESTRA LA PANTALLA INICIAL

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'

           DISPLAY BLANK-SCREEN.
           DISPLAY "Cajero Automatico UnizarBank" AT LINE 2 COL 26
               WITH FOREGROUND-COLOR IS CYAN.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.
           *> Cambiado al formato de cobol Standard
           *> Se encontraba en un estilo solo para Windows
           DISPLAY DIA AT LINE 4 COL 32.
           DISPLAY  "-" AT LINE 4 COL 34.
           DISPLAY MES AT LINE 4 COL 35.
           DISPLAY  "-" AT LINE 4 COL 37.
           DISPLAY  ANO AT LINE 4 COL 38.
           DISPLAY HORAS AT LINE 4 COL 44.
           DISPLAY ":" AT LINE 4 COL 46.
           DISPLAY MINUTOS AT LINE 4 COL 47.


       P1.
           DISPLAY "Bienvenido a UnizarBank" AT LINE 8 COL 28.
           DISPLAY "Por favor, introduzca la tarjeta para operar"
               AT LINE 10 COL 22.

           DISPLAY "Enter - Aceptar"
                AT LINE 24 COL 33.

       P1-ENTER. *> Estaba en AT LINE 24 COL 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 80 ON EXCEPTION
           IF ENTER-PRESSED
               GO TO P2
           ELSE
               GO TO P1-ENTER.

       
       P2.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "ESC - Salir" AT LINE 24 COL 33.
           INITIALIZE TNUM.
           INITIALIZE PIN-INTRODUCIDO.
           INITIALIZE TPIN.
           DISPLAY "Numero de tarjeta:" AT LINE 8 COL 15.
           DISPLAY "Inserte el pin de tarjeta:"
                AT LINE 9 COL 15.
           ACCEPT DATA-ACCEPT ON EXCEPTION *> IRENE: delete AT
               IF ESC-PRESSED
                   GO TO IMPRIMIR-CABECERA
               ELSE
                   GO TO P2.
           *>COMPRUEBA SI LA TARJETA ES VALIDA
           OPEN I-O TARJETAS.
           IF FST NOT = 00 THEN
               GO TO PSYS-ERR.
           READ TARJETAS INVALID KEY GO TO PSYS-ERR.
           *>COMPRUEBA SI AL USUARIO LE QUEDAN INTENTOS 
           OPEN I-O INTENTOS.
           IF FSI NOT = 00 THEN
               GO TO PSYS-ERR.
           MOVE TNUM TO INUM.

           READ INTENTOS INVALID KEY GO TO PSYS-ERR.
           *> SI NO LE QUEDAN INTENTOS MUESTRA EL ERROR
           IF IINTENTOS = 0 THEN
               GO TO PINT-ERR.
         
           *> LE INDICA QUE SE HA EQUIVOCADO
           IF PIN-INTRODUCIDO NOT = TPIN THEN
               GO TO PPIN-ERR.
 

           PERFORM REINICIAR-INTENTOS THRU REINICIAR-INTENTOS.

       PMENU.
           CLOSE TARJETAS.
           CLOSE INTENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "1 - Consultar saldo" AT LINE 8 COL 15.
           DISPLAY "2 - Consultar movimientos" AT LINE 9 COL 15.
           DISPLAY "3 - Retirar efectivo" AT LINE 10 COL 15.
           DISPLAY "4 - Ingresar efectivo" AT LINE 11 COL 15.
           DISPLAY "5 - Ordenar transferencia"
               AT LINE 12 COL 15.
           DISPLAY "6 - Listado de transferencias"
               AT LINE 13 COL 15.
           DISPLAY "7 - Comprar entradas de espectaculos"
               AT LINE 14 COL 15.
           DISPLAY "8 - Cambiar clave" AT LINE 15 COL 15.
           DISPLAY "ESC - Salir" AT LINE 24 COL 34.

       PMENUA1. *> Estaba en 24 80) (
           ACCEPT CHOICE AT LINE 24 COL 79 ON EXCEPTION
               IF ESC-PRESSED
                   GO TO IMPRIMIR-CABECERA
               ELSE
                   GO TO PMENUA1.

           IF CHOICE = 1
               CALL "BANK2" USING TNUM.

           IF CHOICE = 2
               CALL "BANK3" USING TNUM.

           IF CHOICE = 3
               CALL "BANK4" USING TNUM.
           IF CHOICE = 4
               CALL "BANK5" USING TNUM.

           IF CHOICE = 5
               CALL "BANK6" USING TNUM.
           IF CHOICE = 6
               CALL "BANK9" USING TNUM.

           IF CHOICE = 7
               CALL "BANK7" USING TNUM.

           IF CHOICE = 8
           *>DE ESTA FORMA CADA VEZ QUE VUELVA COMPROBARA SI LE QUEDAN
           *> INTENTOS O NO
               CALL "BANK8" USING TNUM
               PERFORM CALCULAR-INTENTOS THRU CALCULAR-INTENTOS.

           
           GO TO PMENU.


       PSYS-ERR.

           CLOSE TARJETAS.
           CLOSE INTENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha ocurrido un error interno"
               AT LINE 9 COL 25
               WITH FOREGROUND-COLOR IS WHITE
                   BACKGROUND-COLOR IS RED.
           DISPLAY "Vuelva mas tarde"
               AT LINE 11 COL 32
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Aceptar" AT LINE 24 COL 33.

           GO TO PINT-ERR-ENTER.


       PINT-ERR.

           CLOSE TARJETAS.
           CLOSE INTENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Se ha sobrepasado el numero de intentos"
               AT LINE 9 COL 20
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Por su seguridad se ha bloqueado la tarjeta"
               AT LINE 11 COL 18
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
                    
           DISPLAY "Acuda a una sucursal"
               AT LINE 12 COL 30
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.

           DISPLAY "Enter - Aceptar" AT LINE 24 COL 33.

       PINT-ERR-ENTER.
       *>AT LINE 24 COL 80 

           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 80 ON EXCEPTION 
           IF ENTER-PRESSED THEN
               GO TO IMPRIMIR-CABECERA
           ELSE 
               GO TO PINT-ERR-ENTER.


       PPIN-ERR.
           SUBTRACT 1 FROM IINTENTOS.
           REWRITE INTENTOSREG INVALID KEY GO TO PSYS-ERR.

           CLOSE TARJETAS.
           CLOSE INTENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "El codigo PIN es incorrecto"
               AT LINE 9 COL 26
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY"Le quedan "
               AT LINE 11 COL 30
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY IINTENTOS
               AT LINE 11 COL 40
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.

           DISPLAY " intentos"
               AT LINE 11 COL 42 
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.

           DISPLAY "Enter - Aceptar" AT LINE 24 COL 1.
           DISPLAY "ESC - Cancelar" AT LINE 24 COL 65.

       PPIN-ERR-ENTER.
       *> Estaba en 24 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 80 ON EXCEPTION
           IF ENTER-PRESSED
               GO TO P2
           ELSE
               IF ESC-PRESSED
                   GO TO IMPRIMIR-CABECERA
               ELSE
                   GO TO PPIN-ERR-ENTER
           END-IF.
       REINICIAR-INTENTOS.
           MOVE 3 TO IINTENTOS.
           REWRITE INTENTOSREG INVALID KEY GO TO PSYS-ERR.

       CALCULAR-INTENTOS.
           OPEN I-O INTENTOS.
           IF FSI NOT = 00 THEN
               GO TO PSYS-ERR.
           MOVE TNUM TO INUM.

           READ INTENTOS INVALID KEY GO TO PSYS-ERR.

           IF IINTENTOS = 0 THEN
               GO TO IMPRIMIR-CABECERA.
              
           CLOSE INTENTOS.       