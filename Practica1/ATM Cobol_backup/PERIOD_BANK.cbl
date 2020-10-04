       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERIOD_BANK.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM-E
           FILE STATUS IS FST.

           SELECT OPTIONAL TRANSFERENCIAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TRANS-NUM
           FILE STATUS IS FSTR.

       DATA DIVISION.
       FILE SECTION.
       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TAJETAREG.
           02 TNUM-E      PIC 9(16).
           02 TPIN-E      PIC  9(4).
       FD TRANSFERENCIAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "transferencias.ubd".
       01 TRANSFERENCIA-REG.
           02 TRANS-NUM            PIC  9(35).
           02 TRANS-TARJETA-ORD    PIC  9(16).
           02 TRANS-TARJETA-DST    PIC  9(16).
           02 TRANS-ANO            PIC   9(4).
           02 TRANS-MES            PIC   9(2).
           02 TRANS-DIA            PIC   9(2).
           02 TRANS-IMPORTE-ENT    PIC  S9(7).
           02 TRANS-IMPORTE-DEC    PIC   9(2).
           02 TRANS-PERIODO        PIC  X(35).

       WORKING-STORAGE SECTION.
       01 CHECKERR                 PIC   X(24).
       77 FST                      PIC   X(2).
       77 FSTR                     PIC   X(2).

       78 BLACK                  VALUE      0.
       78 BLUE                   VALUE      1.
       78 GREEN                  VALUE      2.
       78 CYAN                   VALUE      3.
       78 RED                    VALUE      4.
       78 MAGENTA                VALUE      5.
       78 YELLOW                 VALUE      6.
       78 WHITE                  VALUE      7.

       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO              PIC   9(4).
               10 MES              PIC   9(2).
               10 DIA              PIC   9(2).
           05 HORA.
               10 HORAS            PIC   9(2).
               10 MINUTOS          PIC   9(2).
               10 SEGUNDOS         PIC   9(2).
               10 MILISEGUNDOS     PIC   9(2).
           05 DIF-GMT              PIC  S9(4).

       01 KEYBOARD-STATUS          PIC  9(4).
           88 ENTER-PRESSED      VALUE     0.
           88 PGUP-PRESSED       VALUE  2001.
           88 PGDN-PRESSED       VALUE  2002.
           88 UP-ARROW-PRESSED   VALUE  2003.
           88 DOWN-ARROW-PRESSED VALUE  2004.
           88 ESC-PRESSED        VALUE  2005.
           88 F1-PRESSED         VALUE  1001.
           88 F2-PRESSED         VALUE  1002.

       77 PRESSED-KEY              PIC   9(4).

       77 TNUM-DST                 PIC   9(16).
       77 NOMBRE-DESTINO           PIC  X(15).
       77 DIA-TRANS-USUARIO        PIC   9(2).
       77 MES-TRANS-USUARIO        PIC   9(2).
       77 ANO-TRANS-USUARIO        PIC   9(4).
       77 EURENT-USUARIO           PIC   9(7).
       77 EURDEC-USUARIO           PIC   9(2).
       77 FECHA-TOTAL-USUARIO      PIC   9(8).
       77 FECHA-TOTAL-ACTUAL       PIC   9(8).
       77 LAST-TRANS-NUM           PIC   9(35).
       

       77 MSJ-MENSUAL-PERIOD       PIC  X(35) VALUE "Mensual".
       77 MSJ-PUNTUAL-PERIOD       PIC  X(35) VALUE "Puntual".    
       
       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).
       77 MSJ-PERIOD               PIC  X(35).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       *> Se recoge la fecha que se quiere hacer la transaccion
       01 DATOS-TRANS.
           05 CUENTA-DESTINO BLANK ZERO AUTO UNDERLINE
               LINE 9 COL 47 PIC 9(16) USING TNUM-DST.
           05 CUENTA-NOMBRE AUTO UNDERLINE
               LINE 11 COL 54 PIC  X(15) USING NOMBRE-DESTINO.
           05 DIA-TRANS BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 41 PIC 9(2) USING DIA-TRANS-USUARIO.
           05 MES-TRANS BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 44 PIC 9(2) USING MES-TRANS-USUARIO.
           05 ANO-TRANS BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 47 PIC 9(4) USING ANO-TRANS-USUARIO.
           05 EUR-ENT BLANK ZERO AUTO UNDERLINE
               LINE 15 COL 41 PIC 9(7) USING EURENT-USUARIO.
           05 EUR-DEC BLANK ZERO UNDERLINE
               LINE 15 COL 51 PIC 9(2) USING EURDEC-USUARIO.
           


       PROCEDURE DIVISION USING TNUM, MSJ-PERIOD.
       IMPRIMIR-CABECERA.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           
           DISPLAY BLANK-SCREEN.
           DISPLAY "Cajero Automatico UnizarBank"
               AT LINE 2 COL 26 WITH FOREGROUND-COLOR IS 1.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY DIA AT LINE 4 COL 32.
           DISPLAY "-" AT LINE 4 COL 34.
           DISPLAY MES AT LINE 4 COL 35.
           DISPLAY "-" AT LINE 4 COL 37.
           DISPLAY ANO AT LINE 4 COL 38.
           DISPLAY HORAS AT LINE 4 COL 44.
           DISPLAY ":" AT LINE 4 COL 46.
           DISPLAY MINUTOS AT LINE 4 COL 47.

       PCONSULTA-TRANS.
           
           INITIALIZE TNUM-DST.
           INITIALIZE CUENTA-NOMBRE.
           INITIALIZE DIA-TRANS-USUARIO.
           INITIALIZE MES-TRANS-USUARIO.
           INITIALIZE ANO-TRANS-USUARIO.
           INITIALIZE EURENT-USUARIO.
           INITIALIZE EURDEC-USUARIO.
           INITIALIZE FECHA-TOTAL-ACTUAL.
           INITIALIZE FECHA-TOTAL-USUARIO.
           INITIALIZE LAST-TRANS-NUM.
           
           
           DISPLAY "Transaccion de tipo: " AT LINE 7 COL 20.
           DISPLAY MSJ-PERIOD AT LINE 7 COL 41.
           DISPLAY "Indique la cuenta destino: " AT LINE 9 COL 20.
           DISPLAY "Indique el nombre del titular: " AT LINE 11 COL 20.
           DISPLAY "Indique la fecha:      /  /     " AT LINE 13 COL 20.
           DISPLAY "Indique el importe: " AT LINE 15 COL 20.
           DISPLAY ",   EUR" AT LINE 15 COL 50.
           
           
           DISPLAY "Enter - Aceptar" AT LINE 24 COL 01.
           DISPLAY "ESC - Cancelar" AT LINE 24 COL 65.

           *> Recoger datos de la transaccion
           ACCEPT DATOS-TRANS ON EXCEPTION
           IF ESC-PRESSED
               EXIT PROGRAM
           END-IF.

       VERIFICACION-FECHA-CORRECTA.

           COMPUTE FECHA-TOTAL-USUARIO = (ANO-TRANS-USUARIO * 10000)
                                       + (MES-TRANS-USUARIO * 100) 
                                       + (DIA-TRANS-USUARIO).
           
           COMPUTE FECHA-TOTAL-ACTUAL = (ANO * 10000)
                                       + (MES * 100) 
                                       + (DIA).

           IF FECHA-TOTAL-USUARIO <= FECHA-TOTAL-ACTUAL
               THEN GO TO DATE-BAD
           END-IF.

           IF (MES-TRANS-USUARIO <= 0 OR MES-TRANS-USUARIO > 12)
            THEN GO TO DATE-INVALID
           END-IF.

           IF (DIA-TRANS-USUARIO <= 0 OR DIA-TRANS-USUARIO > 31)
            THEN GO TO DATE-INVALID
           END-IF.

       OPEN-TRANSFERENCIAS.
           OPEN I-O TRANSFERENCIAS CLOSE TRANSFERENCIAS.
           OPEN I-O TRANSFERENCIAS.
           IF FSTR <> 00 THEN
               GO TO PSYS-ERR
           END-IF.

       LEER-ULTIMA-TRANS.
           *> Coger el numero de la ultima transferencia
           READ TRANSFERENCIAS NEXT RECORD AT END GO TO 
               VERIFICACION-DATOS.
               IF LAST-TRANS-NUM < TRANS-NUM THEN
                   MOVE TRANS-NUM TO LAST-TRANS-NUM
               END-IF.
               GO TO LEER-ULTIMA-TRANS.

       VERIFICACION-DATOS.
           *> Preparar datos de la transferencia
           ADD 1 TO LAST-TRANS-NUM.

           MOVE LAST-TRANS-NUM         TO TRANS-NUM.
           MOVE TNUM                   TO TRANS-TARJETA-ORD.
           MOVE TNUM-DST               TO TRANS-TARJETA-DST.
           MOVE ANO-TRANS-USUARIO      TO TRANS-ANO.
           MOVE MES-TRANS-USUARIO      TO TRANS-MES.
           MOVE DIA-TRANS-USUARIO      TO TRANS-DIA.
           MOVE EUR-ENT                TO TRANS-IMPORTE-ENT.
           MOVE EUR-DEC                TO TRANS-IMPORTE-DEC.
           MOVE MSJ-PERIOD             TO TRANS-PERIODO.

       

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           
           DISPLAY "Se va a programar la siguiente transferencia:"
               AT LINE 6 COL 15.
           *>DISPLAY TRANS-NUM AT LINE 8 COL 20.
           DISPLAY "Ordenante: " AT LINE 10 COL 20.
           DISPLAY TRANS-TARJETA-ORD AT LINE 10 COL 35.
           DISPLAY "Destinatario: " AT LINE 12 COL 20.
           DISPLAY TRANS-TARJETA-DST AT LINE 12 COL 35.
           DISPLAY "Titular: " AT LINE 14 COL 20.
           DISPLAY NOMBRE-DESTINO AT LINE 14 COL 35.
           DISPLAY "Fecha: " AT LINE 16 COL 20.
           DISPLAY TRANS-DIA AT LINE 16 COL 35.
           DISPLAY "/" AT LINE 16 COL 37.
           DISPLAY TRANS-MES AT LINE 16 COL 38.
           DISPLAY "/" AT LINE 16 COL 40.
           DISPLAY TRANS-ANO AT LINE 16 COL 41.
           DISPLAY "Importe: " AT LINE 18 COL 20.
           DISPLAY TRANS-IMPORTE-ENT AT LINE 18 COL 35.
           DISPLAY "," AT LINE 18 COL 42.
           DISPLAY TRANS-IMPORTE-DEC AT LINE 18 COL 43.
           DISPLAY "EUR" AT LINE 18 COL 46.
           DISPLAY "Tipo: " AT LINE 20 COL 20.
           DISPLAY TRANS-PERIODO AT LINE 20 COL 35.

           DISPLAY "Enter - Aceptar" AT LINE 24 COL 01.
           DISPLAY "ESC - Cancelar" AT LINE 24 COL 65.

           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79 ON EXCEPTION
           IF ENTER-PRESSED THEN
               GO TO ESCRIBIR-TRANS
           ELSE
               IF ESC-PRESSED THEN
                   EXIT PROGRAM
               ELSE 
                   GO TO VERIFICACION-DATOS
               END-IF
           END-IF. 
           
       ESCRIBIR-TRANS.
       *> Escribir en transferencias
           WRITE TRANSFERENCIA-REG INVALID KEY GO TO PSYS-ERR.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "La transferencia se ha programado correctamente."
               AT LINE 10 COL 17.
           DISPLAY "Tenga un buen dia."
               AT LINE 12 COL 31.

           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79.
           EXIT PROGRAM.

       EXIT-ENTER.
           DISPLAY "Enter - Salir" AT LINE 24 COL 33.
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79.
           CLOSE TRANSFERENCIAS.
           EXIT PROGRAM.

       DATE-BAD.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "La fecha introducida debe ser posterior"
               AT LINE 9 COL 22
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           GO TO EXIT-ENTER.
       
       DATE-INVALID.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "La fecha introducida no es valida"
               AT LINE 9 COL 22
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           GO TO EXIT-ENTER.

       PSYS-ERR.
           CLOSE TARJETAS.
           CLOSE TRANSFERENCIAS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY  "Ha ocurrido un error interno"
               AT LINE 9 COL 25
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           DISPLAY  "Vuelva mas tarde"
               AT LINE 11 COL 32
               WITH FOREGROUND-COLOR IS BLACK
                    BACKGROUND-COLOR IS RED.
           GO TO EXIT-ENTER.


       
