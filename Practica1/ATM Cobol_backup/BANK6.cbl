       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK6.

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

           SELECT OPTIONAL  F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.


       DATA DIVISION.
       FILE SECTION.
       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TAJETAREG.
           02 TNUM-E      PIC 9(16).
           02 TPIN-E      PIC  9(4).
       FD F-MOVIMIENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "movimientos.ubd".
       01 MOVIMIENTO-REG.
           02 MOV-NUM              PIC  9(35).
           02 MOV-TARJETA          PIC  9(16).
           02 MOV-ANO              PIC   9(4).
           02 MOV-MES              PIC   9(2).
           02 MOV-DIA              PIC   9(2).
           02 MOV-HOR              PIC   9(2).
           02 MOV-MIN              PIC   9(2).
           02 MOV-SEG              PIC   9(2).
           02 MOV-IMPORTE-ENT      PIC  S9(7).
           02 MOV-IMPORTE-DEC      PIC   9(2).
           02 MOV-CONCEPTO         PIC  X(35).
           02 MOV-SALDOPOS-ENT     PIC  S9(9).
           02 MOV-SALDOPOS-DEC     PIC   9(2).


       WORKING-STORAGE SECTION.
       *>> VARIABLE PARA COMPROBAR DONDE ESTA EL ERROR
       01 CHECKERR                    PIC   X(24).
       77 FST                      PIC   X(2).
       77 FSM                      PIC   X(2).

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

       77 LAST-MOV-NUM             PIC  9(35).
       77 LAST-USER-ORD-MOV-NUM    PIC  9(35).
       77 LAST-USER-DST-MOV-NUM    PIC  9(35).

       77 EURENT-USUARIO           PIC  S9(7).
       77 EURDEC-USUARIO           PIC   9(2).
       77 CUENTA-DESTINO           PIC  9(16).
       77 NOMBRE-DESTINO           PIC  X(35).

       77 CENT-SALDO-ORD-USER      PIC  S9(9).
       77 CENT-SALDO-DST-USER      PIC  S9(9).
       77 CENT-IMPOR-USER          PIC  S9(9).

       77 MSJ-ORD                  PIC  X(35) VALUE "Transferimos".
       77 MSJ-DST                  PIC  X(35) VALUE "Nos transfieren".

       77 MSJ-PERIOD               PIC  X(35).

       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 FILTRO-CUENTA.
           05 FILLER BLANK WHEN ZERO AUTO UNDERLINE
               LINE 12 COL 54 PIC 9(16) USING CUENTA-DESTINO.
           05 FILLER AUTO UNDERLINE
               LINE 14 COL 54 PIC X(15) USING NOMBRE-DESTINO.

           *> Se ha cambiado
           *> eliminando BLANK ZERO Y UNA S AL TIPO DE DATO
           05 FILLER AUTO UNDERLINE
               SIGN IS LEADING SEPARATE
               LINE 16 COL 54 PIC S9(7) USING EURENT-USUARIO.
           05 FILLER BLANK ZERO UNDERLINE
               LINE 16 COL 63 PIC 9(2) USING EURDEC-USUARIO.

       01 SALDO-DISPLAY.
           05 FILLER SIGN IS LEADING SEPARATE
               LINE 10 COL 33 PIC -9(7) FROM MOV-SALDOPOS-ENT.
           05 FILLER LINE 10 COL 41 VALUE ",".
           05 FILLER LINE 10 COL 42 PIC 99 FROM MOV-SALDOPOS-DEC.
           05 FILLER LINE 10 COL 45 VALUE "EUR".


       PROCEDURE DIVISION USING TNUM.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.

           INITIALIZE CUENTA-DESTINO.
           INITIALIZE NOMBRE-DESTINO.
           INITIALIZE EURENT-USUARIO.
           INITIALIZE EURDEC-USUARIO.
           INITIALIZE LAST-MOV-NUM.
           INITIALIZE LAST-USER-ORD-MOV-NUM.
           INITIALIZE LAST-USER-DST-MOV-NUM.
           INITIALIZE MSJ-PERIOD.

       IMPRIMIR-CABECERA.

          DISPLAY BLANK-SCREEN.
           DISPLAY "Cajero Automatico UnizarBank"
               AT LINE 2 COL 26
               WITH FOREGROUND-COLOR IS 1.


           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY DIA AT LINE 4 COL 32.
           DISPLAY "-" AT LINE 4 COL 34.
           DISPLAY MES AT LINE 4 COL 35.
           DISPLAY "-" AT LINE 4 COL 37.
           DISPLAY ANO AT LINE 4 COL 38.
           DISPLAY HORAS AT LINE 4 COL 44.
           DISPLAY ":" AT LINE 4 COL 46.
           DISPLAY MINUTOS AT LINE 4 COL 47.


       MOVIMIENTOS-OPEN.

           *>FORZAMOS QUE CREE UN FICHERO POR SI NO EXISTE
           OPEN I-O F-MOVIMIENTOS CLOSE F-MOVIMIENTOS.

           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 00 THEN
               GO TO PSYS-ERR
           END-IF.

       LECTURA-MOVIMIENTOS.
           *> Se obtiene el ultimo movimiento de la tarjeta/cuenta
           READ F-MOVIMIENTOS NEXT RECORD AT END GO TO ORDENACION-TRF.
           IF MOV-TARJETA = TNUM THEN
               IF LAST-USER-ORD-MOV-NUM < MOV-NUM THEN
                   *> Guardo el ultimo mov del usuario
                   MOVE MOV-NUM TO LAST-USER-ORD-MOV-NUM
               END-IF
           END-IF.
           IF LAST-MOV-NUM < MOV-NUM THEN
               MOVE MOV-NUM TO LAST-MOV-NUM
           END-IF.
           GO TO LECTURA-MOVIMIENTOS.

       ORDENACION-TRF.
           CLOSE F-MOVIMIENTOS.

           DISPLAY "Ordenar Transferencia" AT LINE 8 COL 30.
           DISPLAY "Saldo Actual:" AT LINE 10 COL 19.

           DISPLAY "Enter - Confirmar" AT LINE 24 COL 2.
           DISPLAY "ESC - Cancelar" AT LINE 24 COL 66.

           DISPLAY "F1 - T.Mensual" AT LINE 24 COL 26.
           DISPLAY "F2 - T.Puntual" AT LINE 24 COL 42.

           IF LAST-USER-ORD-MOV-NUM = 0 THEN
               GO TO NO-MOVIMIENTOS
           END-IF.

           MOVE LAST-USER-ORD-MOV-NUM TO MOV-NUM.
           *> DE AQUI ES DE DONDE SACA LA INFO DEL QUE ENVIA
           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.
           *> Se leen los datos del ultimo movimiento del usuario
           READ F-MOVIMIENTOS INVALID KEY GO PSYS-ERR.
           DISPLAY SALDO-DISPLAY.
           CLOSE F-MOVIMIENTOS.


       INDICAR-CTA-DST.
           DISPLAY "Indica la cuenta destino"
                AT LINE 12 COL 19.
           DISPLAY "y nombre del titular"
                AT LINE 14 COL 19.
           DISPLAY "Indique la cantidad a transferir"
                AT LINE 16 COL 19.
           DISPLAY "," AT LINE 16 COL 61.
           DISPLAY "EUR" AT LINE 16 COL 66.

           COMPUTE CENT-SALDO-ORD-USER = (MOV-SALDOPOS-ENT * 100)
                                         + MOV-SALDOPOS-DEC.
           
           *> Recoger cuenta y nombre destinatario
           ACCEPT FILTRO-CUENTA ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           ELSE
               IF F1-PRESSED THEN
                   MOVE "Mensual" TO MSJ-PERIOD
                   CALL "PERIOD_BANK" USING TNUM, MSJ-PERIOD
                   EXIT PROGRAM
               ELSE
                   IF F2-PRESSED THEN
                       MOVE "Puntual" TO MSJ-PERIOD
                       CALL "PERIOD_BANK" USING TNUM, MSJ-PERIOD
                       EXIT PROGRAM
                   ELSE
                       GO TO INDICAR-CTA-DST
                   END-IF
               END-IF
           END-IF.

           COMPUTE CENT-IMPOR-USER = (EURENT-USUARIO * 100)
                                     + EURDEC-USUARIO.

           IF CENT-IMPOR-USER > CENT-SALDO-ORD-USER THEN
                   DISPLAY "Indique una cantidad menor!!"
                    AT LINE 20 COL 19
                    WITH BACKGROUND-COLOR RED
                   GO TO INDICAR-CTA-DST
           END-IF.

           GO TO REALIZAR-TRF-VERIFICACION.

       NO-MOVIMIENTOS.
           *> No hay movimientos y se muestra saldo 0 
           DISPLAY "0"  AT LINE 10 COL 51.
           DISPLAY "."  AT LINE 10 COL 52.
           DISPLAY "00"  AT LINE 10 COL 53.
           DISPLAY "EUR"  AT LINE 10 COL 54.

           DISPLAY "Indica la cuenta destino "  AT LINE 12 COL 19.
           DISPLAY  "y nombre del titular"  AT LINE 14 COL 19.
           DISPLAY "Indique la cantidad a transferir" AT LINE 16 COL 19.
           DISPLAY ","  AT LINE 16 COL 61.
           DISPLAY "EUR"  AT LINE 16 COL 66.

           *> Recoger cuenta y nombre destinatario
           ACCEPT FILTRO-CUENTA ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           END-IF.
           
           *> Siempre se va a rechazar porque el saldo es 0 !!
           DISPLAY "No hay dinero en la cuenta!!"
            AT LINE 20 COL 19
            WITH BACKGROUND-COLOR RED.

           GO TO NO-MOVIMIENTOS.

       REALIZAR-TRF-VERIFICACION.
           *> Si que hay movimientos y se muestra el saldo
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ordenar Transferencia"  AT LINE 08 COL 30.
           DISPLAY "Va a transferir:"  AT LINE 11 COL 19.
           DISPLAY EURENT-USUARIO  AT LINE 11 COL 38.
           DISPLAY "."  AT LINE 11 COL 45.
           DISPLAY EURDEC-USUARIO  AT LINE 11 COL 46.
           DISPLAY  "EUR de su cuenta"  AT LINE 11 COL 49.
           DISPLAY  "a la cuenta cuyo titular es"  AT LINE 12 COL 19.
           DISPLAY  NOMBRE-DESTINO  AT LINE 12 COL 48.

           DISPLAY  "Enter - Confirmar"  AT LINE 24 COL 2.
           DISPLAY "ESC - Cancelar"  AT LINE 24 COL 62.

       ENTER-VERIFICACION.
       *> 24 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 75  ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           ELSE
               GO TO ENTER-VERIFICACION
           END-IF.

       VERIFICACION-CTA-CORRECTA.
           OPEN I-O TARJETAS.
           IF FST <> 00
              GO TO PSYS-ERR.

           *> Comprobar tarjeta destino
           MOVE CUENTA-DESTINO TO TNUM-E.
           READ TARJETAS INVALID KEY GO TO USER-BAD.
           CLOSE TARJETAS.

           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.
           MOVE 0 TO MOV-NUM.
           MOVE 0 TO LAST-USER-DST-MOV-NUM.

       LECTURA-SALDO-DST.
           *> Buscamos los movimientos de la tarjeta destino para
           *> encontrar el ultimo saldo
           READ F-MOVIMIENTOS NEXT RECORD AT END GO TO GUARDAR-TRF.
           IF MOV-TARJETA = CUENTA-DESTINO THEN
               IF LAST-USER-DST-MOV-NUM < MOV-NUM THEN
                   *> Nos quedamos con el ultimo mov del usuario
                   MOVE MOV-NUM TO LAST-USER-DST-MOV-NUM
               END-IF
           END-IF.

           GO TO LECTURA-SALDO-DST.


       GUARDAR-TRF.
           CLOSE F-MOVIMIENTOS.
           MOVE LAST-USER-DST-MOV-NUM TO MOV-NUM.
           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.
           *> Si la cuenta destino no tiene mov -> saldo=0
           READ F-MOVIMIENTOS INVALID KEY GO NO-MONEY.

       CALCULO-SALDO-DESTINO-USUARIO.    
           *> Calculamos el saldo de la cuenat destino en centimos
           COMPUTE CENT-SALDO-DST-USER = (MOV-SALDOPOS-ENT * 100)
                                         + MOV-SALDOPOS-DEC.

           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.
       
           ADD 1 TO LAST-MOV-NUM.

           *> Preparamos datos para guardar el mov de transferencia
           *> respecto a la cuenta del ordenante
           MOVE LAST-MOV-NUM   TO MOV-NUM.
           MOVE TNUM           TO MOV-TARJETA.
           MOVE ANO            TO MOV-ANO.
           MOVE MES            TO MOV-MES.
           MOVE DIA            TO MOV-DIA.
           MOVE HORAS          TO MOV-HOR.
           MOVE MINUTOS        TO MOV-MIN.
           MOVE SEGUNDOS       TO MOV-SEG.

           MULTIPLY -1 BY EURENT-USUARIO.
           MOVE EURENT-USUARIO TO MOV-IMPORTE-ENT.
           MULTIPLY -1 BY EURENT-USUARIO.
           MOVE EURDEC-USUARIO TO MOV-IMPORTE-DEC.
           *> REGISTRAMOS LA TRANSFERENCIA
           MOVE MSJ-ORD        TO MOV-CONCEPTO.
           *> AL SALDO DEL USUARIO LE QUITAMOS EL DINERO QUE VA ENVIAR
           SUBTRACT CENT-IMPOR-USER FROM CENT-SALDO-ORD-USER.

           *> Se vuelve a calcular cent a euros
           COMPUTE MOV-SALDOPOS-ENT = (CENT-SALDO-ORD-USER / 100).
           MOVE FUNCTION MOD(CENT-SALDO-ORD-USER, 100)
               TO MOV-SALDOPOS-DEC.
           *> Se escribe el movimiento respecto al ordenante
           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.

           ADD 1 TO LAST-MOV-NUM.

           *> Se preparan los datos del mov respecto al receptor
           MOVE LAST-MOV-NUM   TO MOV-NUM.
           MOVE CUENTA-DESTINO TO MOV-TARJETA.
           MOVE ANO            TO MOV-ANO.
           MOVE MES            TO MOV-MES.
           MOVE DIA            TO MOV-DIA.
           MOVE HORAS          TO MOV-HOR.
           MOVE MINUTOS        TO MOV-MIN.
           MOVE SEGUNDOS       TO MOV-SEG.

           MOVE EURENT-USUARIO TO MOV-IMPORTE-ENT.
           MOVE EURDEC-USUARIO TO MOV-IMPORTE-DEC.

           MOVE MSJ-DST        TO MOV-CONCEPTO.

           *> Se aumenta el saldo del receptor y se pasa a EUR
           ADD CENT-IMPOR-USER TO CENT-SALDO-DST-USER.
           COMPUTE MOV-SALDOPOS-ENT = (CENT-SALDO-DST-USER / 100).
           MOVE FUNCTION MOD(CENT-SALDO-DST-USER, 100)
               TO MOV-SALDOPOS-DEC.

           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.

           CLOSE F-MOVIMIENTOS.

       P-EXITO.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.

           DISPLAY  "Ordenar transferencia" AT LINE 8 COL 30.
           DISPLAY "Transferencia realizada correctamente!"
                AT LINE 11 COL 19.
           DISPLAY  "Enter - Aceptar" AT LINE 24 COL 33.

           GO TO EXIT-ENTER.

       PSYS-ERR.
           CLOSE TARJETAS.
           CLOSE F-MOVIMIENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY  "Ha ocurrido un error interno"
               AT LINE 9 COL 25
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY  "Vuelva mas tarde"
               AT LINE 11 COL 32
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.

           DISPLAY "Enter - Aceptar" AT LINE 24 COL 33.

       EXIT-ENTER.
       *> 24 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79 
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.

       USER-BAD.
           CLOSE TARJETAS.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "La cuenta introducida es incorrecta"
               AT LINE 9 COL 22
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Salir" AT LINE 24 COL 33.
           GO TO EXIT-ENTER.
       *> ESTO SE HA PUESTO PARA SOLUCIONAR CUANDO UNA TARJETA NO TIENE
       *> MOVIMIENTOS
       NO-MONEY.

           MOVE 0 TO MOV-SALDOPOS-ENT.
           MOVE 0 TO MOV-SALDOPOS-DEC.
           GO TO CALCULO-SALDO-DESTINO-USUARIO.
