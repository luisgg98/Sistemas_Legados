       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK7.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.

           SELECT F-ESPECTACULOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ESP-NUM
           FILE STATUS IS FSE.


       DATA DIVISION.
       FILE SECTION.
       FD F-MOVIMIENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "movimientos.ubd".
       01 MOVIMIENTO-REG.
           02 MOV-NUM               PIC  9(35).
           02 MOV-TARJETA           PIC  9(16).
           02 MOV-ANO               PIC   9(4).
           02 MOV-MES               PIC   9(2).
           02 MOV-DIA               PIC   9(2).
           02 MOV-HOR               PIC   9(2).
           02 MOV-MIN               PIC   9(2).
           02 MOV-SEG               PIC   9(2).
           02 MOV-IMPORTE-ENT       PIC  S9(7).
           02 MOV-IMPORTE-DEC       PIC   9(2).
           02 MOV-CONCEPTO          PIC  X(35).
           02 MOV-SALDOPOS-ENT      PIC  S9(9).
           02 MOV-SALDOPOS-DEC      PIC   9(2).

       FD F-ESPECTACULOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "espectaculos.ubd".
       01 ESPECTACULO-REG.
           02 ESP-NUM               PIC   9(4).
           02 ESP-ANO               PIC   9(4).
           02 ESP-MES               PIC   9(2).
           02 ESP-DIA               PIC   9(2).
           02 ESP-HOR               PIC   9(2).
           02 ESP-MIN               PIC   9(2).
           02 ESP-DESCR             PIC  X(40).
           02 ESP-DISP              PIC   9(7).
           02 ESP-PRECIO-ENT        PIC   9(4).
           02 ESP-PRECIO-DEC        PIC   9(2).


       WORKING-STORAGE SECTION.
       77 FSM                       PIC   X(2).
       77 FSE                       PIC   X(2).

       78 BLACK                     VALUE    0.
       78 BLUE                      VALUE    1.
       78 GREEN                     VALUE    2.
       78 CYAN                      VALUE    3.
       78 RED                       VALUE    4.
       78 MAGENTA                   VALUE    5.
       78 YELLOW                    VALUE    6.
       78 WHITE                     VALUE    7.

       01 CAMPOS-FECHA.
           05 FECHA.
               10 ANO               PIC   9(4).
               10 MES               PIC   9(2).
               10 DIA               PIC   9(2).
           05 HORA.
               10 HORAS             PIC   9(2).
               10 MINUTOS           PIC   9(2).
               10 SEGUNDOS          PIC   9(2).
               10 MILISEGUNDOS      PIC   9(2).
           05 DIF-GMT               PIC  S9(4).

       01 KEYBOARD-STATUS           PIC   9(4).
           88 ENTER-PRESSED         VALUE    0.
           88 PGUP-PRESSED          VALUE 2001.
           88 PGDN-PRESSED          VALUE 2002.
           88 UP-ARROW-PRESSED      VALUE 2003.
           88 DOWN-ARROW-PRESSED    VALUE 2004.
           88 ESC-PRESSED           VALUE 2005.
       77 PRESSED-KEY   BLANK WHEN ZERO            PIC   9(4).

       77 USER-NUM-ENTRADAS         PIC   9(2).
       77 USER-NUM-ESPECT           PIC   9(4).
       77 SALDO-USER-ENT            PIC  S9(9).
       77 SALDO-USER-DEC            PIC   9(2).
       77 SALDO-USER-CENT           PIC S9(11).
       77 IMPORTE-ENTRADAS-CENT     PIC   9(8).
       77 IMPORTE-ENTRADAS-ENT      PIC  S9(6).
       77 IMPORTE-ENTRADAS-DEC      PIC   9(2).
       77 ENTRAD-CONCEPTO           PIC  X(40).

       77 ESP-EN-PANTALLA           PIC   9(2).
       77 LINEA-ESP-ACTUAL          PIC   9(2).
       77 MODULO-LIN-ACTUAL         PIC   9(1).
       77 LAST-MOV-NUM              PIC  9(35).
       77 LAST-USER-MOV-NUM         PIC  9(35).
       77 SALDO-POST-ENT            PIC  S9(9).
       77 SALDO-POST-DEC            PIC   9(2).


       01 TABLA.
           05 REGISTROS-EN-PANTALLA PIC  9(35) OCCURS 15 TIMES.

       77 CONTADOR                  PIC   9(2).
       77 ITERACIONES               PIC   9(2).
       77 COPIA-ESP                 PIC  9(35).
       77 ESP-VALIDO                PIC   9(1).
       77 FECHA-ESP-FILTRO          PIC   9(8).
       77 FECHA-ACTUAL              PIC   9(8).

       LINKAGE SECTION.
       77 TNUM                      PIC  9(16).


       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 INFO-SALDO.
           05 FILLER LINE 07 COL 26 VALUE IS "Saldo actual:".
           05 FILLER SIGN IS LEADING SEPARATE LINE 07 COL 40
               PIC S9(9) FROM SALDO-USER-ENT.
           05 FILLER LINE 07 COL 50 VALUE IS ".".
           05 FILLER LINE 07 COL 51 PIC 99 FROM SALDO-USER-DEC.
           05 FILLER LINE 07 COL 54 VALUE IS "EUR".

       01 FILA-ESPECTACULO-PAR.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 02
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM ESP-NUM.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 06
               FOREGROUND-COLOR YELLOW VALUE "|".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 07
               FOREGROUND-COLOR YELLOW PIC 99 FROM ESP-DIA.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 09
               FOREGROUND-COLOR YELLOW VALUE "-".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 10
               FOREGROUND-COLOR YELLOW PIC 99 FROM ESP-MES.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 12
               FOREGROUND-COLOR YELLOW VALUE "-".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 13
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM ESP-ANO.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 18
               FOREGROUND-COLOR YELLOW PIC 99 FROM ESP-HOR.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 20
               FOREGROUND-COLOR YELLOW VALUE ":".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 21
               FOREGROUND-COLOR YELLOW PIC 99 FROM ESP-MIN.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 23
               FOREGROUND-COLOR YELLOW VALUE "|".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 24
               FOREGROUND-COLOR YELLOW PIC X(40) FROM ESP-DESCR.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 64
               FOREGROUND-COLOR YELLOW VALUE "|".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 65
               FOREGROUND-COLOR YELLOW PIC 9(7) FROM ESP-DISP.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 72
               FOREGROUND-COLOR YELLOW VALUE "|".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 73
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM ESP-PRECIO-ENT.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 77
               FOREGROUND-COLOR YELLOW VALUE ".".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 78
               FOREGROUND-COLOR YELLOW PIC 9(2) FROM ESP-PRECIO-DEC.

       01 FILA-ESPECTACULO-IMPAR.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 02
               PIC 9(4) FROM ESP-NUM.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 06
               VALUE "|".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 07
               PIC 99 FROM ESP-DIA.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 09
               VALUE "-".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 10
                PIC 99 FROM ESP-MES.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 12
               VALUE "-".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 13
               PIC 9(4) FROM ESP-ANO.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 18
               PIC 99 FROM ESP-HOR.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 20
               VALUE ":".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 21
               PIC 99 FROM ESP-MIN.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 23
               VALUE "|".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 24
               PIC X(80) FROM ESP-DESCR.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 64
               VALUE "|".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 65
               PIC 9(7) FROM ESP-DISP.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 72
               VALUE "|".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 73
               PIC 9(4) FROM ESP-PRECIO-ENT.
           05 FILLER LINE LINEA-ESP-ACTUAL COL 77
               VALUE ".".
           05 FILLER LINE LINEA-ESP-ACTUAL COL 78
               PIC 9(2) FROM ESP-PRECIO-DEC.

       01 ACCEPT-COMPRA-ENTRADAS.
           05 FILLER LINE 21 COL 20 VALUE
               "Comprar    entradas del espectaculo     ".
           05 FILLER LINE 22 COL 10
               VALUE "(presione [ENTER] para continuar".
           05 FILLER LINE 22 COL 43
               VALUE "con el proceso de compra)".
           05 FILLER BLANK WHEN ZERO UNDERLINE AUTO
               LINE 21 COL 28 PIC 99 USING USER-NUM-ENTRADAS.
           05 FILLER BLANK WHEN ZERO UNDERLINE
               LINE 21 COL 56 PIC 9(4) USING USER-NUM-ESPECT.

       01 FORM-ERR.
           05 FILLER LINE 23 COL 14 BACKGROUND-COLOR RED VALUE
               "Por favor, rellene los campos con valores correctos".

       01 SALDOPOSTERIOR.
           05 FILLER SIGN IS LEADING SEPARATE LINE 12 COL 47
               PIC S9(9) FROM SALDO-POST-ENT.


       PROCEDURE DIVISION USING TNUM.
       IMPRIMIR-CABECERA.

           *>FORZAMOS QUE CREE UN FICHERO POR SI NO EXISTE
           OPEN I-O F-MOVIMIENTOS CLOSE F-MOVIMIENTOS.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'

           INITIALIZE MOVIMIENTO-REG.
           INITIALIZE ESPECTACULO-REG.

           DISPLAY BLANK-SCREEN.
           DISPLAY "Cajero Automatico UnizarBank"
               AT LINE 2 COL 26
               WITH FOREGROUND-COLOR IS CYAN.


           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           DISPLAY DIA AT LINE 4 COL 32.
           DISPLAY "-" AT LINE 4 COL 34.
           DISPLAY MES AT LINE 4 COL 35.
           DISPLAY "-" AT LINE 4 COL 37.
           DISPLAY ANO AT LINE 4 COL 38.
           DISPLAY HORAS AT LINE 4 COL 44.
           DISPLAY ":" AT LINE 4 COL 46.
           DISPLAY MINUTOS AT LINE 4 COL 47.

           DISPLAY "Compra de entradas de espectaculos"
                AT LINE 6 COL 22.

       CONSULTA-SALDO.
           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 00
               GO TO PSYS-ERR.

           MOVE 0 TO LAST-USER-MOV-NUM.
           MOVE 0 TO LAST-MOV-NUM.


       LECTURA-MOV.
           READ F-MOVIMIENTOS NEXT RECORD AT END GO
               LAST-USER-MOV-FOUND.
               IF MOV-TARJETA = TNUM
                   IF LAST-USER-MOV-NUM < MOV-NUM
                       MOVE MOV-NUM TO LAST-USER-MOV-NUM.
               IF LAST-MOV-NUM < MOV-NUM
                   MOVE MOV-NUM TO LAST-MOV-NUM.

              GO LECTURA-MOV.

       LAST-USER-MOV-FOUND.
           IF LAST-USER-MOV-NUM = 0 THEN
               MOVE 0 TO SALDO-USER-ENT
               MOVE 0 TO SALDO-USER-DEC
           ELSE
               MOVE LAST-USER-MOV-NUM TO MOV-NUM
               PERFORM READ-MOVIMIENTO THRU READ-MOVIMIENTO
               MOVE MOV-SALDOPOS-ENT TO SALDO-USER-ENT
               MOVE MOV-SALDOPOS-DEC TO SALDO-USER-DEC
           END-IF.


       PLECTURA-ESP.
           DISPLAY "NUM"  AT LINE 9 COL 2.
           DISPLAY "|"  AT LINE 9 COL 6.
           DISPLAY "FECHA"  AT LINE 9 COL 13.
           DISPLAY "|"  AT LINE 9 COL 23.
           DISPLAY "NOMBRE Y DESCRIPCION"  AT LINE 9 COL 34.
           DISPLAY "|"  AT LINE 9 COL 64.
           DISPLAY "LIBRES"  AT LINE 9 COL 65.
           DISPLAY "|"  AT LINE 9 COL 72.
           DISPLAY "IMPORTE"  AT LINE 9 COL 73.

           DISPLAY "Re. pag - Esp. anteriores" AT LINE 24 COL 2.
           DISPLAY "ESC - Salir" AT LINE 24 COL 33.
           DISPLAY "Av. pag - Esp. posteriores" AT LINE 24 COL 54.

           MOVE 0 TO ESP-EN-PANTALLA.
           MOVE 9 TO LINEA-ESP-ACTUAL.

           OPEN I-O F-ESPECTACULOS.
           IF FSE <> 00
               GO TO PSYS-ERR.


       LEER-PRIMEROS.
           READ F-ESPECTACULOS NEXT RECORD AT END GO WAIT-ORDER.
               MOVE 1 TO ESP-VALIDO.

               PERFORM FILTRADO THRU FILTRADO.

               IF ESP-VALIDO = 1
                   ADD 1 TO LINEA-ESP-ACTUAL
                   ADD 1 TO ESP-EN-PANTALLA
                   MOVE ESP-NUM TO
                       REGISTROS-EN-PANTALLA(ESP-EN-PANTALLA)
                   MOVE 0 TO ESP-VALIDO
                   PERFORM MOSTRAR-ESPECTACULO
                       THRU MOSTRAR-ESPECTACULO.

               IF ESP-EN-PANTALLA = 10
                   GO TO WAIT-ORDER.

               GO TO LEER-PRIMEROS.

       WAIT-ORDER.
           *> ESTABA EN LA POSICION 24LINE 80 COL ESTO DESPLAZABA TODO
           *> EL MENU A FUERA HACIENDO QUE SE SALIERA DE LA PANTALLA
           ACCEPT ACCEPT-COMPRA-ENTRADAS ON EXCEPTION

              IF ESC-PRESSED THEN
                  CLOSE F-MOVIMIENTOS
                  CLOSE F-ESPECTACULOS
                  EXIT PROGRAM
              END-IF

              IF PGDN-PRESSED THEN
                  GO TO FLECHA-ABAJO
              END-IF

              IF PGUP-PRESSED THEN
                  GO TO FLECHA-ARRIBA
              END-IF

           END-ACCEPT.

           IF USER-NUM-ENTRADAS = 0
               DISPLAY FORM-ERR
               GO TO WAIT-ORDER.

           IF USER-NUM-ESPECT = 0
               DISPLAY FORM-ERR
               GO TO WAIT-ORDER.

       CALCULO-SUFICIENCIA.
           MOVE USER-NUM-ESPECT TO ESP-NUM.
           READ F-ESPECTACULOS INVALID KEY
               DISPLAY FORM-ERR
               GO TO WAIT-ORDER.

           COMPUTE SALDO-USER-CENT = (SALDO-USER-ENT * 100)
                                     + SALDO-USER-DEC.

           COMPUTE IMPORTE-ENTRADAS-CENT = ((ESP-PRECIO-ENT * 100)
                                            + ESP-PRECIO-DEC)
                                            * USER-NUM-ENTRADAS.

           IF SALDO-USER-CENT < IMPORTE-ENTRADAS-CENT
               GO TO SALDO-INSUFICIENTE.
           IF ESP-DISP < USER-NUM-ENTRADAS
               GO TO NO-ENTR-DISP.

           COMPUTE IMPORTE-ENTRADAS-ENT =
               (IMPORTE-ENTRADAS-CENT / 100).
           MOVE FUNCTION MOD(IMPORTE-ENTRADAS-CENT, 100)
               TO IMPORTE-ENTRADAS-DEC.

       SALDO-SUFICIENTE.
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
           *> SE HA MODIFICADO PARA MOSTRARLO DE FORMA LEGIBLE
           DISPLAY "Compra de entradas de espectaculos" 
               AT LINE 6 COL 22.
           DISPLAY "Vas a comprar    entradas"
                AT LINE 8 COL 15.
           DISPLAY USER-NUM-ENTRADAS
            AT LINE 8 COL 29.
           DISPLAY "del espectaculo:"  AT LINE 8 COL 41.
           DISPLAY ESP-DESCR  AT LINE 9 COL 15.
           DISPLAY "con fecha"  AT LINE 11 COL 15.
           DISPLAY ESP-DIA  AT LINE 11 COL 25.
           DISPLAY "-"  AT LINE 11 COL 27.
           DISPLAY ESP-MES  AT LINE 11 COL 28.
           DISPLAY "-"  AT LINE 11 COL 30.
           DISPLAY ESP-ANO  AT LINE 11 COL 31.
           DISPLAY "a las"  AT LINE 11 COL 36.
           DISPLAY ESP-HOR  AT LINE 11 COL 42.
           DISPLAY ":"  AT LINE 11 COL 43.
           DISPLAY ESP-MIN  AT LINE 11 COL 44.
           DISPLAY "Codigo del espectaculo: "  AT LINE 12 COL 15.
           DISPLAY ESP-NUM  AT LINE 12 COL 39.

           DISPLAY "Importe total: "  AT LINE 14 COL 15.
           DISPLAY IMPORTE-ENTRADAS-ENT  AT LINE 14 COL 30.
           DISPLAY "."  AT LINE 14 COL 36.
           DISPLAY IMPORTE-ENTRADAS-DEC  AT LINE 14 COL 37.
           DISPLAY "EUR"  AT LINE 14 COL 40.

           DISPLAY "Enter - Confirmar"  AT LINE 24 COL 2.
           *>ESTABA EN EL 66, LA R DE CANCELAR NO SE VEIA
           DISPLAY "ESC - Cancelar"  AT LINE 24 COL 65.

       SALDO-SUF-ENTER.
       *> estaba en 24 y 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79 
           IF ENTER-PRESSED THEN
               GO TO GUARDAR-VENTA
           ELSE
               IF ESC-PRESSED THEN
                   CLOSE F-MOVIMIENTOS
                   CLOSE F-ESPECTACULOS
                   EXIT PROGRAM
               ELSE
                   GO TO SALDO-SUF-ENTER
               END-IF
           END-IF.

       GUARDAR-VENTA.
           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.
           ADD 1 TO LAST-MOV-NUM.
           SUBTRACT IMPORTE-ENTRADAS-CENT FROM SALDO-USER-CENT.
           COMPUTE SALDO-POST-ENT = (SALDO-USER-CENT / 100).
           MOVE FUNCTION MOD(SALDO-USER-CENT, 100) TO SALDO-POST-DEC.
           MULTIPLY -1 BY IMPORTE-ENTRADAS-ENT.

           MOVE FUNCTION
               CONCATENATE ("Compra entradas UnizarBank cod. ",
               ESP-NUM) TO ENTRAD-CONCEPTO.

           MOVE LAST-MOV-NUM         TO MOV-NUM.
           MOVE TNUM                 TO MOV-TARJETA.
           MOVE ANO                  TO MOV-ANO.
           MOVE MES                  TO MOV-MES.
           MOVE DIA                  TO MOV-DIA.
           MOVE HORAS                TO MOV-HOR.
           MOVE MINUTOS              TO MOV-MIN.
           MOVE SEGUNDOS             TO MOV-SEG.
           MOVE IMPORTE-ENTRADAS-ENT TO MOV-IMPORTE-ENT.
           MOVE IMPORTE-ENTRADAS-DEC TO MOV-IMPORTE-DEC.
           MOVE ESP-DESCR            TO MOV-CONCEPTO.
           MOVE SALDO-POST-ENT       TO MOV-SALDOPOS-ENT.
           MOVE SALDO-POST-DEC       TO MOV-SALDOPOS-DEC.

           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.

           SUBTRACT USER-NUM-ENTRADAS FROM ESP-DISP.
           REWRITE ESPECTACULO-REG INVALID KEY GO TO PSYS-ERR.

           CLOSE F-MOVIMIENTOS.
           CLOSE F-ESPECTACULOS.

       PANTALLA-RECOGIDA.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Compra de entradas de espectaculos" AT LINE 6 COL 22.
           DISPLAY "Por favor, retire las entradas" AT LINE 9 COL 25.
           DISPLAY "El saldo resultante es de" AT LINE 12 COL 21.
           DISPLAY SALDOPOSTERIOR.
           DISPLAY "." AT LINE 12 COL 57.
           DISPLAY SALDO-POST-DEC AT LINE 12 COL 58.

           DISPLAY "Enter - Aceptar" AT LINE 24 COL 33.

       RECOGER-ENTER.
       *> 24 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79 
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO RECOGER-ENTER.

       NO-ENTR-DISP.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY  "Compra de entradas de espectaculos"
               AT LINE 06 COL 22.
           DISPLAY  "Lamentamos comunicarle que"
               AT LINE 08 COL 27
               WITH BACKGROUND-COLOR RED.
           DISPLAY  "El espectaculo seleccionado no"
               AT LINE 9 COL 9
               WITH BACKGROUND-COLOR RED.
           DISPLAY  "dispone de suficientes entradas"
               AT LINE 9 COL 40
               WITH BACKGROUND-COLOR RED.
           DISPLAY "Enter - Aceptar" AT LINE 24 COL 33.

           GO TO VENTA-ERR-ENTER.

       SALDO-INSUFICIENTE.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Compra de entradas de espectaculos"
                AT LINE 6 COL 22.
           DISPLAY "Lamentamos comunicarle que"
                AT LINE 8 COL 27
               WITH BACKGROUND-COLOR RED.
           DISPLAY "su saldo es insuficiente"
                AT LINE 9 COL 28
               WITH BACKGROUND-COLOR RED.
           DISPLAY "Enter - Aceptar" AT LINE 24 COL 33.

       VENTA-ERR-ENTER.
       *> 24 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79 
           IF ENTER-PRESSED
               CLOSE F-MOVIMIENTOS
               CLOSE F-ESPECTACULOS
               GO TO IMPRIMIR-CABECERA
           ELSE
               GO TO VENTA-ERR-ENTER.

       FLECHA-ABAJO.
           MOVE REGISTROS-EN-PANTALLA(ESP-EN-PANTALLA) TO ESP-NUM.
           READ F-ESPECTACULOS INVALID KEY GO WAIT-ORDER.
           GO TO LEER-POSTERIORES.

       FLECHA-ARRIBA.
           MOVE REGISTROS-EN-PANTALLA(1) TO ESP-NUM.
           READ F-ESPECTACULOS INVALID KEY GO WAIT-ORDER.
           GO TO LEER-ANTERIORES.

       LEER-POSTERIORES.
           READ F-ESPECTACULOS NEXT RECORD
               AT END GO WAIT-ORDER.

               MOVE 1 TO ESP-VALIDO.
               PERFORM FILTRADO THRU FILTRADO.

               IF ESP-VALIDO = 1
                   MOVE 2 TO ESP-VALIDO
                   GO TO CONTROL-PANTALLA
               ELSE
                   GO TO LEER-POSTERIORES.

       LEER-ANTERIORES.
           READ F-ESPECTACULOS PREVIOUS RECORD
               AT END GO WAIT-ORDER.

               MOVE 1 TO ESP-VALIDO.
               PERFORM FILTRADO THRU FILTRADO.

               IF ESP-VALIDO = 1
                   MOVE 3 TO ESP-VALIDO
                   GO TO CONTROL-PANTALLA
               ELSE
                   GO TO LEER-ANTERIORES.

       CONTROL-PANTALLA.
           IF ESP-VALIDO = 2 THEN
               MOVE 0 TO ESP-VALIDO
               PERFORM REORDENAR-1 THRU REORDENAR-1
               GO TO WAIT-ORDER
           ELSE
               IF ESP-VALIDO = 3 THEN
                   MOVE 0 TO ESP-VALIDO
                   PERFORM REORDENAR-2 THRU REORDENAR-2
                   GO TO WAIT-ORDER
               ELSE
                   GO TO WAIT-ORDER
               END-IF
           END-IF.

       REORDENAR-1.
           MOVE 2 TO CONTADOR.
           MOVE ESP-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.

           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-ESP
               SUBTRACT 1 FROM CONTADOR
               MOVE COPIA-ESP TO REGISTROS-EN-PANTALLA(CONTADOR)
               ADD 2 TO CONTADOR
           END-PERFORM.

           MOVE ESP-NUM TO REGISTROS-EN-PANTALLA(ESP-EN-PANTALLA).
           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       REORDENAR-2.
           MOVE ESP-EN-PANTALLA TO CONTADOR.
           SUBTRACT 1 FROM CONTADOR.
           MOVE ESP-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.


           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-ESP
               ADD 1 TO CONTADOR
               MOVE COPIA-ESP TO REGISTROS-EN-PANTALLA(CONTADOR)
               SUBTRACT 2 FROM CONTADOR
           END-PERFORM.

           MOVE ESP-NUM TO REGISTROS-EN-PANTALLA(1).

           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       MOSTRAR-TABLA.
           MOVE 10 TO LINEA-ESP-ACTUAL.
           MOVE 1 TO CONTADOR.

           PERFORM ESP-EN-PANTALLA TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO ESP-NUM
               PERFORM READ-ESPECTACULO THRU READ-ESPECTACULO
               PERFORM MOSTRAR-ESPECTACULO THRU MOSTRAR-ESPECTACULO
               ADD 1 TO LINEA-ESP-ACTUAL
               ADD 1 TO CONTADOR
           END-PERFORM.

       READ-ESPECTACULO.
           READ F-ESPECTACULOS INVALID KEY GO TO PSYS-ERR.

       PSYS-ERR.
           CLOSE F-MOVIMIENTOS.
           CLOSE F-ESPECTACULOS.

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
        *> ESTABA EN 24 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79 
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.


       READ-MOVIMIENTO.
           READ F-MOVIMIENTOS INVALID KEY GO TO PSYS-ERR.


       MOSTRAR-ESPECTACULO.

           MOVE FUNCTION MOD(LINEA-ESP-ACTUAL, 2)
               TO MODULO-LIN-ACTUAL.

           IF MODULO-LIN-ACTUAL = 0 THEN
               DISPLAY FILA-ESPECTACULO-PAR
           ELSE
               DISPLAY FILA-ESPECTACULO-IMPAR
           END-IF.


       FILTRADO.

           COMPUTE FECHA-ESP-FILTRO = (ESP-ANO * 10000)
                                      + (ESP-MES * 100)
                                      + ESP-DIA.

           COMPUTE FECHA-ACTUAL = (ANO * 10000)
                                  + (MES * 100)
                                  + DIA.

           IF FECHA-ACTUAL > FECHA-ESP-FILTRO
               MOVE 0 TO ESP-VALIDO.
