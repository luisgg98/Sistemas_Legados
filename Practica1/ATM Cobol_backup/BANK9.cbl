       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK9.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL TRANSFERENCIAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TRANS-NUM
           FILE STATUS IS FSTR.

           SELECT OPTIONAL F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.


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
       77 FSTR                     PIC   X(2).
       77 FSM                       PIC   X(2).

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
       77 PRESSED-KEY               PIC   9(4).

       77 DIA1-USUARIO              PIC   9(2).
       77 MES1-USUARIO              PIC   9(2).
       77 ANO1-USUARIO              PIC   9(4).
       77 DIA2-USUARIO              PIC   9(2).
       77 MES2-USUARIO              PIC   9(2).
       77 ANO2-USUARIO              PIC   9(4).

       77 EURENT1-USUARIO           PIC  S9(7).
       77 EURDEC1-USUARIO           PIC   9(2).
       77 EURENT2-USUARIO           PIC  S9(7).
       77 EURDEC2-USUARIO           PIC   9(2).

       77 FECHA-MIN                 PIC   9(8).
       77 FECHA-MOV                 PIC   9(8).
       77 FECHA-TRANS               PIC   9(8).
       77 FECHA-MAX                 PIC   9(8).
       77 CENT-MIN                  PIC  S9(9).
       77 CENT-MOV                  PIC  S9(9).
       77 CENT-MAX                  PIC  S9(9).

       77 MOV-EN-PANTALLA           PIC   9(2).
       77 LINEA-MOV-ACTUAL          PIC   9(2).
       77 MOV-VALIDO                PIC   9(1).
       77 MODULO-LIN-ACTUAL         PIC   9(1).

       01 TABLA.
           05 REGISTROS-EN-PANTALLA PIC  9(35) OCCURS 15 TIMES.

       77 CONTADOR                  PIC   9(2).
       77 ITERACIONES               PIC   9(2).
       77 COPIA-MOV                 PIC  9(35).

       77 TIPO                      PIC 9(1).
       77 TIPO-ANTIGUA              PIC 9(1) VALUE 1.
       77 TIPO-PROGRAMADA           PIC 9(1) VALUE 2.

      

       77 MSJ-ORD                   PIC  X(35) VALUE "Transferimos".
       77 MSJ-DST                   PIC  X(35) VALUE "Nos transfieren".
       77 MSJ-MENSUAL-PERIOD        PIC  X(35) VALUE "Mensual".
       77 MSJ-PUNTUAL-PERIOD        PIC  X(35) VALUE "Puntual". 

       77 CHOICE   BLANK WHEN ZERO  PIC  9(1).

       LINKAGE SECTION.
       77 TNUM                      PIC  9(16).


       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       *> Pantalla con campos para filtrar mov por fecha y cantidad
       01 FILTRO-MOVIMIENTOS.
           05 DIA-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 37 PIC 9(2) USING DIA1-USUARIO.
           05 MES-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 40 PIC 9(2) USING MES1-USUARIO.
           05 ANO-MIN BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 43 PIC 9(4) USING ANO1-USUARIO.
           05 DIA-MAX BLANK ZERO BEEP AUTO UNDERLINE
               LINE 13 COL 50 PIC 9(2) USING DIA2-USUARIO.
           05 MES-MAX BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 53 PIC 9(2) USING MES2-USUARIO.
           05 ANO-MAX BLANK ZERO AUTO UNDERLINE
               LINE 13 COL 56 PIC 9(4) USING ANO2-USUARIO.

       01 FILA-MOVIMIENTO-PAR.
           *> Filas pares con letra de color amarillo
           05 MOV-DIA-PAR LINE LINEA-MOV-ACTUAL COL 02
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-DIA.
           05 SEPARADOR-PAR-1 LINE LINEA-MOV-ACTUAL COL 04
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 MOV-MES-PAR LINE LINEA-MOV-ACTUAL COL 05
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-MES.
           05 SEPARADOR-PAR-2 LINE LINEA-MOV-ACTUAL COL 07
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 MOV-ANO-PAR LINE LINEA-MOV-ACTUAL COL 08
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM MOV-ANO.
           05 SEPARADOR-PAR-4 LINE LINEA-MOV-ACTUAL COL 12
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-CONCEPTO-PAR LINE LINEA-MOV-ACTUAL COL 13
               FOREGROUND-COLOR YELLOW PIC X(35) FROM MOV-CONCEPTO.
           05 SEPARADOR-5-PAR LINE LINEA-MOV-ACTUAL COL 51
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-IMPORTE-ENT-PAR SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 52
               FOREGROUND-COLOR YELLOW PIC S9(7) FROM MOV-IMPORTE-ENT.
           05 SEPARADOR-6-PAR LINE LINEA-MOV-ACTUAL COL 60
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 MOV-IMPORTE-DEC-PAR LINE LINEA-MOV-ACTUAL COL 61
               FOREGROUND-COLOR YELLOW PIC 99 FROM MOV-IMPORTE-DEC.
           05 SEPARADOR-7-PAR LINE LINEA-MOV-ACTUAL COL 63
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 MOV-TARJETA-PAR LINE LINEA-MOV-ACTUAL COL 64
               FOREGROUND-COLOR YELLOW PIC 9(16)
               FROM MOV-TARJETA.

       01 FILA-MOVIMIENTO-IMPAR.
           05 MOV-DIA-IMPAR LINE LINEA-MOV-ACTUAL COL 02
               PIC 99 FROM MOV-DIA.
           05 SEPARADOR-IMPAR-1 LINE LINEA-MOV-ACTUAL COL 04
               PIC A FROM "-".
           05 MOV-MES-IMPAR LINE LINEA-MOV-ACTUAL COL 05
               PIC 99 FROM MOV-MES.
           05 SEPARADOR-IMPAR-2 LINE LINEA-MOV-ACTUAL COL 07
               PIC A FROM "-".
           05 MOV-ANO-IMPAR LINE LINEA-MOV-ACTUAL COL 08
               PIC 9(4) FROM MOV-ANO.
           05 SEPARADOR-IMPAR-4 LINE LINEA-MOV-ACTUAL COL 12
               PIC A FROM "|".
           05 MOV-CONCEPTO-IMPAR LINE LINEA-MOV-ACTUAL COL 13
               PIC X(35) FROM MOV-CONCEPTO.
           05 SEPARADOR-5-IMPAR LINE LINEA-MOV-ACTUAL COL 51
               PIC A FROM "|".
           05 MOV-IMPORTE-ENT-IMPAR
               SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 52
               PIC S9(7) FROM MOV-IMPORTE-ENT.
           05 SEPARADOR-6-IMPAR LINE LINEA-MOV-ACTUAL COL 60
               PIC A FROM ",".
           05 MOV-IMPORTE-DEC-IMPAR LINE LINEA-MOV-ACTUAL COL 61
               PIC 99 FROM MOV-IMPORTE-DEC.
           05 SEPARADOR-7-IMPAR LINE LINEA-MOV-ACTUAL COL 63
               PIC A FROM "|".
           05 MOV-TARJETA-IMPAR LINE LINEA-MOV-ACTUAL COL 64
               PIC  9(16) FROM MOV-TARJETA.

       01 FILA-TRANSFERENCIA-PAR.
           *> Filas pares con letra de color amarillo
           05 TRANS-DIA-PAR LINE LINEA-MOV-ACTUAL COL 02
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRANS-DIA.
           05 SEPARADOR-PAR-1 LINE LINEA-MOV-ACTUAL COL 04
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 TRANS-MES-PAR LINE LINEA-MOV-ACTUAL COL 05
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRANS-MES.
           05 SEPARADOR-PAR-2 LINE LINEA-MOV-ACTUAL COL 07
               FOREGROUND-COLOR YELLOW PIC A FROM "-".
           05 TRANS-ANO-PAR LINE LINEA-MOV-ACTUAL COL 08
               FOREGROUND-COLOR YELLOW PIC 9(4) FROM TRANS-ANO.
           05 SEPARADOR-PAR-4 LINE LINEA-MOV-ACTUAL COL 12
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 TRANS-PERIODO-PAR LINE LINEA-MOV-ACTUAL COL 13
               FOREGROUND-COLOR YELLOW PIC X(35) FROM TRANS-PERIODO.
           05 SEPARADOR-5-PAR LINE LINEA-MOV-ACTUAL COL 51
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 TRANS-IMPORTE-ENT-PAR SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 52
               FOREGROUND-COLOR YELLOW PIC S9(7) FROM TRANS-IMPORTE-ENT.
           05 SEPARADOR-6-PAR LINE LINEA-MOV-ACTUAL COL 60
               FOREGROUND-COLOR YELLOW PIC A FROM ",".
           05 TRANS-IMPORTE-DEC-PAR LINE LINEA-MOV-ACTUAL COL 61
               FOREGROUND-COLOR YELLOW PIC 99 FROM TRANS-IMPORTE-DEC.
           05 SEPARADOR-7-PAR LINE LINEA-MOV-ACTUAL COL 63
               FOREGROUND-COLOR YELLOW PIC A FROM "|".
           05 TRANS-TARJETA-ORD-PAR LINE LINEA-MOV-ACTUAL COL 64
               FOREGROUND-COLOR YELLOW PIC 9(16)
               FROM TRANS-TARJETA-DST.

       01 FILA-TRANSFERENCIA-IMPAR.
           05 TRANS-DIA-IMPAR LINE LINEA-MOV-ACTUAL COL 02
               PIC 99 FROM TRANS-DIA.
           05 SEPARADOR-IMPAR-1 LINE LINEA-MOV-ACTUAL COL 04
               PIC A FROM "-".
           05 TRANS-MES-IMPAR LINE LINEA-MOV-ACTUAL COL 05
               PIC 99 FROM TRANS-MES.
           05 SEPARADOR-IMPAR-2 LINE LINEA-MOV-ACTUAL COL 07
               PIC A FROM "-".
           05 TRANS-ANO-IMPAR LINE LINEA-MOV-ACTUAL COL 08
               PIC 9(4) FROM TRANS-ANO.
           05 SEPARADOR-IMPAR-4 LINE LINEA-MOV-ACTUAL COL 12
               PIC A FROM "|".
           05 TRANS-CONCEPTO-IMPAR LINE LINEA-MOV-ACTUAL COL 13
               PIC X(35) FROM TRANS-PERIODO.
           05 SEPARADOR-5-IMPAR LINE LINEA-MOV-ACTUAL COL 51
               PIC A FROM "|".
           05 TRANS-IMPORTE-ENT-IMPAR
               SIGN IS LEADING SEPARATE
               LINE LINEA-MOV-ACTUAL COL 52
               PIC S9(7) FROM TRANS-IMPORTE-ENT.
           05 SEPARADOR-6-IMPAR LINE LINEA-MOV-ACTUAL COL 60
               PIC A FROM ",".
           05 TRANS-IMPORTE-DEC-IMPAR LINE LINEA-MOV-ACTUAL COL 61
               PIC 99 FROM TRANS-IMPORTE-DEC.
           05 SEPARADOR-7-IMPAR LINE LINEA-MOV-ACTUAL COL 63
               PIC A FROM "|".
           05 TRANS-TARJETA-IMPAR LINE LINEA-MOV-ACTUAL COL 64
               PIC  9(16) FROM TRANS-TARJETA-DST.





       PROCEDURE DIVISION USING TNUM.
       IMPRIMIR-CABECERA.

           *>FORZAMOS QUE CREE UN FICHERO POR SI NO EXISTE
           OPEN I-O F-MOVIMIENTOS CLOSE F-MOVIMIENTOS.
           
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'

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


       PELECCION-TIPO.

           INITIALIZE CHOICE.
           INITIALIZE TIPO.

           DISPLAY "Elija el tipo de transferencia que desea ver:"
               AT LINE 8 COL 15.
           DISPLAY "1 - Transferencias anteriores" AT LINE 12 COL 22.
           DISPLAY "2 - Transferencias programadas" AT LINE 14 COL 22.
           
           DISPLAY "Enter - Aceptar" AT LINE 24 COL 01.
           DISPLAY "ESC - Cancelar" AT LINE 24 COL 65.

           ACCEPT CHOICE AT LINE 24 COL 80 ON EXCEPTION
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO IMPRIMIR-CABECERA.

           IF (CHOICE = TIPO-ANTIGUA) OR (CHOICE = TIPO-PROGRAMADA)
               GO TO PCONSULTA-MOV.

           GO TO IMPRIMIR-CABECERA.

       PCONSULTA-MOV.
           
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.

           INITIALIZE DIA1-USUARIO.
           INITIALIZE MES1-USUARIO.
           INITIALIZE ANO1-USUARIO.
           INITIALIZE DIA2-USUARIO.
           INITIALIZE MES2-USUARIO.
           INITIALIZE ANO2-USUARIO.

           DISPLAY "Se mostraran las ultimas transferencias,"
                AT LINE 8 COL 8.
           DISPLAY "de mas a menos recientes."
                AT LINE 8 COL 47.

           DISPLAY"Alternativamente, indique un intervalo"
               AT LINE 10 COL 8.
           DISPLAY "de fechas"
               AT LINE 10 COL 47.

           DISPLAY "Entre las fechas   /  /     y   /  /    "
               AT LINE 13 COL 20.


           DISPLAY "Enter - Aceptar" AT LINE 24 COL 01.
           DISPLAY "ESC - Cancelar" AT LINE 24 COL 65.
           
           *> Recoger datos de filtro de movimientos
           ACCEPT FILTRO-MOVIMIENTOS ON EXCEPTION
               IF ESC-PRESSED
                   EXIT PROGRAM
               ELSE
                   GO TO PCONSULTA-MOV.
           
           *> Si los campos no se han llenado se pone el maximo para 
           *> mostrar todos los movimientos
           IF DIA2-USUARIO = 0
               IF MES2-USUARIO = 0
                   IF ANO2-USUARIO = 0
                       MOVE 99   TO DIA2-USUARIO
                       MOVE 99   TO MES2-USUARIO
                       MOVE 9999 TO ANO2-USUARIO.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.

           OPEN INPUT F-MOVIMIENTOS.
               IF FSM <> 00
                   GO TO PSYS-ERR.
           
           OPEN INPUT TRANSFERENCIAS.
               IF FSTR <> 00
                   GO TO PSYS-ERR.

       POSICIONAR-FINAL.
           *> Se empieza a leer los movimientos desde el final
           IF (CHOICE = TIPO-ANTIGUA) THEN
               READ F-MOVIMIENTOS NEXT RECORD AT END GO PLECTURA-MOV
           END-IF.
           IF (CHOICE = TIPO-PROGRAMADA) THEN
               READ TRANSFERENCIAS NEXT RECORD AT END GO PLECTURA-MOV
           END-IF.    
               
           GO TO POSICIONAR-FINAL.

       PLECTURA-MOV.
           DISPLAY "FECHA" AT LINE 7 COL 4.
           DISPLAY "|" AT LINE 7 COL 12.
           DISPLAY "CONCEPTO" AT LINE 7 COL 27.
           DISPLAY "|" AT LINE 7 COL 51.
           DISPLAY "IMPORTE" AT LINE 7 COL 54.
           DISPLAY "|" AT LINE 7 COL 63.
           DISPLAY "CUENTA DESTINO" AT LINE 7 COL 65.

           DISPLAY "Re. pag - Esp. anteriores" AT LINE 24 COL 2.
           DISPLAY "ESC - Salir" AT LINE 24 COL 33.
           *> SCREEN FIXED
           DISPLAY "Av. pag - Esp. posteriores" AT LINE 24 COL 50.

           MOVE 0 TO MOV-EN-PANTALLA.
           MOVE 7 TO LINEA-MOV-ACTUAL.


       LEER-PRIMEROS.
           *> Se lee desde el ultimo al primero
           IF (CHOICE = TIPO-ANTIGUA) THEN
               READ F-MOVIMIENTOS PREVIOUS RECORD AT END GO WAIT-ORDER
           END-IF.

           IF (CHOICE = TIPO-PROGRAMADA) THEN
               READ TRANSFERENCIAS PREVIOUS RECORD AT END GO 
               WAIT-ORDER
           END-IF.

           MOVE 1 TO MOV-VALIDO.

               PERFORM FILTRADO THRU FILTRADO.

               *> Se ha leido un movimiento valido y 
               *> se pasa al siguiente
               IF MOV-VALIDO = 1
                   ADD 1 TO LINEA-MOV-ACTUAL
                   ADD 1 TO MOV-EN-PANTALLA
                   *> Guardo el mov-num en la tabla y lo muestro
                   MOVE MOV-NUM TO
                       REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA)
                   MOVE 0 TO MOV-VALIDO
                   PERFORM MOSTRAR-MOVIMIENTO THRU MOSTRAR-MOVIMIENTO.

               *> Se muestra solo una cantidad por pantalla y se espera
               IF MOV-EN-PANTALLA = 15
                   GO TO WAIT-ORDER.

               GO TO LEER-PRIMEROS.

       WAIT-ORDER.
           *> 24 80
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 79  ON EXCEPTION

              IF ESC-PRESSED THEN
                  CLOSE F-MOVIMIENTOS
                  CLOSE TRANSFERENCIAS
                  EXIT PROGRAM
              END-IF

              IF PGDN-PRESSED THEN
                  GO TO FLECHA-ABAJO
              END-IF

              IF PGUP-PRESSED THEN
                  GO TO FLECHA-ARRIBA
              END-IF

           END-ACCEPT.

           GO TO WAIT-ORDER.

       FLECHA-ABAJO.
           *> Se parte del ultimo guardado para seguir hacia atras
           MOVE REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA) TO MOV-NUM.
           IF (CHOICE = TIPO-ANTIGUA) THEN
               READ F-MOVIMIENTOS INVALID KEY GO WAIT-ORDER
           END-IF.
           IF (CHOICE = TIPO-PROGRAMADA) THEN
               READ TRANSFERENCIAS INVALID KEY GO WAIT-ORDER
           END-IF.
           
           GO TO LEER-VIEJO.

       FLECHA-ARRIBA.
           MOVE REGISTROS-EN-PANTALLA(1) TO MOV-NUM.
           IF (CHOICE = TIPO-ANTIGUA) THEN
               READ F-MOVIMIENTOS INVALID KEY GO WAIT-ORDER
           END-IF.
           IF (CHOICE = TIPO-PROGRAMADA) THEN
               READ TRANSFERENCIAS INVALID KEY GO WAIT-ORDER
           END-IF.
           
           GO TO LEER-NUEVO.

       LEER-VIEJO.
           IF (CHOICE = TIPO-ANTIGUA) THEN
               READ F-MOVIMIENTOS PREVIOUS RECORD
               AT END GO WAIT-ORDER
           END-IF.
           IF (CHOICE = TIPO-PROGRAMADA) THEN
               READ TRANSFERENCIAS PREVIOUS RECORD
               AT END GO WAIT-ORDER
           END-IF.

           MOVE 1 TO MOV-VALIDO.
           PERFORM FILTRADO THRU FILTRADO.

           IF MOV-VALIDO = 1
               MOVE 2 TO MOV-VALIDO
               GO TO CONTROL-PANTALLA
           ELSE
               GO TO LEER-VIEJO.

       LEER-NUEVO.

           IF (CHOICE = TIPO-ANTIGUA) THEN
               READ F-MOVIMIENTOS NEXT RECORD
               AT END GO WAIT-ORDER
           END-IF.
           IF (CHOICE = TIPO-PROGRAMADA) THEN
               READ TRANSFERENCIAS NEXT RECORD
               AT END GO WAIT-ORDER
           END-IF.

           MOVE 1 TO MOV-VALIDO.
           PERFORM FILTRADO THRU FILTRADO.

           IF MOV-VALIDO = 1
               MOVE 3 TO MOV-VALIDO
               GO TO CONTROL-PANTALLA
           ELSE
               GO TO LEER-NUEVO.

       CONTROL-PANTALLA.
           IF MOV-VALIDO = 2 THEN
               MOVE 0 TO MOV-VALIDO
               PERFORM REORDENAR-1 THRU REORDENAR-1
               GO TO WAIT-ORDER
           ELSE
               IF MOV-VALIDO = 3 THEN
                   MOVE 0 TO MOV-VALIDO
                   PERFORM REORDENAR-2 THRU REORDENAR-2
                   GO TO WAIT-ORDER
               ELSE
                   GO TO WAIT-ORDER
               END-IF
           END-IF.

       REORDENAR-1.
           MOVE 2 TO CONTADOR.
           MOVE MOV-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.

           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-MOV
               SUBTRACT 1 FROM CONTADOR
               MOVE COPIA-MOV TO REGISTROS-EN-PANTALLA(CONTADOR)
               ADD 2 TO CONTADOR
           END-PERFORM.

           MOVE MOV-NUM TO REGISTROS-EN-PANTALLA(MOV-EN-PANTALLA).
           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       REORDENAR-2.
           MOVE MOV-EN-PANTALLA TO CONTADOR.
           SUBTRACT 1 FROM CONTADOR.
           MOVE MOV-EN-PANTALLA TO ITERACIONES.
           SUBTRACT 1 FROM ITERACIONES.


           PERFORM ITERACIONES TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO COPIA-MOV
               ADD 1 TO CONTADOR
               MOVE COPIA-MOV TO REGISTROS-EN-PANTALLA(CONTADOR)
               SUBTRACT 2 FROM CONTADOR
           END-PERFORM.

           MOVE MOV-NUM TO REGISTROS-EN-PANTALLA(1).

           PERFORM MOSTRAR-TABLA THRU MOSTRAR-TABLA.

           GO TO WAIT-ORDER.

       MOSTRAR-TABLA.
           MOVE 8 TO LINEA-MOV-ACTUAL.
           MOVE 1 TO CONTADOR.

           PERFORM MOV-EN-PANTALLA TIMES
               MOVE REGISTROS-EN-PANTALLA(CONTADOR) TO MOV-NUM
               PERFORM READ-MOVIMIENTO THRU READ-MOVIMIENTO
               PERFORM MOSTRAR-MOVIMIENTO THRU MOSTRAR-MOVIMIENTO
               ADD 1 TO LINEA-MOV-ACTUAL
               ADD 1 TO CONTADOR
           END-PERFORM.

       READ-MOVIMIENTO.
           IF (CHOICE = TIPO-ANTIGUA) THEN
               READ F-MOVIMIENTOS INVALID KEY GO TO PSYS-ERR
           END-IF.

           IF (CHOICE = TIPO-PROGRAMADA) THEN
               READ TRANSFERENCIAS INVALID KEY GO TO PSYS-ERR
           END-IF.

       PSYS-ERR.
           CLOSE F-MOVIMIENTOS.
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
           DISPLAY "Enter - Aceptar" AT LINE 24 COL 33.

       EXIT-ENTER.
           *> ESTABA EN 24 80
           ACCEPT PRESSED-KEY AT LINE 24 COL 79 
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.


       FILTRADO.
           
           *> Comprobar que el movimiento pertenece a su cuenta
           IF (CHOICE = TIPO-ANTIGUA) THEN
               IF TNUM NOT = MOV-TARJETA
                   MOVE 0 TO MOV-VALIDO
               END-IF
               IF (MOV-CONCEPTO <> MSJ-DST) AND
                   (MOV-CONCEPTO <> MSJ-ORD)
                   MOVE 0 TO MOV-VALIDO
               END-IF
           END-IF.

           IF (CHOICE = TIPO-PROGRAMADA) THEN
               IF (TNUM NOT = TRANS-TARJETA-ORD) THEN
                   MOVE 0 TO MOV-VALIDO
               END-IF
           END-IF.

           *> Juntar la fecha y comprobar que entra en el rango de fecha
           COMPUTE FECHA-MIN = (ANO1-USUARIO * 10000)
                               + (MES1-USUARIO * 100)
                               + DIA1-USUARIO.

           COMPUTE FECHA-MOV = (MOV-ANO * 10000)
                               + (MOV-MES * 100)
                               + MOV-DIA.

           COMPUTE FECHA-TRANS = (TRANS-ANO * 10000)
                               + (TRANS-MES * 100)
                               + TRANS-DIA.

           COMPUTE FECHA-MAX = (ANO2-USUARIO * 10000)
                               + (MES2-USUARIO * 100)
                               + DIA2-USUARIO.
           IF (CHOICE = TIPO-ANTIGUA) THEN
               IF FECHA-MIN > FECHA-MOV THEN
                   MOVE 0 TO MOV-VALIDO
               END-IF
               IF FECHA-MAX < FECHA-MOV THEN
                   MOVE 0 TO MOV-VALIDO
               END-IF
           END-IF.

           IF (CHOICE = TIPO-PROGRAMADA) THEN
               IF FECHA-MIN > FECHA-TRANS THEN
                   MOVE 0 TO MOV-VALIDO
               END-IF
               IF FECHA-MAX < FECHA-TRANS THEN
                   MOVE 0 TO MOV-VALIDO
               END-IF
           END-IF.

       MOSTRAR-MOVIMIENTO.

           MOVE FUNCTION MOD(LINEA-MOV-ACTUAL, 2)
               TO MODULO-LIN-ACTUAL.

           IF MODULO-LIN-ACTUAL = 0 THEN
               IF (CHOICE = TIPO-ANTIGUA)  
                   DISPLAY FILA-MOVIMIENTO-PAR
               END-IF
               IF (CHOICE = TIPO-PROGRAMADA)
                   DISPLAY FILA-TRANSFERENCIA-PAR
               END-IF
           ELSE
               IF (CHOICE = TIPO-ANTIGUA)  
                   DISPLAY FILA-MOVIMIENTO-IMPAR
               END-IF
               IF (CHOICE = TIPO-PROGRAMADA)
                   DISPLAY FILA-TRANSFERENCIA-IMPAR
               END-IF
           END-IF.
