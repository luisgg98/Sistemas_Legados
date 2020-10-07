       *>Ejecutar transferencias pendientes
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK10. 

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

           SELECT OPTIONAL  F-MOVIMIENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MOV-NUM
           FILE STATUS IS FSM.
           
           SELECT TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM-E
           FILE STATUS IS FST.

       DATA DIVISION.
       FILE SECTION.

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


       FD TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TAJETAREG.
           02 TNUM-E      PIC 9(16).
           02 TPIN-E      PIC  9(4).


       WORKING-STORAGE SECTION.
       01 CHECKERR                 PIC   X(24).
       77 FSTR                     PIC   X(2).
       77 FSM                       PIC   X(2).
       77 FST                      PIC   X(2).

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

       77 TRANS-VALIDO                PIC   9(1).

       77 FECHA-TRANS               PIC   9(8).
       77 FECHA-ACTUAL              PIC   9(8).

       77 LAST-MOV-NUM             PIC  9(35).
       77 LAST-USER-ORD-MOV-NUM    PIC  9(35).
       77 LAST-USER-DST-MOV-NUM    PIC  9(35).

       77 TRAN-NUM                 PIC  9(35).
       77 EURENT-USUARIO           PIC  S9(7).
       77 EURDEC-USUARIO           PIC   9(2).
       77 CUENTA-DESTINO           PIC  9(16).
       77 CUENTA-ORIGEN            PIC  X(35).

       77 CENT-SALDO-ORD-USER      PIC  S9(9).
       77 CENT-SALDO-DST-USER      PIC  S9(9).
       77 CENT-IMPOR-USER          PIC  S9(9).

       77 MSJ-MENSUAL-PERIOD        PIC  X(35) VALUE "Mensual".
       77 MSJ-PUNTUAL-PERIOD        PIC  X(35) VALUE "Puntual". 

       77 MSJ-ORD                  PIC  X(35) VALUE "Transferimos".
       77 MSJ-DST                  PIC  X(35) VALUE "Nos transfieren".

       77 MSJ-PERIOD               PIC  X(35).

       PROCEDURE DIVISION.
       INICIO.
           INITIALIZE TRAN-NUM.
           INITIALIZE CUENTA-DESTINO.
           INITIALIZE CUENTA-ORIGEN.
           INITIALIZE EURENT-USUARIO.
           INITIALIZE EURDEC-USUARIO.
           INITIALIZE LAST-MOV-NUM.
           INITIALIZE LAST-USER-ORD-MOV-NUM.
           INITIALIZE LAST-USER-DST-MOV-NUM.
           INITIALIZE MSJ-PERIOD.

           *>FORZAMOS QUE CREE UN FICHERO POR SI NO EXISTE
           OPEN I-O F-MOVIMIENTOS CLOSE F-MOVIMIENTOS.


           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.
           

       MOVIMIENTOS-OPEN.
           *>FORZAMOS QUE CREE UN FICHERO POR SI NO EXISTE
           OPEN I-O F-MOVIMIENTOS CLOSE F-MOVIMIENTOS.

           OPEN I-O F-MOVIMIENTOS.
           IF FSM <> 00 THEN
               GO TO PSYS-ERR
           END-IF.
       
       TRANSFERENCIAS-OPEN.
           OPEN I-O TRANSFERENCIAS.
           IF FSTR <> 00 THEN
               GO TO PSYS-ERR
           END-IF.

       LEER-TRANSFERENCIAS.
           *>Leemos las transferencias
           READ TRANSFERENCIAS NEXT RECORD AT END GO TO FIN.


           *>Guardamos los valores de la transferencia leida
           MOVE TRANS-IMPORTE-ENT TO EURENT-USUARIO.
           MOVE TRANS-IMPORTE-DEC TO EURDEC-USUARIO.
           MOVE TRANS-TARJETA-ORD TO CUENTA-ORIGEN. 
           MOVE TRANS-NUM TO TRAN-NUM.
           MOVE TRANS-TARJETA-DST TO CUENTA-DESTINO.
           MOVE TRANS-PERIODO TO MSJ-PERIOD.
           

           *> Comprobamos que la fecha de la transferencia coincide
           *> con la fecha del dia que se ejecuta el script
           MOVE 1 TO TRANS-VALIDO.
           
           PERFORM FILTRADO THRU FILTRADO.
           
           *> Si la fecha coincide, se debe ejecutar
           *> sino, leemos otra transferencia
           IF TRANS-VALIDO = 1
               GO TO TRANSFERENCIA.
          
           GO TO LEER-TRANSFERENCIAS.


       TRANSFERENCIA.
           *>Calculamos el importe de la transferencia
           COMPUTE CENT-IMPOR-USER = (EURENT-USUARIO * 100)
                                     + EURDEC-USUARIO.           
           
           GO TO VERIFICACION-CTA-CORRECTA.
 

       FILTRADO.
           *> Juntar la fecha y comprobar que coincide con la fecha 
           *> actual 
           
           COMPUTE FECHA-TRANS = (TRANS-ANO * 10000)
                               + (TRANS-MES * 100)
                               + TRANS-DIA.

           COMPUTE FECHA-ACTUAL = (ANO * 10000)
                               + (MES * 100)
                               + DIA.

           IF (FECHA-TRANS <> FECHA-ACTUAL) THEN
              MOVE 0 TO TRANS-VALIDO.

     
       VERIFICACION-CTA-CORRECTA.
           OPEN I-O TARJETAS.
           IF FST <> 00
              GO TO PSYS-ERR.
           
           *> Comprobar tarjeta origen
           MOVE CUENTA-ORIGEN TO TNUM-E.
           READ TARJETAS INVALID KEY GO TO USER-BAD.
           CLOSE TARJETAS.
           
           *> Comprobar tarjeta destino
           MOVE CUENTA-DESTINO TO TNUM-E.
           READ TARJETAS INVALID KEY GO TO USER-BAD.
           CLOSE TARJETAS.
    
           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.

           MOVE 0 TO MOV-NUM.
           MOVE 0 TO LAST-USER-ORD-MOV-NUM.
           GO TO LECTURA-SALDO-ORD.


         
       LECTURA-SALDO-ORD.
           *>Leemos los movimientos de la cuenta ord, buscamos el ultimo
           MOVE CUENTA-ORIGEN TO MOV-TARJETA. 
           READ F-MOVIMIENTOS NEXT RECORD AT END GO TO ENTRE-LECTURAS.
           IF MOV-TARJETA = CUENTA-ORIGEN THEN
               IF LAST-USER-ORD-MOV-NUM < MOV-NUM THEN
                   *> Nos quedamos con el ultimo mov del usuario
                   MOVE MOV-NUM TO LAST-USER-ORD-MOV-NUM
               END-IF
           END-IF.
           IF LAST-MOV-NUM < MOV-NUM THEN
               MOVE MOV-NUM TO LAST-MOV-NUM
           END-IF.
           GO TO LECTURA-SALDO-ORD.
           


       ENTRE-LECTURAS.

           CLOSE F-MOVIMIENTOS.
           
           *>Leemos el ultimo movimiento de la cuenta ordenante
           MOVE LAST-USER-ORD-MOV-NUM TO MOV-NUM.
           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.
           *> Si la cuenta origen no tiene mov -> saldo=0
           READ F-MOVIMIENTOS INVALID KEY GO NO-MONEY.
           
           *> Obtenemos el saldo de la cuenta ordenante
           COMPUTE CENT-SALDO-ORD-USER = (MOV-SALDOPOS-ENT * 100)
                                     + MOV-SALDOPOS-DEC.
           
           CLOSE F-MOVIMIENTOS.
           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.
           
           MOVE 0 TO MOV-NUM.
           MOVE 0 TO LAST-USER-DST-MOV-NUM.
           MOVE CUENTA-DESTINO TO MOV-TARJETA.
           

       LECTURA-SALDO-DST.
           *>Leemos los movimientos de la cuenta ord, buscamos el ultimo
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

           *>FORZAMOS QUE CREE UN FICHERO POR SI NO EXISTE
           PERFORM MOVIMIENTOS-OPEN THRU MOVIMIENTOS-OPEN.

           *> Si la cuenta destino no tiene mov -> saldo=0
           READ F-MOVIMIENTOS INVALID KEY GO NO-MONEY.
           
       CALCULO-SALDO-DESTINO-USUARIO.   
           
         
           *> Calculamos el saldo de la cuenta destino en centimos
           COMPUTE CENT-SALDO-DST-USER = (MOV-SALDOPOS-ENT * 100)
                                     + MOV-SALDOPOS-DEC.
           
           MOVE FUNCTION CURRENT-DATE TO CAMPOS-FECHA.

           
           *> MOVIMIENTO DEL ORDENANTE
           ADD 1 TO LAST-MOV-NUM.

           *> Preparamos datos para guardar el mov de transferencia
           *> respecto a la cuenta del ordenante
           MOVE LAST-MOV-NUM               TO MOV-NUM.
           MOVE CUENTA-ORIGEN              TO MOV-TARJETA.
           MOVE ANO                        TO MOV-ANO.
           MOVE MES                        TO MOV-MES.
           MOVE DIA                        TO MOV-DIA.
           MOVE HORAS                      TO MOV-HOR.
           MOVE MINUTOS                    TO MOV-MIN.
           MOVE SEGUNDOS                   TO MOV-SEG.
    
           MULTIPLY -1 BY EURENT-USUARIO.
           MOVE EURENT-USUARIO TO MOV-IMPORTE-ENT.
           MULTIPLY -1 BY EURENT-USUARIO.
           MOVE EURDEC-USUARIO TO MOV-IMPORTE-DEC.
           
           MOVE MSJ-ORD       TO MOV-CONCEPTO.

           *> AL SALDO DEL USUARIO LE QUITAMOS EL DINERO QUE VA ENVIAR
           SUBTRACT CENT-IMPOR-USER FROM CENT-SALDO-ORD-USER.
         

           *> Calculamos euros y los euros restantes
           COMPUTE MOV-SALDOPOS-ENT = (CENT-SALDO-ORD-USER / 100).
           MOVE FUNCTION MOD(CENT-SALDO-ORD-USER, 100)
               TO MOV-SALDOPOS-DEC.

           *> Se escribe el movimiento del ordenante
           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.
           

           *> MOVIMIENTO DEL DESTINO
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
    
           MOVE MSJ-DST         TO MOV-CONCEPTO.
    
           *> Se aumenta el saldo del receptor y se pasa a EUR
           ADD CENT-IMPOR-USER TO CENT-SALDO-DST-USER.
           COMPUTE MOV-SALDOPOS-ENT = (CENT-SALDO-DST-USER / 100).
           MOVE FUNCTION MOD(CENT-SALDO-DST-USER, 100)
               TO MOV-SALDOPOS-DEC.
           
           *> Escribimos movimiento destino
           WRITE MOVIMIENTO-REG INVALID KEY GO TO PSYS-ERR.

          
           CLOSE F-MOVIMIENTOS.

           *> Filtramos si transferencia era puntual o mensual
           IF (MSJ-PERIOD = MSJ-PUNTUAL-PERIOD) THEN
                   GO TO TRANSFERENCIA-PUNTUAL.

           IF (MSJ-PERIOD = MSJ-MENSUAL-PERIOD) THEN
                   GO TO TRANSFERENCIA-MENSUAL.
           

       TRANSFERENCIA-PUNTUAL.
           *>BORRAMOS LA TRANSFERENCIA EN EL FICHERO transferencias.ubd
           CLOSE TRANSFERENCIAS.
            
           MOVE TRAN-NUM TO TRANS-NUM.

           PERFORM TRANSFERENCIAS-OPEN THRU TRANSFERENCIAS-OPEN.
         
           DELETE TRANSFERENCIAS INVALID KEY GO TO PSYS-ERR.
          
           
           CLOSE F-MOVIMIENTOS.
           CLOSE TRANSFERENCIAS.
           CLOSE TARJETAS.
           *> Volvemos para leer otra transferencia pendiente
           GO TO TRANSFERENCIAS-OPEN.


       TRANSFERENCIA-MENSUAL.
            *>REESCRIBIMOS LA TRANSFERENCIA EN EL 
            *>FICHERO transferencias.ubd
           CLOSE TRANSFERENCIAS. 
           
           *> Si es diciembre, reescribimos con los datos de enero
           *> y ano siguiente
           IF (MES = 12) 
              MOVE 1 TO TRANS-MES
              ADD 1 TO TRANS-ANO.
           
           *> Si no es diciembre, incrementamos en uno el mes
           IF (MES <> 12) 
               ADD 1 TO TRANS-MES.

           MOVE TRAN-NUM TO TRANS-NUM.
           
           PERFORM TRANSFERENCIAS-OPEN THRU TRANSFERENCIAS-OPEN.
          
           *> REESCRIBIMOS LA TRANSFERENCIA PARA QUE SE HAGA EN EL 
           *> SIGUIENTE MES
           REWRITE TRANSFERENCIA-REG INVALID KEY GO PSYS-ERR.

           
           CLOSE F-MOVIMIENTOS.
           CLOSE TRANSFERENCIAS.
           CLOSE TARJETAS.

           *> Volvemos para leer otra transferencia pendiente
           GO TO TRANSFERENCIAS-OPEN.

           
       NO-MONEY.
           MOVE 0 TO MOV-SALDOPOS-ENT.
           MOVE 0 TO MOV-SALDOPOS-DEC.
           GO TO CALCULO-SALDO-DESTINO-USUARIO.

       
       USER-BAD.
           CLOSE TARJETAS.
           MOVE TRANS-NUM TO TRANS-NUM.
           DELETE TRANSFERENCIAS INVALID KEY GO PSYS-ERR.   
      

       PSYS-ERR.
           CLOSE TRANSFERENCIAS.
           CLOSE TARJETAS.
           CLOSE F-MOVIMIENTOS.

           DISPLAY  "Ha ocurrido un error".
           DISPLAY  "Vuelva mas tarde".

       FIN.
       CLOSE F-MOVIMIENTOS.
       CLOSE TRANSFERENCIAS.
       CLOSE TARJETAS.

       DISPLAY "OK".