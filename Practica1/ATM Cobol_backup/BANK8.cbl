       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK8.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-TARJETAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TNUM-E
           FILE STATUS IS FST.

           SELECT INTENTOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS INUM
           FILE STATUS IS FSI.       

       DATA DIVISION.
       FILE SECTION.
       FD F-TARJETAS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "tarjetas.ubd".
       01 TARJETAREG.
           02 TNUM-E      PIC 9(16).
           02 TPIN-E      PIC  9(4).
       FD INTENTOS
           LABEL RECORD STANDARD
           VALUE OF FILE-ID IS "intentos.ubd".
       01 INTENTOSREG.
           02 INUM      PIC 9(16).
           02 IINTENTOS PIC 9(1).       

       WORKING-STORAGE SECTION.
       *>> VARIABLE PARA COMPROBAR DONDE ESTA EL ERROR
       77 FST                      PIC   X(2).
       77 FSI                    PIC   X(2).
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

       77 PRESSED-KEY BLANK WHEN ZERO  PIC   9(4).


       77 CLAVE-ANTIGUA             PIC  9(4).
       77 CLAVE-INTRODUCIR             PIC  9(4).
       77 PRIMERA-CLAVE-NUEVA             PIC  9(4).
       77 SEGUNDA-CLAVE-NUEVA             PIC  9(4).
       77 CHOICE BLANK WHEN ZERO   PIC  9(1).
       LINKAGE SECTION.
       77 TNUM                     PIC  9(16).

       SCREEN SECTION.
       01 BLANK-SCREEN.
           05 FILLER LINE 1 BLANK SCREEN BACKGROUND-COLOR BLACK.

       01 DATA-ACCEPT.
           05 BLANK ZERO SECURE AUTO 
               LINE 10 COL 54 PIC 9(4) USING CLAVE-INTRODUCIR.
           05  BLANK ZERO AUTO
               LINE 11 COL 54 PIC 9(4) USING PRIMERA-CLAVE-NUEVA.

           05  BLANK ZERO
               LINE 12 COL 54 PIC 9(4) USING SEGUNDA-CLAVE-NUEVA.

       PROCEDURE DIVISION USING TNUM.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.

       IMPRIMIR-CABECERA.

          DISPLAY BLANK-SCREEN.
           DISPLAY "Cajero Automatico UnizarBank"
               AT LINE 2 COL 26
               WITH FOREGROUND-COLOR IS 1.

           DISPLAY "Cambio de clave personal"
               AT LINE 7 COL 19
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
   

       TARJETA-OPEN.
           *>FORZAMOS QUE CREE UN FICHERO POR SI NO EXISTE
           OPEN I-O F-TARJETAS.
           IF FST <> 00 THEN
               GO TO PSYS-ERR
           END-IF.
           CLOSE F-TARJETAS.

           OPEN I-O INTENTOS.
           IF FSI NOT = 00 THEN
               GO TO PSYS-ERR.
           CLOSE INTENTOS.

       

       *>LEEMOS LA TARJETA PARA SABER CUAL ES SU CLAVE
       LECTURA-TARJETAS.
           OPEN I-O F-TARJETAS.
           READ F-TARJETAS NEXT RECORD AT END GO TO P2.
           IF TNUM-E = TNUM THEN              
                   MOVE TPIN-E  TO CLAVE-ANTIGUA
                   
           END-IF.
           GO TO LECTURA-TARJETAS.

       P2.
           CLOSE F-TARJETAS.
       *>LE DAMOS UN VALOR INICIAL
           INITIALIZE CLAVE-INTRODUCIR.
           INITIALIZE PRIMERA-CLAVE-NUEVA.
           INITIALIZE SEGUNDA-CLAVE-NUEVA.

           DISPLAY "Introduzca clave actual:" AT LINE 10 COL 19.
           DISPLAY "Introduzca clave nueva:" AT LINE 11 COL 19.
           DISPLAY "Repita la clave nueva:" AT LINE 12 COL 19.

           DISPLAY "Enter - Confirmar" AT LINE 24 COL 2.
           DISPLAY "ESC - Cancelar" AT LINE 24 COL 66.

           ACCEPT DATA-ACCEPT ON EXCEPTION *> IRENE: delete AT               
                  IF ENTER-PRESSED
                      GO TO ESCRIBIR-CLAVE
                  ELSE
                      IF ESC-PRESSED
                          EXIT PROGRAM
                      ELSE
                          GO TO P2.
       
           OPEN I-O INTENTOS.
           MOVE TNUM TO INUM.
           READ INTENTOS INVALID KEY GO TO PSYS-ERR.
           CLOSE INTENTOS.
    
           IF IINTENTOS = 0
               GO TO PINT-ERR.           

       *> LA CLAVE QUE INTRODUCE EL USUARIO Y LA QUE HABIA EN EL
       *> FICHERON HAN DE COINCIDIR
           IF CLAVE-ANTIGUA <> CLAVE-INTRODUCIR
               GO TO PPIN-ERR
           ELSE
               PERFORM REINICIAR-INTENTOS THRU REINICIAR-INTENTOS.
       *>COMPRUEBA QUE HAYA ESCRITO LA CLAVE 2 VECES
           IF PRIMERA-CLAVE-NUEVA <> SEGUNDA-CLAVE-NUEVA
               GO TO NO-COINCIDEN-CLAVES.

       *>COMPROBAMOS LOS VALORES INTRODUCIDOS
       ESCRIBIR-CLAVE.

           
           MOVE PRIMERA-CLAVE-NUEVA TO  TPIN-E.
           MOVE TNUM TO TNUM-E.
       *> ESCRIBE EN EL FICHERO Y COMRPUEBA QUE HAYA SIDO CORRECTO
           OPEN I-O F-TARJETAS.
           REWRITE TARJETAREG.
           IF FST <> 00
               GO TO PSYS-ERR.
           CLOSE F-TARJETAS.


       P-EXITO.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.

           DISPLAY  "La clave se ha cambiado correctamente"
                AT LINE 11 COL 19.
          
           DISPLAY  "Enter - Aceptar" AT LINE 24 COL 33.

           GO TO EXIT-ENTER.           

       ENTER-VERIFICACION.
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 80  ON EXCEPTION
           IF ESC-PRESSED THEN
               EXIT PROGRAM
           ELSE
               GO TO ENTER-VERIFICACION
           END-IF.
       *> INDICA QUE SE HA PRODUCIDO UN ERROR INTERNO
       PSYS-ERR.
           CLOSE F-TARJETAS.
           CLOSE INTENTOS.

           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY  "Ha ocurrido un error interno"
               AT LINE 11 COL 19
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY  "Vuelva mas tarde"
               AT LINE 12 COL 19
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.

           DISPLAY "Enter - Aceptar" AT LINE 24 COL 33.
       *>FUNCION PARA SALIR DE LA FUNCIONALIDAD
       EXIT-ENTER.
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 80 
           IF ENTER-PRESSED
               EXIT PROGRAM
           ELSE
               GO TO EXIT-ENTER.
       *>VOLVEMOS A CARGAR LA PANTALLA DE LA FUNCIONALIDAD
       BACK-ENTER.
           ACCEPT PRESSED-KEY OFF AT LINE 24 COL 80 
           IF ENTER-PRESSED
               GO TO IMPRIMIR-CABECERA
           ELSE
               GO TO BACK-ENTER.
       *> FUNCION QUE INDICA QUE LAS CLAVES NO COINCIDEN
       NO-COINCIDEN-CLAVES.
           CLOSE F-TARJETAS.
           CLOSE INTENTOS.
           PERFORM IMPRIMIR-CABECERA THRU IMPRIMIR-CABECERA.
           DISPLAY "Ha introducido mal la nueva clave."
               AT LINE 10 COL 19
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Los valores de ambos campos no coinciden."
               AT LINE 12 COL 19
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "La repetición ha de coincidir con el primer valor."
               AT LINE 14 COL 19
               WITH FOREGROUND-COLOR IS WHITE
                    BACKGROUND-COLOR IS RED.
           DISPLAY "Enter - Salir" AT LINE 24 COL 33.

           GO TO BACK-ENTER.

       PINT-ERR.
        *>FUNCION QUE INDICA QUE NO SE PUEDE HACER LA TARJETA
           CLOSE F-TARJETAS.
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

           GO TO EXIT-ENTER.
       *>COMPROBAMOS SI EL NUMERO DE INTENTOS HA LLEGADO A 0
       PPIN-ERR.
           OPEN I-O INTENTOS.
           SUBTRACT 1 FROM IINTENTOS.
           REWRITE INTENTOSREG INVALID KEY GO TO PSYS-ERR.

           CLOSE F-TARJETAS.
           CLOSE INTENTOS.
       *>EN EL CASO DE NO QUEDAR ACCESOS SE EL DENIEGA EL USO DE LA 
       *> TARJETA
           IF IINTENTOS = 0
               GO TO PINT-ERR.

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

           GO TO BACK-ENTER.
      
       REINICIAR-INTENTOS.
           MOVE 3 TO IINTENTOS.
           DISPLAY IINTENTOS.
           REWRITE INTENTOSREG INVALID KEY GO TO PSYS-ERR.
