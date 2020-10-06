       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCRIBI.
       *> Programa para escribir en el fichero de espectaculos y poder
       *> mostrar espectaculos por pantalla
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ESPECTACULOS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ESP-NUM.


       DATA DIVISION.
       FILE SECTION.
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
       PROCEDURE DIVISION.
       *> PUEDE QUE HAYA QUE PONER UN EXTEND
       OPEN I-O F-ESPECTACULOS.
       
       *> CAMBIAR ESTOS VALORES CADA VEZ QUE SE EJECUTE.
       MOVE 0012 TO ESP-NUM.
       MOVE 2021 TO ESP-ANO.
       MOVE 06  TO ESP-MES.
       MOVE 04 TO ESP-DIA.
       MOVE 02 TO ESP-HOR.
       MOVE 06 TO ESP-MIN.
       MOVE "Aprobar sistemas legados"
            TO ESP-DESCR.
       MOVE 0000002 TO ESP-DISP.
       MOVE  0022 TO ESP-PRECIO-ENT.
       MOVE  02 TO ESP-PRECIO-DEC

       WRITE ESPECTACULO-REG
       END-WRITE

       CLOSE F-ESPECTACULOS.
       
       STOP RUN.