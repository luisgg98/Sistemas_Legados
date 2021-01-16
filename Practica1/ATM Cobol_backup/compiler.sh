#!/bin/bash
rm -f BANK1
cobc -x  BANK1.cbl BANK2.cbl BANK3.cbl BANK4.cbl BANK5.cbl BANK6.cbl BANK7.cbl BANK8.cbl BANK9.cbl PERIOD_BANK.cbl 
./BANK1
