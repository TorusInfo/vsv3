100  '  PSI Informatica
110  '
120  '  CODE39.BAS - Programa de impressao do codigo de barras CODE 39.
130  '               Utilizacao em impressoras compativeis com a EPSON.
140  '
150  DIM P$(80)
160  WIDTH LPRINT 255
170  CLS
180  GOSUB 1000                      : ' Chama rotina de tela
190  IF A$ = CHR$(27) THEN CLS : END : ' Encerra quando A$ = Esc
200  LOCATE 18,18,1 : PRINT STRING$(62," ");
210  LOCATE 18,1,1  : PRINT "Codigo de Barra? ";
220  NN$ = ""
230  GOSUB 5000                      : ' Aceita codigo
240  IF NN$ = "" THEN 180
250  GOSUB 2000                      : ' Chama a rotina de impressao
260  GOTO  200                       : ' Input do proximo codigo
1000  '
1010  ' Tela de entrada de variaveis
1020  '
1030  LOCATE 1,1,0  
1040  PRINT "MODULO FONTE - Codigo de Barras  CODE 39 ";
1050  LOCATE 2,1,0  : PRINT STRING$(80,"-");
1060  LOCATE 8,1,0  : PRINT "(1) Densidade (1/2).....: .  (Esc para encerrar)";
1070  LOCATE 10,1,0 : PRINT "(2) Altura da barra.....: .";
1080  LOCATE 12,1,0 : PRINT "(3) Numero de repassadas: .";
1090  LOCATE 14,1,0 : PRINT "(4) Tabulacao...........: ..";
1100  LOCATE 18,1,0 : PRINT STRING$(80," ");
1110  ' 
1120  ' Aceita densidade de impressao do codigo de barras
1130  '         **  DS%  **
1140  '      1 - Densidade simples
1150  '      2 - Densidade dupla
1160  '
1170  LOCATE 8,27,1 : PRINT ".  (Esc para encerrar)";
1180  LOCATE 8,27,1 : PRINT ".";
1190  A$ = INPUT$(1)
1200  IF A$ = CHR$(27) THEN RETURN
1210  IF A$ <> "1" AND A$ <> "2" THEN 1170
1220  LOCATE 8,27,0 : PRINT A$;"                        ";
1230  DS% = VAL(A$)
1240  '
1250  ' Aceita altura de impressao com altura minima 2 (dois),
1260  ' incluindo o codigo de barras e sua descricao
1270  '         **  ALT%  **
1280  '
1290  LOCATE 10,27,1 : PRINT ".";
1300  A$ = INPUT$(1)
1310  IF A$ = CHR$(27) THEN 1170     : ' Esc volta para densidade
1320  IF A$ < "2" OR A$ > "9" THEN 1290
1330  LOCATE 10,27,0 : PRINT A$;
1340  ALT% = VAL(A$) - 1
1350  '
1360  ' Aceita numero de repassadas 0 a 5
1370  ' 0 e' a impressao normal da barra sem repassar
1380  ' 1 repassa uma vez , e assim por diante
1390  '         **  REP%  **
1400  '
1410  LOCATE 12,27,1 : PRINT ".";
1420  A$ = INPUT$(1)
1430  IF A$ = CHR$(27) THEN 1290     : ' Esc volta para altura da barra
1440  IF A$ = CHR$(13) THEN A$ = "0"
1450  IF A$ < "0" OR A$ > "5" THEN 1410 : ' Aceita ate 5 repassadas
1460  LOCATE 12,27,1 : PRINT A$;
1470  REP% = VAL (A$)
1480  '
1490  ' Aceita valor da tabulacao na impressao do codigo de barras
1500  '         **  TB%  **
1510  '
1520  LOCATE 14,27,1 : PRINT "..";
1530  A$ = INPUT$(1)
1540  IF A$ = CHR$(27) THEN 1410     : ' Esc volta para repassada
1550  IF A$ = CHR$(13) THEN C$ = "00" : GOTO 1620
1560  IF A$ < "0" OR A$ > "9" THEN 1520
1570  LOCATE 14,28,1 : PRINT A$;
1580  B$ = INPUT$(1)
1590  IF B$ = CHR$(13) THEN C$ = "0" + A$ : GOTO 1620
1600  IF B$ <"0" OR B$ > "9" THEN 1570
1610  C$ = A$ + B$
1620  LOCATE 14,27,1 : PRINT C$;
1630  TB% = VAL(C$)
1640  RETURN
2000 '
2010 ' Rotina de impressao do codigo de barras
2020 '
2030 GOSUB 3000                      : ' Rotina para deletar brancos do codigo
2040 NT$ = "*" + NT$ + "*"           : ' Acrescenta caracteres Start/Stop
2050 NC% = NC% + 2
2060 L1% = 0
2070   FOR I1% = 1 TO NC%            : ' Para cada caracter de NT$
2080   C$ = MID$(NT$,I1%,1)
2090   GOSUB 4000                    : ' Procura codigo correspondente
2100   IF B$ = "" THEN LOCATE 20,1,0 : PRINT STRING$(80," "); : RETURN
2110   IF I1% = NC% THEN 2130
2120   B$ = B$ + CHR$(0) + CHR$(0) + CHR$(0)
2130   P$(I1%) = B$                  : ' Padrao de barras do (I1%)-esimo carac.
2140   L1% = L1% + LEN(B$)           : ' No. dots graficos do codigo de barras
2150   NEXT I1%
2160 REAL% = ALT% * 216 / 6          : ' Alt. real baseada no menor dot (1/216)
2170 TOTLIN% = INT ( REAL% / ( 24 + REP% ) )
2180                                   ' Linhas utilizadas para 24/216
2190 COMPENSA% = REAL% - TOTLIN% * ( 24 + REP% )
2200                                   ' Diferenca a compensar
2210 IF COMPENSA% < 4 THEN COMPENSA% = COMPENSA%+24+REP% : TOTLIN% = TOTLIN%-1
2220 L3% = L1%
2240 L2% = INT(L1% / 256)
2250 L1% = L1% - 256 * L2%
2251 PRINT : PRINT L3% : PRINT : PRINT L2% : PRINT : PRINT L1%
2252 INPUT Z$
2260   FOR I2% = 1 TO TOTLIN%        : ' Impressao de cada linha de barras
2270   CNT% = 0                      : ' Contador de repassadas
2280   LPRINT STRING$(TB%," ");      : ' Imprime tabulacao
2285   LPRINT CHR$(27);"L";CHR$(L1%);CHR$(L2%);
2290                                   ' Modo de impressao grafica
2320     FOR I1% = 1 TO NC%
2330     LPRINT P$(I1%);             : ' Impressao do codigo
2340     NEXT I1%
2350   CNT% = CNT% + 1
2360   IF CNT% <= REP% THEN LPRINT CHR$(27);"3";CHR$(1) : GOTO 2280
2370                                   ' Espaco entre linha 1/216 p/ repassada
2380   LPRINT CHR$(27);"3";CHR$(24)  : ' Cada linha de barra ocupa 24/216
2390   NEXT I2%
2400 LPRINT CHR$(27);"3";CHR$(COMPENSA%)
2410 LPRINT CHR$(27);"2";            : ' Espacamento entre linhas 1/6
2420 L4% = 0
2430 IF LEN(NN$)*12 < L3% THEN L4% =INT( INT( ( L3% - (LEN(NN$)*12) ) / 2)/12)
2440 LPRINT STRING$(L4% + TB%," ");NN$
2450                                   ' Imprime codigo tabulado e centraliz.
2460 RETURN
3000 '
3010 '  Subrotina para deletar brancos no fim do codigo.
3020 '
3030 NC% = LEN(NN$)
3040 C$  = MID$(NN$,NC%,1)
3050 IF C$ = " " OR C$ = "" THEN NC% = NC% - 1 : GOTO 3040
3060 NT$ = LEFT$(NN$,NC%)
3070 RETURN
4000 '
4010 '  Padrao de definicao do codigo de barras CODE 39.
4020 '
4030 IF DS% = 2 THEN 4090
4040 B1$ = CHR$(255)+CHR$(255)     : ' Definicao de barra fina   /Dens. simples
4050 B2$ = B1$ + B1$ + B1$         : ' Definicao de barra larga  /Dens. simples
4060 S1$ = CHR$(0)+CHR$(0)+CHR$(0) : ' Definicao de espaco fino  /Dens. simples
4070 S2$ = S1$ + S1$               : ' Definicao de espaco largo /Dens. simples
4080 IF DS% = 1 THEN 4130
4090 B1$ = CHR$(255)               : ' Definicao de barra fina   /Dens. dupla
4100 B2$ = B1$ + B1$ + B1$         : ' Definicao de barra larga  /Dens. dupla
4110 S1$ = CHR$(0)+CHR$(0)+CHR$(0) : ' Definicao de espaco fino  /Dens. dupla
4120 S2$ = S1$ + CHR$(0) + CHR$(0) : ' Definicao de espaco largo /Dens. dupla
4130 IF C$ = "*" THEN B$ = B1$+S2$+B1$+S1$+B2$+S1$+B2$+S1$+B1$ : RETURN
4140 IF C$ = "1" THEN B$ = B2$+S1$+B1$+S2$+B1$+S1$+B1$+S1$+B2$ : RETURN
4150 IF C$ = "2" THEN B$ = B1$+S1$+B2$+S2$+B1$+S1$+B1$+S1$+B2$ : RETURN
4160 IF C$ = "3" THEN B$ = B2$+S1$+B2$+S2$+B1$+S1$+B1$+S1$+B1$ : RETURN
4170 IF C$ = "4" THEN B$ = B1$+S1$+B1$+S2$+B2$+S1$+B1$+S1$+B2$ : RETURN
4180 IF C$ = "5" THEN B$ = B2$+S1$+B1$+S2$+B2$+S1$+B1$+S1$+B1$ : RETURN
4190 IF C$ = "6" THEN B$ = B1$+S1$+B2$+S2$+B2$+S1$+B1$+S1$+B1$ : RETURN
4200 IF C$ = "7" THEN B$ = B1$+S1$+B1$+S2$+B1$+S1$+B2$+S1$+B2$ : RETURN
4210 IF C$ = "8" THEN B$ = B2$+S1$+B1$+S2$+B1$+S1$+B2$+S1$+B1$ : RETURN
4220 IF C$ = "9" THEN B$ = B1$+S1$+B2$+S2$+B1$+S1$+B2$+S1$+B1$ : RETURN
4230 IF C$ = "0" THEN B$ = B1$+S1$+B1$+S2$+B2$+S1$+B2$+S1$+B1$ : RETURN
4240 IF C$ = "A" THEN B$ = B2$+S1$+B1$+S1$+B1$+S2$+B1$+S1$+B2$ : RETURN
4250 IF C$ = "B" THEN B$ = B1$+S1$+B2$+S1$+B1$+S2$+B1$+S1$+B2$ : RETURN
4260 IF C$ = "C" THEN B$ = B2$+S1$+B2$+S1$+B1$+S2$+B1$+S1$+B1$ : RETURN
4270 IF C$ = "D" THEN B$ = B1$+S1$+B1$+S1$+B2$+S2$+B1$+S1$+B2$ : RETURN
4280 IF C$ = "E" THEN B$ = B2$+S1$+B1$+S1$+B2$+S2$+B1$+S1$+B1$ : RETURN
4290 IF C$ = "F" THEN B$ = B1$+S1$+B2$+S1$+B2$+S2$+B1$+S1$+B1$ : RETURN
4300 IF C$ = "G" THEN B$ = B1$+S1$+B1$+S1$+B1$+S2$+B2$+S1$+B2$ : RETURN
4310 IF C$ = "H" THEN B$ = B2$+S1$+B1$+S1$+B1$+S2$+B2$+S1$+B1$ : RETURN
4320 IF C$ = "I" THEN B$ = B1$+S1$+B2$+S1$+B1$+S2$+B2$+S1$+B1$ : RETURN
4330 IF C$ = "J" THEN B$ = B1$+S1$+B1$+S1$+B2$+S2$+B2$+S1$+B1$ : RETURN
4340 IF C$ = "K" THEN B$ = B2$+S1$+B1$+S1$+B1$+S1$+B1$+S2$+B2$ : RETURN
4350 IF C$ = "L" THEN B$ = B1$+S1$+B2$+S1$+B1$+S1$+B1$+S2$+B2$ : RETURN
4360 IF C$ = "M" THEN B$ = B2$+S1$+B2$+S1$+B1$+S1$+B1$+S2$+B1$ : RETURN
4370 IF C$ = "N" THEN B$ = B1$+S1$+B1$+S1$+B2$+S1$+B1$+S2$+B2$ : RETURN
4380 IF C$ = "O" THEN B$ = B2$+S1$+B1$+S1$+B2$+S1$+B1$+S2$+B1$ : RETURN
4390 IF C$ = "P" THEN B$ = B1$+S1$+B2$+S1$+B2$+S1$+B1$+S2$+B1$ : RETURN
4400 IF C$ = "Q" THEN B$ = B1$+S1$+B1$+S1$+B1$+S1$+B2$+S2$+B2$ : RETURN
4410 IF C$ = "R" THEN B$ = B2$+S1$+B1$+S1$+B1$+S1$+B2$+S2$+B1$ : RETURN
4420 IF C$ = "S" THEN B$ = B1$+S1$+B2$+S1$+B1$+S1$+B2$+S2$+B1$ : RETURN
4430 IF C$ = "T" THEN B$ = B1$+S1$+B1$+S1$+B2$+S1$+B2$+S2$+B1$ : RETURN
4440 IF C$ = "U" THEN B$ = B2$+S2$+B1$+S1$+B1$+S1$+B1$+S1$+B2$ : RETURN
4450 IF C$ = "V" THEN B$ = B1$+S2$+B2$+S1$+B1$+S1$+B1$+S1$+B2$ : RETURN
4460 IF C$ = "W" THEN B$ = B2$+S2$+B2$+S1$+B1$+S1$+B1$+S1$+B1$ : RETURN
4470 IF C$ = "X" THEN B$ = B1$+S2$+B1$+S1$+B2$+S1$+B1$+S1$+B2$ : RETURN
4480 IF C$ = "Y" THEN B$ = B2$+S2$+B1$+S1$+B2$+S1$+B1$+S1$+B1$ : RETURN
4490 IF C$ = "Z" THEN B$ = B1$+S2$+B2$+S1$+B2$+S1$+B1$+S1$+B1$ : RETURN
4500 IF C$ = "-" THEN B$ = B1$+S2$+B1$+S1$+B1$+S1$+B2$+S1$+B2$ : RETURN
4510 IF C$ = "." THEN B$ = B2$+S2$+B1$+S1$+B1$+S1$+B2$+S1$+B1$ : RETURN
4520 IF C$ = " " THEN B$ = B1$+S2$+B2$+S1$+B1$+S1$+B2$+S1$+B1$ : RETURN
4530 IF C$ = "$" THEN B$ = B1$+S2$+B1$+S2$+B1$+S2$+B1$+S1$+B1$ : RETURN
4540 IF C$ = "/" THEN B$ = B1$+S2$+B1$+S2$+B1$+S1$+B1$+S2$+B1$ : RETURN
4550 IF C$ = "+" THEN B$ = B1$+S2$+B1$+S1$+B1$+S2$+B1$+S2$+B1$ : RETURN
4560 IF C$ = "%" THEN B$ = B1$+S1$+B1$+S2$+B1$+S2$+B1$+S2$+B1$ : RETURN
4570 LOCATE 20,10,0 : PRINT "***** Caracter invalido para Code 39 *****";
4580 FOR I% = 1 TO 500 : NEXT I% : B$ = "" : RETURN
5000 '
5010 ' Rotina aceita string na tela
5020 ' NN$  - string aceita
5030 ' CC%  - coluna
5040 ' TAM% - tamanho de NN$
5050 '
5060 CC% = 18 : TAM% = 0
5070 LOCATE 18,CC%+TAM%,0
5080 PRINT CHR$(95);
5090 A$ = INPUT$(1)
5100 IF A$ = CHR$(13) THEN IF NN$ = "" THEN 5090 ELSE 5310
5110 IF A$ = CHR$(27) THEN 5290
5120 IF A$ <> CHR$(8) THEN 5190
5130 IF TAM% = 0 THEN 5090
5140 NN$ = LEFT$(NN$,LEN(NN$)-1)
5150 TAM% = TAM% - 1
5160 LOCATE 18,CC%+TAM%,0
5170 PRINT "  ";
5180 GOTO 5070
5190 IF A$ < CHR$(32) OR A$ > CHR$(126) THEN 5090
5200 NN$ = NN$ + A$
5210 LOCATE 18,CC%+TAM%,0
5220 PRINT A$;
5230 TAM% = TAM% + 1
5240 IF TAM% < 60 THEN 5070
5250 LOCATE 18,CC%+TAM%,0
5260 PRINT CHR$(219);
5270 A$ = INPUT$(1)
5280 IF A$ = CHR$(8) THEN 5130
5290 IF A$ = CHR$(27) THEN NN$ = "" : GOTO 5330
5300 IF A$ <> CHR$(13) THEN 5270
5310 LOCATE 18,TAM%+CC%,0
5320 PRINT " ";
5330 RETURN
