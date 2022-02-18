 10  '  PSI Informatica
 20  '
 30  '  INT25.BAS - Programa de impressao do codigo de barras INTER. 2 DE 5.
 40  '              Utilizacao em impressoras compativeis com a EPSON.
 50  '
 60  '   18/05/90:
 70  '   Modificado para pular 7 pts entre linhas
 80  '   Modificado para imprimir Barras de Protecao Superior e Inferior
 90  '   Modificado para aumentar o tamanho dos espacos
100  '
110  DIM P$(80),T(9,5),B$(1),S$(1),D$(50)
120  DEFINT A-Z
130  WIDTH LPRINT 255
140  '
150  ' Define Parametros da Impressora
160  '
170  LPP  = 6	 : ' LPP - linhas de texto por polegada
180  PPPV = 216  : ' PPPV - pontos por polegada na vertical
190  PPPH = 120  : ' PPPH - pontos por polegada na horizontal
200		: ' (este parametro esta' implicito na largura das barras e espacos)
210  '
220  CLS
230  GOSUB 330                      : ' Chama rotina de tela
240  GOSUB 1590                      : ' Carrega tabela de codigos
250  IF A$ = CHR$(27) THEN CLS : END : ' Encerra quando A$ = Esc
260  LOCATE 18,18,1 : PRINT STRING$(62," ");
270  LOCATE 18,1,1  : PRINT "Codigo de Barra? ";
280  NN$ = ""
290  GOSUB 1900                      : ' Aceita codigo
300  IF NN$ = "" THEN 230
310  GOSUB 980                      : ' Chama a rotina de impressao
320  GOTO  260                       : ' Input do proximo codigo
330  '
340  ' Tela de entrada de variaveis
350  '
360  LOCATE 1,1,0  
370  PRINT "MODULO FONTE - Codigo de Barras  INTER. 2 DE 5";
380  LOCATE 2,1,0  : PRINT STRING$(80,"-");
390  LOCATE 8,1,0  : PRINT "(1) Densidade (1/2).....: .  (Esc para encerrar)";
400  LOCATE 10,1,0 : PRINT "(2) Altura da barra.....: .";
410  LOCATE 12,1,0 : PRINT "(3) Numero de repassadas: .";
420  LOCATE 14,1,0 : PRINT "(4) Tabulacao...........: ..";
430  LOCATE 18,1,0 : PRINT STRING$(80," ");
440  ' 
450  ' Aceita densidade de impressao do codigo de barras
460  '         **  DS  **
470  '      1 - Densidade simples
480  '      2 - Densidade dupla
490  '
500  LOCATE 8,27,1 : PRINT ".  (Esc para encerrar)";
510  LOCATE 8,27,1 : PRINT ".";
520  A$ = INPUT$(1)
530  IF A$ = CHR$(27) THEN RETURN 
540  IF A$ <> "1" AND A$ <> "2" THEN 500
550  LOCATE 8,27,0 : PRINT A$;"                        ";
560  DS = VAL(A$)
570  '
580  ' Aceita altura da barra com altura minima 2 (dois),
590  ' incluindo o codigo de barras e sua descricao
600  '         **  ALT  **
610  '
620  LOCATE 10,27,1 : PRINT ".";
630  A$ = INPUT$(1)
640  IF A$ = CHR$(27) THEN 500     : ' Esc volta para densidade
650  IF A$ < "2" OR A$ > "9" THEN 620
660  LOCATE 10,27,0 : PRINT A$;
670  ALT = VAL(A$) - 1
680  '
690  ' Aceita numero de repassadas 0 a 9
700  ' 0 e' a impressao normal da barra sem repassar
710  ' 1 repassa uma vez , e assim por diante
720  '         **  REP  **
730  '
740  LOCATE 12,27,1 : PRINT ".";
750  A$ = INPUT$(1)
760  IF A$ = CHR$(27) THEN 620     : ' Esc volta para altura da barra
770  IF A$ = CHR$(13) THEN A$ = "0"
780  IF A$ < "0" OR A$ > "9" THEN 740
790  LOCATE 12,27,1 : PRINT A$;
800  REP = VAL (A$)
810  '
820  ' Aceita valor da tabulacao na impressao do codigo
830  '         **  TB  **
840  '
850  LOCATE 14,27,1 : PRINT "..";
860  A$ = INPUT$(1)
870  IF A$ = CHR$(27) THEN 740     : ' Esc volta para repassada
880  IF A$ = CHR$(13) THEN C$ = "00" : GOTO 950
890  IF A$ < "0" OR A$ > "9" THEN 850
900  LOCATE 14,28,1 : PRINT A$;
910  B$ = INPUT$(1)
920  IF B$ = CHR$(13) THEN C$ = "0" + A$ : GOTO 950
930  IF B$ <"0" OR B$ > "9" THEN 900
940  C$ = A$ + B$
950  LOCATE 14,27,1 : PRINT C$;
960  TB = VAL(C$)
970  RETURN
980 '
990 ' Rotina de impressao do codigo de barras
1000 '
1010 GOSUB 1430                    : ' Rotina deleta brancos do codigo
1020 IF C$ = "" THEN LOCATE 20,1,0 : PRINT STRING$(80," "); : RETURN
1030 NC = INT( NC / 2 )
1040 L1 = 0
1050   FOR I1 = 1 TO NC            : ' Para cada caracter de NN$
1060   N1 = 1 + 2 * ( I1 - 1 )
1070   N2 = N1 + 1
1080   N1 = VAL (D$(N1))
1090   N2 = VAL (D$(N2))
1100   GOSUB 1820                  : ' Procura codigo na tabela
1110   P$(I1) = C$                 : ' Padrao de barras do (I1)-esimo carac.
1120   L1 = L1 + LEN(C$)           : ' No. dots graficos do codigo de barras
1130   NEXT I1
1140 L1 = L1 + LEN(ST$) + LEN(SP$)            : ' Acrescenta Start/Stop
1150 REAL = ALT * PPPV / LPP        
1151                               : ' Altura real baseada no menor dot (1/216)
1160 TOTLIN = INT(REAL / (22+REP)) : ' Altura utilizada (22/216) -> 7 pts + 1/3
1170 COMPENSA = REAL - TOTLIN * (22 + REP)  : ' Diferenca a compensar
1180 IF COMPENSA < 8 THEN COMPENSA = COMPENSA+22+REP : TOTLIN = TOTLIN-1
1190 L3 = L1
1200 L2 = INT(L1 / 256)
1210 L1 = L1 - 256 * L2
1220   FOR I2 = 1 TO TOTLIN        : ' Impressao de cada linha de barras
1230   CNT = 0        	             : ' Contador de repassadas
1240   LPRINT STRING$(TB," ");      : ' Imprime tabulacao
1250   LPRINT CHR$(27);"L";CHR$(L1);CHR$(L2); 
1251              : ' Modo de impressao grafica (densidade dupla da impressora)
1260   A$ = ST$ : GOSUB 2240 : LPRINT A$;      : ' Imprime Start
1270     FOR I1 = 1 TO NC
1280     A$ = P$(I1) : GOSUB 2240 : LPRINT A$;
1290     NEXT I1
1300   A$ = SP$ : GOSUB 2240 : LPRINT A$;      : ' Imprime Stop
1310   CNT = CNT + 1
1320   IF CNT <= REP THEN LPRINT CHR$(27);"3";CHR$(1) : GOTO 1240
1330                                   ' Espaco entre linha 1/216 p/ repassada
1340   LPRINT CHR$(27);"3";CHR$(22) : ' Avanca ate' a prox. linha
1350   NEXT I2
1360 LPRINT CHR$(27);"3";CHR$(COMPENSA) 
1361 '        : ' Avanca n/216 - suficiente p/ alinhar com outra linha de texto
1370 LPRINT CHR$(27);"2";            : ' Espacamento entre linhas 1/6
1380 IF LPP = 8 THEN LPRINT CHR$(27);"0"; : ' Espacamento entre linhas 1/8
1390 L4 = 0
1400 IF LEN(NN$)*12 < L3 THEN L4 = INT(INT ( ( L3 - (LEN(NN$)*12) ) / 2)/12)
1410 LPRINT STRING$(L4 + TB," ");NN$ : ' Imprime codigo tabulado e central.
1420 RETURN
1430 '
1440 '  Subrotina para deletar brancos no fim do codigo.
1450 '
1460 NC = LEN(NN$)
1470 C$  = MID$(NN$,NC,1)
1480 IF C$ = " " OR C$ = "" THEN NC = NC - 1 : GOTO 1470
1490 NN$ = LEFT$(NN$,NC)
1500 IF (NC - (2 * INT(NC / 2))) = 1 THEN NC = NC + 1 : NN$ = "0" + NN$ 
1501 '                             : ' Qtd. par de caracteres 
1510   FOR I = 1 TO NC
1520   D$(I) = MID$(NN$,I,1)
1530   IF D$(I) < "0" OR D$(I) > "9" THEN 1560
1540   NEXT I
1550 RETURN
1560 LOCATE 20,10,0
1570 PRINT "***** Caracter invalido para o Inter. 2 de 5 *****";
1580 FOR I = 1 TO 500 : NEXT I : C$ = "" : RETURN
1590 '
1600 '  Padrao de definicao do codigo de barras INTER. 2 DE 5.
1610 '
1620 C$ = CHR$(255)                : ' Densidade dupla
1630 IF DS = 1 THEN C$ = C$ + C$   : ' Densidade simples
1640 B$(0) = C$                    : ' Definicao de barra fina
1650 B$(1) = C$ + C$ + C$          : ' Definicao de barra larga
1660 S$(0) = STRING$(3,CHR$(0))    : ' Definicao de espaco fino
1670 S$(1) = STRING$(6,CHR$(0))    : ' Definicao de espaco largo
1680 IF DS = 2 THEN S$(1) = STRING$(5,CHR$(0))
1681 '                             : ' Espaco Largo / Densidade simples

1683    ' o 0 indica que espaco ou barra fino
1684    ' o 1 indica que espaco ou barra grosso
1685    ' o primeiro digito e' codificado pelas barras
1686    ' o segundo  digito e' codificado pelos espacos
1687    ' inicio = BF EF BF EF   final = BG EF BF
1688    ' barra fina = 1 agulhada / espaco fino = 2 nao agulhadas

1690 T(0,1) = 0 : T(0,2) = 0 : T(0,3) = 1 : T(0,4) = 1 : T(0,5) = 0 : ' 0
1700 T(1,1) = 1 : T(1,2) = 0 : T(1,3) = 0 : T(1,4) = 0 : T(1,5) = 1 : ' 1
1710 T(2,1) = 0 : T(2,2) = 1 : T(2,3) = 0 : T(2,4) = 0 : T(2,5) = 1 : ' 2
1720 T(3,1) = 1 : T(3,2) = 1 : T(3,3) = 0 : T(3,4) = 0 : T(3,5) = 0 : ' 3
1730 T(4,1) = 0 : T(4,2) = 0 : T(4,3) = 1 : T(4,4) = 0 : T(4,5) = 1 : ' 4
1740 T(5,1) = 1 : T(5,2) = 0 : T(5,3) = 1 : T(5,4) = 0 : T(5,5) = 0 : ' 5
1750 T(6,1) = 0 : T(6,2) = 1 : T(6,3) = 1 : T(6,4) = 0 : T(6,5) = 0 : ' 6
1760 T(7,1) = 0 : T(7,2) = 0 : T(7,3) = 0 : T(7,4) = 1 : T(7,5) = 1 : ' 7
1770 T(8,1) = 1 : T(8,2) = 0 : T(8,3) = 0 : T(8,4) = 1 : T(8,5) = 0 : ' 8
1780 T(9,1) = 0 : T(9,2) = 1 : T(9,3) = 0 : T(9,4) = 1 : T(9,5) = 0 : ' 9
1790 ST$ = B$(0) + S$(0) + B$(0) + S$(0) : ' Start
1800 SP$ = B$(1) + S$(0) + B$(0)         : ' Stop
1810 RETURN
1820 '
1830 '  Obtem o padrao correspondente do codigo
1840 '
1850 C$ = ""
1860   FOR J = 1 TO 5
1870   C$ = C$ + B$(T(N1,J)) + S$(T(N2,J))
1880   NEXT J
1890 RETURN
1900 '
1910 ' Rotina aceita string na tela
1920 ' NN$  - string aceita
1930 ' CC  - coluna
1940 ' TAM - tamanho de NN$
1950 '
1960 CC = 18 : TAM = 0
1970 LOCATE 18,CC+TAM,0
1980 PRINT CHR$(95);
1990 A$ = INPUT$(1)
2000 IF A$ = CHR$(13) THEN IF NN$ = "" THEN 1990 ELSE 2210
2010 IF A$ = CHR$(27) THEN 2190
2020 IF A$ <> CHR$(8) THEN 2090
2030 IF TAM = 0 THEN 1990
2040 NN$ = LEFT$(NN$,LEN(NN$)-1)
2050 TAM = TAM - 1
2060 LOCATE 18,CC+TAM,0
2070 PRINT "  ";
2080 GOTO 1970
2090 IF A$ < CHR$(32) OR A$ > CHR$(126) THEN 1990
2100 NN$ = NN$ + A$
2110 LOCATE 18,CC+TAM,0
2120 PRINT A$;
2130 TAM = TAM + 1
2140 IF TAM < 60 THEN 1970
2150 LOCATE 18,CC+TAM,0
2160 PRINT CHR$(219);
2170 A$ = INPUT$(1)
2180 IF A$ = CHR$(8) THEN 2030
2190 IF A$ = CHR$(27) THEN NN$ = "" : GOTO 2230
2200 IF A$ <> CHR$(13) THEN 2170
2210 LOCATE 18,TAM+CC,0
2220 PRINT " ";
2230 RETURN
2240 '
2250 ' Rotina que coloca Barra de Protecao , se necessario
2260 '
2270   IF NOT(I2=1 AND CNT=0) THEN 2340 
2271 '         : ' Se for 1. linha e 1. passada entao imprime Barreira Superior
2280   B$ = ""
2290   FOR  I = 1 TO LEN(A$)
2300   B$ = B$ + CHR$(ASC(MID$(A$,I,1)) OR (&HC0))
2310   NEXT I
2320   A$ = B$
2330   RETURN
2340   IF NOT(I2 = TOTLIN) THEN RETURN 
2341                    : ' Se for ultima linha entao imprime Barreira Inferior
2350   B$ = ""
2360   FOR  I = 1 TO LEN(A$)
2370   J = ASC(MID$(A$,I,1)) OR (&H3)
2380   ' Caso seja desejavel nao imprimir a ultima passada completa
2390   ' multiplicar J por 2 tantas vezes quantos 
2391   ' forem os pontos a serem 'desligados'
2400   B$ = B$ + CHR$(J)
2410   NEXT I
2420   A$ = B$
2430   RETURN
