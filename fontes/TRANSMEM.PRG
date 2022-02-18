clear
set confirm ON
clear
restore from \PODIUM\POD_09.3
use \VSV\TEMPCAT
zap
for a = 0 to 9

	append blank
	replace CATEG with substr( TPFTELA, A * 10 + 1, 10 )

next
close
use \VSV\TEMPDAB
zap
for a = 1 to 30

	append blank
	replace DIAS with val( substr( TPFTELA1, A, 1 ) )

next
close
use \VSV\TEMPQFT
zap
for a = 1 to 21

	append blank
	replace QFT with val( substr( TPFTELA2, A, 1 ) )

next
close all
quit
