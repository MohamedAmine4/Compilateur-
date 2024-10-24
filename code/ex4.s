@ Type exercise 4 here

SETI R0, #0
SETI R1, #0
SETI R2, #1

invoke 1,3,4



L2:
invoke 5,5,0
GOTO_EQ L1,R5,R2
invoke 3,1,0
ADD R1,R1,R2
GOTO_EQ L3,R1,R3
GOTO L2

L1 :
invoke 3,1,0
invoke 4,2,0   @changer la colour de la grille 
@@@@@@ADD R1,R1,R2  
@@@@@@GOTO_EQ L3,R1,R3
GOTO L2       



L3:
@@@@@@@@@@SETI R5, #0
SETI R1, #0
ADD R0,R0,R2
GOTO_EQ L4 ,R0,R3
GOTO L2

L4:
	STOP
