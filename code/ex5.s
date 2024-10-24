@ Type exercise 5 here

SETI R0, #0
SETI R1, #0
SETI R2, #1
SETI R7, #0


invoke 1,3,4



L2: @boucle principale
GOTO L6
invoke 3,1,0
ADD R1,R1,R2
GOTO_EQ L3,R1,R3
GOTO L2

L6: @ boucle sur les voisins  et sur la grille meme 
invoke 5,6,0
GOTO_EQ L9,R6,R2
invoke 5,6,7
GOTO_EQ L1,R6,R2
invoke 5,6,6
GOTO_EQ L1,R6,R2
invoke 5,6,5
GOTO_EQ L1,R6,R2
invoke 5,6,4
GOTO_EQ L1,R6,R2
invoke 5,6,3
GOTO_EQ L1,R6,R2
invoke 5,6,2
GOTO_EQ L1,R6,R2
invoke 5,6,1
GOTO_EQ L1,R6,R2
invoke 5,6,8
GOTO_EQ L1,R6,R2
invoke 3,1,0
ADD R1,R1,R2
GOTO_EQ L3,R1,R3
GOTO L2

L9:    @boucle pour changer la grille actuel on 0 s'il egale a  1 
invoke 4,7,0 
invoke 3,1,0
ADD R1,R1,R2
GOTO L2 

L1 :    @boucle pour changer la coulour ses les voisines sont egale 1
invoke 4,2,0 
invoke 3,1,0
ADD R1,R1,R2
GOTO L2       



L3:   @boucle pour parcourir les colones 
SETI R1, #0
ADD R0,R0,R2
GOTO_EQ L4 ,R0,R3
GOTO L2

L4:
	STOP
  
