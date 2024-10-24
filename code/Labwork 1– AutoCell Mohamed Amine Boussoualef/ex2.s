
SETI R0 ,#0
SETI R1, #0
SETI R2, #1

invoke 1,3,4
SUB R3,R3,R2
SUB R4,R4,R2
invoke 3,3,4
invoke 4,2,0    @ 9_9

invoke 3,0,4   @ 0_9
invoke 4,2,0

invoke 3,3,0   @ 9_0
invoke 4,2,0

invoke 3,0,0   @ 0_0
invoke 4,2,0
STOP
