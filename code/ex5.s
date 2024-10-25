@ Type exercise 4 here
SETI R2, #0
SETI R3, #0
SETI R4, #1
SETI R5, #0
SETI R6, #0
SETI R7, #0
SETI R8, #0


INVOKE 1, 0, 1
SET R2, R0
SET R3, R1

SUB R2, R2, R4
SUB R3, R3, R4

LOOP1:
	GOTO_GT EXIT, R5, R2
	SETI R6, #0
 	LOOP2:
		GOTO_GT INNER, R6, R3
        SETI R9, #0
		SETI R7, #0
		INVOKE 3, 5, 6
		INVOKE 5, 7, 0
        
		GOTO_NE RULE_2, R7, R4

		RULE_1:
			INVOKE 4, 8, 0
            GOTO NOT_SET_1
        
        RULE_2:
            INVOKE 5, 7, 1
            GOTO_EQ RULE_3, R7, R4
            INVOKE 5, 7, 2
            GOTO_EQ RULE_3, R7, R4
            INVOKE 5, 7, 3
            GOTO_EQ RULE_3, R7, R4
            INVOKE 5, 7, 4
            GOTO_EQ RULE_3, R7, R4
            INVOKE 5, 7, 5
            GOTO_EQ RULE_3, R7, R4
            INVOKE 5, 7, 6
            GOTO_EQ RULE_3, R7, R4
            INVOKE 5, 7, 7
            GOTO_EQ RULE_3, R7, R4
            INVOKE 5, 7, 8
            GOTO_EQ RULE_3, R7, R4
            GOTO NOT_SET_1
        
        RULE_3:
            INVOKE 4, 4, 0
            GOTO NOT_SET_1

		NOT_SET_1:
			ADD R6, R6, R4
			GOTO LOOP2
	INNER:
		ADD R5, R5, R4
		GOTO LOOP1
EXIT:
	STOP
