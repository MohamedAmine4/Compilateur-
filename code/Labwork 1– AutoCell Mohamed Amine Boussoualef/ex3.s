SETI R0, #0        
SETI R1, #0       
SETI R2, #1        

invoke 1,3,4      


L4:               
SETI R1, #0       

L5:               
invoke 3, 1, 0   
invoke 4, 2, 0     

ADD R1, R1, R2    
GOTO_LT L5,R1, R3  

ADD R0, R0, R2     
GOTO_LT L4, R0, R4  


STOP            
