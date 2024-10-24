	.meta source "\"autos/ifelse.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 19, 6
	invoke 4, 5, 19
	invoke 5, 18, 2
	invoke 4, 6, 18
	goto_eq L11, r5, r6
	goto L12
L11:
	seti r16, #1
	invoke 4, 16, 0
	goto L13
L12:
	seti r17, #0
	invoke 4, 17, 0
L13:
	add r7, r5, r6
	seti r8, #2
	goto_ne L2, r7, r8
	goto L3
L2:
	invoke 5, 9, 3
	invoke 5, 10, 5
	goto_lt L5, r9, r10
	goto L6
L5:
	seti r11, #2
	invoke 4, 11, 0
	goto L7
L6:
	seti r12, #0
	invoke 4, 12, 0
L7:
	goto L4
L3:
	invoke 5, 13, 0
	seti r14, #0
	goto_eq L8, r13, r14
	goto L9
L8:
	seti r15, #1
	invoke 4, 15, 0
	goto L10
L9:
L10:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
