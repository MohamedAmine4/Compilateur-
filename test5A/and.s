	.meta source "\"test5A/and.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 10, 0
	seti r11, #0
	goto_eq L9, r10, r11
	goto L7
L9:
	invoke 5, 12, 5
	seti r13, #1
	goto_eq L9, r12, r13
	goto L7
L6:
	seti r14, #1
	invoke 4, 14, 0
	goto L8
L7:
L8:
	invoke 5, 5, 0
	seti r6, #1
	goto_eq L5, r5, r6
	goto L3
L5:
	invoke 5, 7, 5
	seti r8, #0
	goto_eq L5, r7, r8
	goto L3
L2:
	seti r9, #0
	invoke 4, 9, 0
	goto L4
L3:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
