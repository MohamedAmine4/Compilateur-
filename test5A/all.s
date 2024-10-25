	.meta source "\"test5A/all.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 5, 3
	seti r6, #1
	goto_eq L6, r5, r6
	goto L5
L6:
	invoke 5, 7, 7
	seti r8, #1
	goto_eq L6, r7, r8
	goto L5
L5:
	invoke 5, 9, 1
	seti r10, #1
	goto_eq L7, r9, r10
	goto L3
L7:
	invoke 5, 11, 5
	seti r12, #0
	goto_eq L3, r11, r12
	goto L7
L2:
	seti r13, #1
	invoke 4, 13, 0
	goto L4
L3:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
