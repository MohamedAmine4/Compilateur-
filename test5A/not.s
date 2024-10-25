	.meta source "\"test5A/not.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 5, 0
	seti r6, #1
	goto_eq L3, r5, r6
	goto L2
L2:
	seti r7, #1
	invoke 4, 7, 0
	goto L4
L3:
	seti r8, #0
	invoke 4, 8, 0
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
