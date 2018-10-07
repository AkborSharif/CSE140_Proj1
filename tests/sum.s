	.text
main:	addiu $t0, $zero, 1
	addu $t1, $zero, $t0
	addu $t2, $t0, $t1
	addu $t3, $t1, $t2
	addu $t4, $t2, $t3
	addu $t5, $t3, $t4
	addu $t6, $t4, $t5
	addu $t7, $t5, $t6