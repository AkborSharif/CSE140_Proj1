.text
		lui	$t0, 0x1001	#address where array length is stored
		ori	$t1, $t0, 0x4	#address where array begins
		
		addiu	$t2, $0, 5	#length of array = 5
		sw	$t2, 0($t0)	#store length of array
		
		addiu	$t3, $0, 0	#i = 0
		addiu	$t2, $0, 0	#value = 0
		
fillLoop:	addiu 	$t2, $t2, 1	#value = value + 1		
		sll	$t4, $t3, 2	#index = i*4
		addiu 	$t3, $t3, 1	#i = i + 1
		addu	$t5, $t1, $t4	#address of array[index]
		sw	$t2, 0($t5)	#array[index] = value
		lw	$t4, 0($t0)	#retrieve length
		bne	$t2, $t4, fillLoop