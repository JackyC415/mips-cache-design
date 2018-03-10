.text
main:
_start:
	addiu	$5,$0,4
	addiu	$5,$5,100	
	addiu	$6,$6,-1
	addiu	$6,$6,8
	
	addu	$6,$0,$0
	addu	$6,$6,$5
	addu	$6,$6,$5
	subu	$6,$6,$5
	addiu	$6,$6,5
	addu	$6,$0,$0
	addiu	$6,$6,16
	sll	$5,$6,2
	srl	$5,$6,4
	addiu	$5,$0,3
loop:	slt	$7,$6,$5
	beq	$7,$0,branched
	addiu	$5,$5,-1
	j	loop
	
branched:
	addu	$6,$0,$0
	addu	$7,$0,$0
	jr	$ra
	
	






	
