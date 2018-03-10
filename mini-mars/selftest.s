.text
main:
_start:
	addiu	$4,$0,40
	addiu	$5,$0,1000	
	addi	$6,$6,1
	addiu	$7,$6,80
	
	addu	$6,$0,$0
	addu	$6,$6,$6
	addu	$7,$6,$5
	subu	$5,$6,$5
	addiu	$6,$6,1
	addu	$7,$0,$0
	addiu	$5,$6,6
	sll	$5,$6,2
	srl	$5,$6,2
	addiu	$5,$0,3
loop:	slt	$5,$6,$5
	beq	$5,$0,branch
	subi	$5,$5,1
	j	loop
branch: addu	$6,$0,$0
	addu	$5,$0,$0
	li      $v0, 0
	jr	$ra
