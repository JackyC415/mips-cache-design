.text
main:
_start:
	addiu	$t0,$0,4
	addiu	$t0,$t0,100000	
	addiu	$t1,$t1,-1
	addiu	$t1,$t1,0x7fff
	
	addu	$t1,$0,$0
	addu	$t1,$t1,$t0
	addu	$t1,$t1,$t0
	subu	$t1,$t1,$t0
	addiu	$t1,$t1,5
	addu	$t1,$0,$0
	addiu	$t1,$t1,16
	sll	$t0,$t1,2
	srl	$t0,$t1,4
	addiu	$t0,$0,3
loop:	slt	$t2,$t1,$t0
	beq	$t2,$0,go
	addiu	$t0,$t0,-1
	j	loop
	
go:
	addu	$t1,$0,$0
	addu	$t2,$0,$0
	jr	$ra
	
	






	
