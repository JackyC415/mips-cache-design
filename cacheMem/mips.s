main:
  jal foo
foo:
  jr $ra
    lui $8, 0x1234
  ori $8, 0xabcd
  sw $8, 0($sp)
  lw $9, 0($sp)

  addi $8, $0, -1
  ori $9, $0, 0x2
  sllv $10, $8, $9
  srlv $11, $8, $9
  srav $12, $8, $9
label:	  beq $0, $0, label

  jr $ra


foo1:
  addi $8, $8, 4
  jr $ra