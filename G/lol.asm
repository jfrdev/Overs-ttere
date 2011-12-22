	.text	0x00400000
	.globl	main
	la	$28, _heap_
	jal	main
	ori	$2, $0, 10
	syscall
# 
main:
	addi	$29, $29, -8
	sw	$31, 4($29)
	sw	$16, 0($29)
	ori	$2, $0, 10
# was:	ori	_exps__2_, 0, 10
# 	ori	2,_exps__2_,0
	jal	getstring
# was:	jal	getstring, 2
# 	ori	_exps__1_,2,0
# 	ori	2,_exps__1_,0
	jal	putstring
# was:	jal	putstring, 2
# 	ori	0,2,0
	ori	$2, $0, 0
# was:	ori	_return__3_, 0, 0
# 	ori	2,_return__3_,0
	j	main_exit
main_exit:
	lw	$16, 0($29)
	lw	$31, 4($29)
	addi	$29, $29, 8
	jr	$31
putint:
	addi	$29, $29, -8
	sw	$2, 0($29)
	sw	$4, 4($29)
	ori	$4, $2, 0
	ori	$2, $0, 1
	syscall
	ori	$2, $0, 4
	la	$4, _cr_
	syscall
	lw	$2, 0($29)
	lw	$4, 4($29)
	addi	$29, $29, 8
	jr	$31
getint:
	ori	$2, $0, 5
	syscall
	jr	$31
putstring:
	ori	$2, $0, 4
	syscall
	la	$4, _cr_
	syscall
	jr	$31
walloc:
	sll	$2, $2, 2
	sub	$28, $28, $2
	ori	$2, $28, 0
	jr	$31
balloc:
	sub	$28, $28, $4
	ori	$2, $28, 0
	jr	$31
getstring:
	sub	$28, $28, $4
	ori	$5, $4, 0
	ori	$4, $28, 0
	ori	$2, $0, 8
	syscall
	ori	$2, $28, 0
	jr	$31
	.data	
	.align	2
_cr_:
	.asciiz	"\n"
	.align	2
_heap_:
	.space	100000
