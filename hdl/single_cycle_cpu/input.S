addi r1, r0, $10
addi r2, r0, $20
add r3, r1, r2
j :skip
addi r3, r3, $40
skip:
hlt
