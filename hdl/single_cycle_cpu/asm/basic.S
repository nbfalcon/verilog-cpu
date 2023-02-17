addi $t1, $r0, 11
addi $r3, $t1, 10
debugDumpState
jmp skipMe
addi $r3, $r0, 11
skipMe:
debugDumpState
hlt
