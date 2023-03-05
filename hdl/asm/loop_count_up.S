; for (int i = 0; i < 10; i++)
addi $t0, $zero, 0
addi $t1, $zero, 10

loop_init:
addi $t0, $t0, 1
blt $t0, $t1, loop_init

addi $r3, $t1, 0
debugDumpState
hlt