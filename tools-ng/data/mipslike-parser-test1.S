; for (int i = 0; i < 10; i++) {
; }
loop_init: li $t0, 0
li $t1, 10
loop_body:
loop_epi:
addi $t0, $t0, 1
blt $t0, $t1, loop_body
