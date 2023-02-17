nop
addi $t0, $r0, hello_world
lb $r3, $t0
debugDumpState

addi $t1, $zero, 1
sb $t0, $t1
lb $r3, $t0
debugDumpState

hlt

hello_world:
.asciz "Hello World <3";