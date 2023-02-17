nop
addi $t0, $r0, hello_world
lw $r3, $t0
debugDumpState
hlt

hello_world:
.asciz "Hello World <3";