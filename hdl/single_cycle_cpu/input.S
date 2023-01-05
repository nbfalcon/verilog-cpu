addi r1, r0, $10
addi r2, r0, $20
add r3, r1, r2

j :skip
addi r3, r3, $40
skip:

debugDumpState
# We should see 30 here

; for (i = 0; i < 10; ++i);
addi r1, r0, $0
addi r2, r0, $10
loop:
addi r1, r1, $1
blt r1, r2, :loop

debugDumpState

hello_world:
.asciz "Hello, World <3"

hlt
