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

; loop header
addi r1, r0, :hello_world
addi r3, r0, $0

print_loop:

add r4, r1, r3
lb r2, r4
beq r2, r0, :exit_loop

debugDumpState
addi r3, r3, $1

j :print_loop

exit_loop:

; copy_loop
addi r1, r0, $0 ; for i = 0;
addi r3, r0, :hello_world ; s = "Hello..."
addi r5, r0, :free_memory ; out = .bss

copy_loop:
add r4, r3, r1 ; &s[i]
add r6, r5, r1 ; &out[i]

lb r2, r4 ; r2 = r4
addi r7, r2, $1 ; s[i] + 1
sb r6, r7 ; out[i] = s[i] + 1

addi r1, r1, $1
bneq r2, r0, :copy_loop ; once we've copied the null byte, we can stop

; loop header
addi r1, r0, :free_memory
addi r3, r0, $0
print_loop2:
add r4, r1, r3
lb r2, r4
beq r2, r0, :exit_loop2

debugDumpState
addi r3, r3, $1

j :print_loop2
exit_loop2:

hlt

hello_world:
.asciz "Hello, World <3"

free_memory:
; we can write to this region of ram
