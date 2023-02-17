; copy_loop
addi $r1, $r0, 1 ; for i = 0;
addi $r3, $r0, hello_world ; s = "Hello..."
addi $r5, $r0, free_memory ; out = .bss

copy_loop:
add $r4, $r3, $r1 ; &s[i]
add $r6, $r5, $r1 ; &out[i]

lb $r2, $r4 ; $r2 = $r4
addi $r7, $r2, 1 ; s[i] + 1
sb $r6, $r7 ; out[i] = s[i] + 1

addi $r1, $r1, 1
bneq $r2, $r0, copy_loop ; once we've copied the null byte, we can stop
sb $r6, $r0 ; actually, one more thing: the NUL byte shouldn't be incremented

; print loop (stage 2)
addi $r1, $r0, free_memory
addi $r3, $r0, 1
print_loop2:
add $r4, $r1, $r3
lb $r2, $r4

debugDumpState
addi $r3, $r3, 1

beq $r2, $r0, exit_loop2

jmp print_loop2
exit_loop2:

hlt

hello_world:
.asciz "Hello, World <3"

free_memory:
; we can write to this region of ram
