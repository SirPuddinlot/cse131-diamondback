section .text
global our_code_starts_here
extern snek_error
our_code_starts_here:
  mov rax, 0
  mov [rsp - 8], rax
  mov rax, 0
  mov [rsp - 16], rax
loop_start_1:
  mov rax, [rsp - 16]
  mov [rsp - 24], rax
  mov rax, 10
  mov rcx, rax
  xor rcx, [rsp - 24]
  test rcx, 1
  jne error_invalid_argument
  cmp rax, [rsp - 24]
  mov rax, 3
  mov rcx, 1
  cmovne rax, rcx
  cmp rax, 1
  je else_3
  mov rax, [rsp - 8]
  jmp loop_end_2
  jmp endif_4
else_3:
  mov rax, [rsp - 16]
  mov [rsp - 24], rax
  mov rax, 2
  mov rcx, rax
  or rcx, [rsp - 24]
  test rcx, 1
  jne error_invalid_argument
  add rax, [rsp - 24]
  jo error_overflow
  mov [rsp - 16], rax
  mov rax, [rsp - 8]
  mov [rsp - 24], rax
  mov rax, [rsp - 16]
  mov rcx, rax
  or rcx, [rsp - 24]
  test rcx, 1
  jne error_invalid_argument
  add rax, [rsp - 24]
  jo error_overflow
  mov [rsp - 8], rax
endif_4:
  jmp loop_start_1
loop_end_2:
  ret

error_overflow:
  mov rdi, 1
  call snek_error
  ret

error_invalid_argument:
  mov rdi, 2
  call snek_error
  ret
