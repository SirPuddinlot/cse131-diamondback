section .text
global our_code_starts_here
extern snek_error
extern _snek_print

our_code_starts_here:
  push rbp
  mov rbp, rsp
  mov [r15 + 0], rdi
  mov rax, 146
  test rax, 1
  jne error_invalid_argument
  add rax, 2
  jo error_overflow
  test rax, 1
  jne error_invalid_argument
  sub rax, 2
  jo error_overflow
  test rax, 1
  jne error_invalid_argument
  sub rax, 2
  jo error_overflow
  mov rsp, rbp
  pop rbp
  ret

error_overflow:
  mov rdi, 1
  call snek_error
  ret

error_invalid_argument:
  mov rdi, 2
  call snek_error
  ret
