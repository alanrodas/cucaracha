%define SYSCALL_WRITE 0x2000004
%define SYSCALL_EXIT  0x2000001

global start

extern _putchar

start:

  mov rdi, 1
  mov rsi, str
  mov rdx, 10
  mov rax, SYSCALL_WRITE
  syscall

  mov rax, SYSCALL_EXIT
  mov rdi, 1
  syscall

section .data
str:
  db `Hello, assembly!\n` ; to use escape sequences, use backticks
strlen equ 18
