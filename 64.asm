%define SYSCALL_EXIT  0x2000001

section .text

global _main

extern _exit

_main:
  ;mov rdi, 72
  ;call _putchar

  ;mov rdi, 79
  ;call _putchar

  ;mov rdi, 76
  ;call _putchar

  ;mov rdi, 65
  ;call _putchar

  ;mov rdi, 10
  ;call _putchar

  ;mov rax, SYSCALL_EXIT
  ;mov rdi, 0
  call _exit

section .data
str: db `Hello, assembly!\n` ; to use escape sequences, use backticks
