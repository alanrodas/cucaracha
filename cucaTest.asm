section .data
lli_format_string db "%lli"

section .text

global main

extern exit, putchar, printf

cuca_f:
  mov rdi, 104
  call putchar
  mov rdi, 111
  call putchar
  mov rdi, 108
  call putchar
  mov rdi, 97
  call putchar
  mov rdi, 10
  call putchar
  ret
  
cuca_main:
  call cuca_f
  ret
  
main:
  call cuca_main
  mov rdi, 0
  call exit
