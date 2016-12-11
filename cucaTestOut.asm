section .data
lli_format_string db "%lli"

section .text

global main

extern exit, putchar, printf

cuca_f:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  mov rdi, 65
  mov [rsp - 8], rdi
  mov rsp, rbp
  pop rbp
  ret
  
cuca_main:
  push rbp
  mov rbp, rsp
  sub rsp, 0
  sub rsp, 0
  call cuca_f
  mov rdi, [rsp - 8]
  add rsp, 0
  call putchar
  mov rdi, 10
  call putchar
  mov rsp, rbp
  pop rbp
  ret
  
main:
  call cuca_main
  mov rdi, 0
  call exit
