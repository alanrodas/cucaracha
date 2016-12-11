section .data
lli_format_string db "%lli"

section .text

global main

extern exit, putchar, printf

cuca_f:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  ; End initialization
  mov rsi, [rbp + 16]
  cmp rsi, 0
  je if_else_0
  mov rdi, 100
  mov [rbp - 16], rdi
  mov rdi, 10
  mov [rbp - 8], rdi
  jmp if_end_0
  if_else_0:
  mov rdi, 99
  mov [rbp - 8], rdi
  mov rdi, 999
  mov [rbp - 16], rdi
  if_end_0:
  mov rsi, [rbp - 16]
  mov rbx, [rbp - 8]
  sub rsi, rbx
  mov rdi, lli_format_string
  mov rax, 0
  call printf
  mov rdi, 10
  call putchar
  ; Start deinitialization
  mov rsp, rbp
  pop rbp
  ret
  
cuca_g:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  ; End initialization
  mov rsi, -1
  cmp rsi, 0
  je if_end_1
  mov rdi, 10
  mov [rbp - 8], rdi
  if_end_1:
  mov rsi, [rbp - 8]
  mov rdi, lli_format_string
  mov rax, 0
  call printf
  mov rdi, 10
  call putchar
  ; Start deinitialization
  mov rsp, rbp
  pop rbp
  ret
  
cuca_main:
  push rbp
  mov rbp, rsp
  ; End initialization
  sub rsp, 8
  mov rdi, -1
  mov [rsp], rdi
  call cuca_f
  add rsp, 8
  sub rsp, 8
  mov rdi, 0
  mov [rsp], rdi
  call cuca_f
  add rsp, 8
  call cuca_g
  ; Start deinitialization
  mov rsp, rbp
  pop rbp
  ret
  
main:
  call cuca_main
  mov rdi, 0
  call exit
