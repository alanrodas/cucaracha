section .data
lli_format_string db "%lli"

section .text

global main

extern exit, putchar, printf

cuca_main:
  push rbp
  mov rbp, rsp
  sub rsp, 64
  ; End initialization
  mov rdi, 0 ; 0
  mov [rbp - 8], rdi ; i assignment
  mov rdi, 0 ; 0
  mov [rbp - 24], rdi ; s assignment
  mov rdi, 0 ; 0
  mov [rbp - 32], rdi ; t assignment
  mov rdi, -1 ; true
  mov [rbp - 16], rdi ; j assignment
  while_start_2:
  ; fetch i
  mov rsi, [rbp - 8]
  mov rdi, 1000 ; 1000
  ; Start a numeric comparisson
  mov rdx, -1
  cmp rsi, rdi ; Comparing
  jl cmp_end_0
  mov rdx, 0
  cmp_end_0:
  mov rsi, rdx ; Compared in reg
  cmp rsi, 0 ; while comparison
  je while_end_2
  ; if start
  ; fetch j
  mov rsi, [rbp - 16]
  cmp rsi, 0 ; if comparison
  je if_else_1
  ; if_then
  ; fetch s
  mov rdi, [rbp - 24]
  ; fetch i
  mov rbx, [rbp - 8]
  add rdi, rbx
  mov [rbp - 24], rdi ; s assignment
  jmp if_end_1
  if_else_1:
  ; fetch t
  mov rdi, [rbp - 32]
  ; fetch i
  mov rbx, [rbp - 8]
  add rdi, rbx
  mov [rbp - 32], rdi ; t assignment
  if_end_1:
  ; fetch j
  mov rdi, [rbp - 16]
  not rdi
  mov [rbp - 16], rdi ; j assignment
  ; fetch i
  mov rdi, [rbp - 8]
  mov rbx, 1 ; 1
  add rdi, rbx
  mov [rbp - 8], rdi ; i assignment
  jmp while_start_2
  while_end_2:
  ; fetch s
  mov rsi, [rbp - 24]
  mov rdi, lli_format_string
  mov rax, 0
  call printf
  mov rdi, 10 ; 10
  call putchar
  ; fetch t
  mov rsi, [rbp - 32]
  mov rdi, lli_format_string
  mov rax, 0
  call printf
  mov rdi, 10 ; 10
  call putchar
  ; Start deinitialization
  mov rsp, rbp
  pop rbp
  ret
  
main:
  call cuca_main
  mov rdi, 0
  call exit
