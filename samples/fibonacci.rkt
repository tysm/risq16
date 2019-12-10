#lang risq16

b main

fib:
  subu sp 2
  sw r1 [sp]

  subu sp 2
  sw r2 [sp]

  set r14 sp
  addu r14 6
  lw r1 [r14]

  cond0:
    set r14 0
    sub r14 r1
    bnz r14 cond1

    set r2 0

    b endcond
  cond1:
    set r14 1
    sub r14 r1
    bnz r14 else

    set r2 1

    b endcond
  else:
    sub r1 1

    sub sp 2
    sw r1 [sp]

    c fib
    add sp 4

    set r2 r0

    sub r1 1

    sub sp 2
    sw r1 [sp]

    c fib
    add sp 4

    add r2 r0
  endcond:

  set r0 r2

  lw r2 [sp]
  addu sp 2

  lw r1 [sp]
  addu sp 2

  lw r14 [sp]
  br r14

main:
  input r1
  loop:
    set r14 -1
    sub r14 r1
    brz r14 endloop

    subu sp 2
    sw r1 [sp]

    c fib
    add sp 4

    output r0

    sub r1 1
    b loop
  endloop: