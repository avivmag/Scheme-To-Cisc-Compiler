/* abs.asm
 * Computes the absolute value of its argument:
 *   R0 <- | ARG[0] |
 *
 * Programmer: Mayer Goldberg, 2010
 */

 ABS:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);

  MOV(R0, FPARG(0));
  CMP(R0, IMM(0));
  JUMP_LT(L_ABS_N);
  
  POP(R1);
  POP(FP);
  RETURN;
 L_ABS_N:
  MOV(R1, R0);
  MOV(R0, IMM(0));
  SUB(R0, R1);
  POP(R1);
  POP(FP);
  RETURN;
