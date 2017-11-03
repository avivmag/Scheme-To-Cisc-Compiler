GCD:
            PUSH(FP);
            MOV(FP,SP);
            
            //CMP(FPARG(1), 3);
            //JUMP_NE(L_error_args_count);
            MOV(R1, FPARG(0));
            //CMP(INDD(R1,0), T_INTEGER);
            //JUMP_NE(L_error_type_mismatch);
            MOV(R2, FPARG(1));
            //CMP(INDD(R2,1), 0);
            //JUMP_EQ(L_second_arg_cannot_be_zero);
            MOV(R1, INDD(R1,1));
            MOV(R2, INDD(R2,1));
            
            GCD_loop:
            MOV(R3, R1);
            REM(R3, R2);
            CMP(R3, 0);
            JUMP_EQ(GCD_end);
            MOV(R1, R2);
            MOV(R2, R3);
            JUMP(GCD_loop);
            
            GCD_end:
            PUSH(R2);
            CALL(MAKE_SOB_INTEGER);
            DROP(1);
            
            POP(FP);
            RETURN;
