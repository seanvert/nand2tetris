// INIT SP AT 256

@256
D=A
@SP
M=D

// PUT CONSTANT 7 IN THE STACK
@7
D=A
@SP
A=M
M=D
@SP
AM=M+1

@7
D=A
@SP
A=M
M=D
@SP
AM=M+1

// sum
// @SP
// A=M
// A=A-1
// D=M
// A=A-1
// M=D+M
// A=A+1

// // xor
// @SP
// A=M
// A=A-1
// D=!M
// A=A-1
// D=D&M
// A=A+1
// A=A+1
// M=D
// A=A-1
// A=A-1
// D=!M
// A=A+1
// D=D&M
// A=A+1
// D=D|M
// A=A-1
// A=A-1
// M=D // sp 258 +2
// @SP
// M=M-1


// eq
// @SP
// A=M
// A=A-1
// D=M
// A=A-1
// D=M-D
// @SP
// M=M-1
// M=M-1
// @TESTE0
// D;JEQ // sub b - a true
// @TESTED
// D;JNE //false
// (TESTE0)
// @SP
// A=M
// M=-1
// @END
// 0;JMP
// (TESTED) //false
// @SP
// A=M
// M=0
// (END)
// @SP
// M=M+1
