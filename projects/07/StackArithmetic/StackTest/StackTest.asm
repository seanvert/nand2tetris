






@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.9A
D;JEQ
@StackTest.9B
D;JNE
(StackTest.9A)
@SP
A=M
M=-1
@StackTest.9
0;JMP
(StackTest.9B)
@SP
A=M
M=0
(StackTest.9)
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.12A
D;JEQ
@StackTest.12B
D;JNE
(StackTest.12A)
@SP
A=M
M=-1
@StackTest.12
0;JMP
(StackTest.12B)
@SP
A=M
M=0
(StackTest.12)
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.15A
D;JEQ
@StackTest.15B
D;JNE
(StackTest.15A)
@SP
A=M
M=-1
@StackTest.15
0;JMP
(StackTest.15B)
@SP
A=M
M=0
(StackTest.15)
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.18A
D;JLT
@StackTest.18B
D;JGE
(StackTest.18A)
@SP
A=M
M=-1
@StackTest.18
0;JMP
(StackTest.18B)
@SP
A=M
M=0
(StackTest.18)
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.21A
D;JLT
@StackTest.21B
D;JGE
(StackTest.21A)
@SP
A=M
M=-1
@StackTest.21
0;JMP
(StackTest.21B)
@SP
A=M
M=0
(StackTest.21)
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.24A
D;JLT
@StackTest.24B
D;JGE
(StackTest.24A)
@SP
A=M
M=-1
@StackTest.24
0;JMP
(StackTest.24B)
@SP
A=M
M=0
(StackTest.24)
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.27A
D;JGT
@StackTest.27B
D;JLE
(StackTest.27A)
@SP
A=M
M=-1
@StackTest.27
0;JMP
(StackTest.27B)
@SP
A=M
M=0
(StackTest.27)
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.30A
D;JGT
@StackTest.30B
D;JLE
(StackTest.30A)
@SP
A=M
M=-1
@StackTest.30
0;JMP
(StackTest.30B)
@SP
A=M
M=0
(StackTest.30)
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M
A=A-1
D=M
A=A-1
D=M-D
@SP
M=M-1
M=M-1
@StackTest.33A
D;JGT
@StackTest.33B
D;JLE
(StackTest.33A)
@SP
A=M
M=-1
@StackTest.33
0;JMP
(StackTest.33B)
@SP
A=M
M=0
(StackTest.33)
@SP
M=M+1
@57
D=A
@SP
A=M
M=D
@SP
M=M+1
@31
D=A
@SP
A=M
M=D
@SP
M=M+1
@53
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=D+M
@112
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=M-D
@SP
A=M
A=A-1
M=-M
@SP
AM=M-1
D=M
A=A-1
M=M&D
@82
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
AM=M-1
D=M
A=A-1
M=M|D
@SP
A=M
A=A-1
M=!M
