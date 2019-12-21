// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

(KEYBOARD)
	@KBD
	D=M
	@WHITE
	D;JEQ
	@KBD
	D=M
	@BLACK
	D;JNE
@KEYBOARD
0;JMP
// loop through the screen and put pixels black
(BLACK)
@i
M=0
(LOOPB) // parte que enche a tela
	@i
	D=M
	@SCREEN
	A=A+D
	M=-1
	@i
	M=M+1 
	D=M
	@8192 // checa se já encheu a tela
	D=D-A
	@KEYBOARD	//	@END 
	D;JEQ
@LOOPB
0;JMP
// loop through the screen and put pixels white
(WHITE)
@i
M=0
(LOOPW)
	@i
	D=M
	@SCREEN
	A=A+D
	M=0
	@i
	M=M+1
	D=M
	@8192
	D=D-A
	@KEYBOARD // AJEITA
	D;JEQ
@LOOPW
0;JMP

(END)
@END
0;JMP
