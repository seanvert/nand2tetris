(* use "vm.sml"; *)

val test1 = writePush Constant (Index 2) 256 = ("@2\nD=A\n@256\nM=D\n@SP\nAM=M+1\n",257)

val test2 = remCommGetTokens "// This file is part of www.nand2tetris.org"
val test3 = remCommGetTokens "// and the book \"The Elements of Computing Systems\""
val test4 = remCommGetTokens "// by Nisan and Schocken, MIT Press."
val test5 = remCommGetTokens "// File name: projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm"
val test6 = remCommGetTokens "// Pushes and adds two constants."
val test7 = remCommGetTokens "push constant 7"
val test8 = remCommGetTokens "push constant 8"
val test9 = remCommGetTokens "add"

val testa2 = operation test2
val testa3 = operation test3
val testa4 = operation test4
val testa5 = operation test5
val testa6 = operation test6
val testa7 = operation test7
val testa8 = operation test8
val testa9 = operation test9

val testb2 = codeWriter testa2
val testb3 = codeWriter testa3
val testb4 = codeWriter testa4
val testb5 = codeWriter testa5
val testb6 = codeWriter testa6
val testb7 = codeWriter testa7
val testb8 = codeWriter testa8
val testb9 = codeWriter testa9

