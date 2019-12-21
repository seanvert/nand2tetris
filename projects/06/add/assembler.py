import sys
import re

#asmfile = sys.argv[1]

asmfile = "/home/sean/nand2tetris/projects/06/add/Add.asm"
binaryfile = asmfile[:-4]

symbols = {}

class Parser(object):
    def __init__(self, path):
        self.predef_symbols = {
            "SP" : 0,
            "LCL" : 1,
            "ARG" : 2,
            "THIS" : 3,
            "THAT" : 4,
            "R0" : 0,
            "R1" : 1,
            "R2" : 2,
            "R3" : 3,
            "R4" : 4,
            "R5" : 5,
            "R6" : 6,
            "R7" : 7,
            "R8" : 8,
            "R9" : 9,
            "R10" : 10,
            "R11" : 11,
            "R12" : 12,
            "R13" : 13,
            "R14" : 14,
            "R15" : 15,
            "SCREEN" : 16384,
            "KBD" : 24576
        }

        self.dest = {
            "null" : 0b000,
            "M" : 0b001,
            "D" : 0b010,
            "MD" : 0b011,
            "A" : 0b100,
            "AM" : 0b101,
            "AD" : 0b110,
            "AMD" : 0b111
        }

        self.jump = {
            "null" : 0b000,
            "JGT" : 0b001,
            "JEQ" : 0b010,
            "JGE" : 0b011,
            "JLT" : 0b100,
            "JNE" : 0b101,
            "JLE" : 0b110,
            "JMP" : 0b111
        }

        self.comp = {
            "0" : 0b0101010,
            "1" : 0b0111111,
            "-1" : 0b0111010,
            "D" : 0b0001100,
            "A" : 0b0110000,
            "!D" : 0b0001101,
            "!A" : 0b0110001,
            "-D" : 0b0001111,
            "-A" : 0b0110011,
            "D+1" : 0b0011111,
            "A+1" : 0b0110111,
            "D-1" : 0b0001110,
            "A-1" : 0b0110010,
            "D+A" : 0b0000010,
            "D-A" : 0b0010011,
            "A-D" : 0b0000111,
            "D&A" : 0b0000000,
            "D|A" : 0b0010101,
            "M" : 0b1110000,
            "!M" : 0b1110001,
            "-M" : 0b1110011,
            "M+1" : 0b1110111,
            "M-1" : 0b1110010,
            "D+M" : 0b1000010,
            "D-M" : 0b1010011,
            "M-D" : 0b1000111,
            "D&M" : 0b1000000,
            "M|D" : 0b1010101
        }

with open(asmfile, "r") as asm, open(binaryfile, "w") as output:
    linecounter = 0
    for line in asm:
        if line == "\n":
            pass
        elif line == "1":
            # coloca uma regexp
            pass
        else:
            linecounter+=1
            
        print(line)

