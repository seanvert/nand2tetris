import sys
import re

predef_symbols = {
    "SP": 0,
    "LCL": 1,
    "ARG": 2,
    "THIS": 3,
    "THAT": 4,
    "R0": 0,
    "R1": 1,
    "R2": 2,
    "R3": 3,
    "R4": 4,
    "R5": 5,
    "R6": 6,
    "R7": 7,
    "R8": 8,
    "R9": 9,
    "R10": 10,
    "R11": 11,
    "R12": 12,
    "R13": 13,
    "R14": 14,
    "R15": 15,
    "SCREEN": 16384,
    "KBD": 24576
}

dest = {
    "null": "000",
    "M": "001",
    "D": "010",
    "MD": "011",
    "A": "100",
    "AM": "101",
    "AD": "110",
    "AMD": "111"
}

jump = {
    "null": "000",
    "JGT": "001",
    "JEQ": "010",
    "JGE": "011",
    "JLT": "100",
    "JNE": "101",
    "JLE": "110",
    "JMP": "111"
}

comp = {
    "0": "0101010",
    "1": "0111111",
    "-1": "0111010",
    "D": "0001100",
    "A": "0110000",
    "!D": "0001101",
    "!A": "0110001",
    "-D": "0001111",
    "-A": "0110011",
    "D+1": "0011111",
    "A+1": "0110111",
    "D-1": "0001110",
    "A-1": "0110010",
    "D+A": "0000010",
    "D-A": "0010011",
    "A-D": "0000111",
    "D&A": "0000000",
    "D|A": "0010101",
    "M": "1110000",
    "!M": "1110001",
    "-M": "1110011",
    "M+1": "1110111",
    "M-1": "1110010",
    "D+M": "1000010",
    "D-M": "1010011",
    "M-D": "1000111",
    "D&M": "1000000",
    "D|M": "1010101"
}


class Parser(object):
    def __init__(self, path):
        self.asm = open(path, "r")
        self.current_line = ''
        self.end = False
        # regular expessions
        self.Commentreg = re.compile(r'^\/\/')
        self.Lreg = re.compile(r'\([A-z0-9_.$:]+\)')
        self.Areg = re.compile(r'@[A-z0-9_.$:]+')
        self.Creg = re.compile(r'^[A-Z]{0,3}(=)?[|&!\-+A-Z0-9]{1,3}(;)?(J[A-Z]{2})?')
        self.Destreg = re.compile(r'^[A-Z]{0,3}=')
        self.Jumpreg = re.compile(r';[A-Z]{3}')
        self.Compreg = re.compile(r'[=;]|//')

    def hasMoreCommands(self):
        if self.end:
            return False
        else:
            return self.asm.readable()

    def advance(self):
        self.current_line = self.asm.readline()
        if self.current_line != "\n":
            self.current_line = self.current_line.lstrip()

        if self.current_line == '':
            self.end = True

    def commandType(self):
        if self.Commentreg.match(self.current_line):
            return None
        if self.Areg.match(self.current_line):
            return "A_COMMAND"
        elif self.Lreg.match(self.current_line):
            return "L_COMMAND"
        elif self.Creg.match(self.current_line):
            return "C_COMMAND"
        else:
            pass

    def symbol(self):
        if self.commandType() == "A_COMMAND":
            return self.Areg.match(self.current_line).group(0)[1:]
        else:
            return self.Lreg.match(self.current_line).group(0)[1:-1]

    def dest(self):
        try:
            return self.Destreg.match(self.current_line).group(0)[:-1]
        except:
            return None

    def comp(self):
        self.compi = self.Compreg.split(self.current_line)
        if self.dest():
            return self.compi[1].rstrip()
        elif self.jump():
            return self.compi[0].rstrip()
        else:
            return self.compi[0].rstrip()

    def jump(self):
        try:
            return self.Jumpreg.search(self.current_line).group(0)[1:].strip()
        except:
            return None


class Code(object):
    def __init__(self, destdic=dest, compdic=comp, jumpdic=jump):
        self.destdic = destdic
        self.compdic = compdic
        self.jumpdic = jumpdic

    def dest(self, mnemonic):
        try:
            return self.destdic[mnemonic]
        except:
            return None

    def comp(self, mnemonic):
        try:
            return self.compdic[mnemonic]
        except:
            return None

    def jump(self, mnemonic):
        try:
            return self.jumpdic[mnemonic]
        except:
            return None


class SymbolTable(object):
    def __init__(self, table=predef_symbols):
        self.symbols = table

    def addEntry(self, symbol, address):
        self.symbols[symbol] = address

    def contains(self, symbol):
        return symbol in self.symbols.keys()

    def GetAddress(self, symbol):
#        print(type(self.symbols[symbol]))
        return self.symbols[symbol]


def main():
    c = Code()
    p = Parser(sys.argv[1])
    bina = open(re.split(r'\.', sys.argv[1])[0] + ".hack", "w")
    s = SymbolTable()
    ram = []
    line_counter = 0
    while p.hasMoreCommands():
        if p.commandType() == "L_COMMAND":
            if s.contains(p.symbol()):
                pass
            else:
                s.addEntry(p.symbol(), line_counter)
        elif p.commandType() == "A_COMMAND" or p.commandType() == "C_COMMAND":
            line_counter += 1
        p.advance()

    p.asm.seek(0)
    p.end = False
    print(s.symbols)
    ram_counter = 16
    while p.hasMoreCommands():
        if p.commandType() == "A_COMMAND" or p.commandType == "L_COMMAND":
            try:
                b = bin(int(p.symbol()))[2:]
            except:
                try:
                    b = bin(s.GetAddress(p.symbol()))[2:]
                except:
                    s.addEntry(p.symbol(), ram_counter)
                    b = bin(ram_counter)[2:]
                    ram_counter += 1
            b = (16 - len(b))*"0" + b + "\n"
            bina.write(b)
        elif p.commandType() == "C_COMMAND":
#            print(p.current_line, p.comp(), len(p.comp()))
            com = c.comp(p.comp())
            try:
                des = c.dest(p.dest()) + ''
            except:
                des = "000"
            try:
                jmp = c.jump(p.jump()) + ''
            except:
                jmp = "000"
            bina.write("111" + com + des + jmp + "\n")
        p.advance()


if __name__ == '__main__':
    main()
