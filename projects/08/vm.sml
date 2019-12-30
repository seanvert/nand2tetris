exception labelError
exception pushConstantError
exception segmentError
exception logicalError
exception stackopError
exception invalidPointerValue
exception functionOperationError

datatype stackop = Push
				 | Pop

datatype segment = Argument
				 | Local
				 | Static
				 | Constant
				 | This
				 | That
				 | Pointer
				 | Temp

datatype index = Index of int

datatype labelFlow = Label
				   | Goto
				   | Ifgoto

datatype labelString = LabelName of string

datatype functionName = Name of string

datatype functionArgs = Localargs of int

datatype functionOp = Declaration of functionName * functionArgs
					| Call of functionName * functionArgs
	   | Return

datatype arithmlogi = Add
					| Sub
					| Neg
					| Eq
					| Gt
					| Lt
					| And
					| Or
					| Not

datatype line = Operation of arithmlogi
			  | Memory of stackop * segment * index
			  | Labelop of labelFlow * labelString
			  | FunctionCommand of functionOp
			  | Empty

val args = CommandLine.arguments()
val parsedDirPathArgs = String.tokens (fn x => x = #"/") (hd args)

fun getFilename (p::ph) =
	case ph of
		[] => p
	  | _ => getFilename ph

val filenameExtension = getFilename parsedDirPathArgs
val filename = hd (String.tokens (fn x => x = #".") filenameExtension)
(* placeholder só pra teste  *)
(* val filename = "asd" *)

fun removeComments (s : string) =
	let
		fun getStrFromLine str =
			case String.fromString str of
				NONE => ""
			  | SOME s => s
		val str = getStrFromLine s
	in
	case s of
		"\r\n" => NONE
	  | "\n" => NONE
	  | _ => SOME (hd (String.fields (fn x => x = #"/") str))
	end
(* if substring (s, 0, 2) = "//" then NONE else SOME s *)

fun getTokens s =
	case s of
		NONE => []
	  | SOME s  => String.tokens (fn x => x = #" ") s

val remCommGetTokens = getTokens o removeComments

fun logicalIdentifier s =
	case s of
		"add" => Add
	  | "sub" => Sub
	  | "neg" => Neg
	  | "eq" => Eq
	  | "gt" => Gt
	  | "lt" => Lt
	  | "and" => And
	  | "or" => Or
	  | "not" => Not
	  | _ => raise logicalError

fun segmentIdentifier s =
	let
		val _ = print s
	in
	case s of
		"argument" => Argument (* RAM [2] POINTER *)
	  | "local" => Local (* RAM [1] POINTER *)
	  | "static" => Static (* STARTS AT RAM [16] *)
	  | "constant" => Constant (* VIRTUAL *)
	  | "this" => This (* RAM [3] POINTER *)
	  | "that" => That (* RAM [4] POINTER *)
	  | "pointer" => Pointer
	  | "temp" => Temp (* RAM [5-12] CONTENTS *)
	  | _ => raise segmentError
	end

fun pushOrPop str =
	case str of
		"push" => SOME Push
	  | "pop" => SOME Pop
	  | _ => NONE

fun memOperations (q, w, e) =
	let
		val SOME i = Int.fromString e
		val s = segmentIdentifier w
	in
		case q of
			Push => (Push, s, Index i)
		  | Pop => (Pop, s, Index i)
	end

fun readLabelFlow p1 p2 =
	let
		val _ = print ("LabelFlow: " ^ p1 ^ p2 ^ "\n")
	in
	case p1 of
		"label" => (Label, LabelName p2)
	  | "goto" => (Goto, LabelName p2)
	  | "if-goto" => (Ifgoto, LabelName p2)
	  | _ => raise labelError
	end

fun functionOperations command function kargs =
	let
		val SOME fname = String.fromString function
		val SOME args = Int.fromString kargs
	in
	case command of
		"call" => Call (Name fname, Localargs args)
	  | "function" => Declaration (Name fname, Localargs args)
	  | _ => raise functionOperationError
	end

fun functionReturn str =
	case str of
		"return" => SOME Return
	  | _ => NONE

fun operation (p : string list) =
	case p of
		(p1::[]) => (case functionReturn p1 of
						 SOME Return => FunctionCommand Return
					   | NONE => Operation (logicalIdentifier p1))
	  | (p1::p2::p3::[]) => (case pushOrPop p1 of
								SOME Push => Memory (memOperations (Push, p2, p3))
							  | SOME Pop => Memory (memOperations (Pop, p2, p3))
							  | NONE => FunctionCommand (functionOperations p1 p2 p3))
	  | (p1::p2::[]) => Labelop (readLabelFlow p1 p2)
	  | _ => Empty

val getOperationsFromTokens = operation

fun writeLabelops (label, LabelName str) =
	case label of
		Label => "(" ^ str ^ ")\n"
	  | Goto => "@" ^ str ^ "\n\
	  \0;JMP\n"
	  | Ifgoto => "@SP\n\
	  \AM=M-1\n\
	  \D=M\n\
	  \@" ^ str ^ "\n\
	  \D;JNE\n"

fun writePush seg (Index i)  =
	let
		val putDRegisterInTheStack = "@SP\n\
		\A=M\n\
		\M=D\n\
		\@SP\n\
		\M=M+1\n"

		val n = Int.toString i

		fun aux seg index = "@" ^ seg ^ "\n\
		\D=M\n\
		\@" ^ index ^ "\n\
		\A=D+A\n\
		\D=M\n" ^ putDRegisterInTheStack

		fun auxPointer seg = "@" ^ seg ^ "\n\
		\D=M\n" ^ putDRegisterInTheStack

		fun auxStaticTemp n x = "@" ^ Int.toString (n + x) ^ "\n\
		\D=M\n" ^ putDRegisterInTheStack
	in
	case seg of
		Constant => "@" ^ n ^ "\n\
		\D=A\n" ^ putDRegisterInTheStack
	  | Argument => aux "ARG" n
	  | Local => aux "LCL" n
	  | Static => auxStaticTemp i 16
	  | This => aux "THIS" n
	  | That => aux "THAT" n
	  | Pointer => (case i of
					   0 => auxPointer "THIS"
					 | 1 => auxPointer "THAT"
					 | _ => raise invalidPointerValue)
	  | Temp => auxStaticTemp i 5
	end

fun writePop seg (Index i) =
	let
		val stackValueIntoDRegister = "@SP\n\
		\AM=M-1\n\
		\D=M\n"

		val n = Int.toString i

		fun aux seg index = "@" ^ seg ^ "\n\
		\D=M\n\
		\@" ^ index ^ "\n\
		\D=D+A\n\
		\@" ^ seg ^ index ^ "\n\
		\M=D\n" ^ stackValueIntoDRegister ^
		"@" ^ seg ^ index ^ "\n\
		\A=M\n\
		\M=D\n"

		fun auxPointer seg = stackValueIntoDRegister ^
		"@" ^ seg ^ "\n\
		\M=D\n"

		fun auxStaticTemp n x = stackValueIntoDRegister ^
		"@" ^ Int.toString (n + x) ^ "\n\
		\M=D\n"
	in
	case seg of
		Argument => aux "ARG" n
	  | Local => aux "LCL" n
	  | Static => auxStaticTemp i 16
	  | Constant => raise pushConstantError
	  | This => aux "THIS" n
	  | That => aux "THAT" n
	  | Pointer => (case i of
					   0 => auxPointer "THIS"
					 | 1 => auxPointer "THAT"
					 | _ => raise invalidPointerValue)
	  | Temp => auxStaticTemp i 5
	end

fun writeStackMemOp s =
	case s of
		(Push, seg, ind) => writePush seg ind
	  | (Pop, seg, ind) => writePop seg ind

(* n é o número de linhas no código										  *)
fun writeLogArith operation n =
	let
		fun auxU s = "@SP\n\
		\A=M\n\
		\A=A-1\n\
		\M="^ s ^"\n"

		fun auxD s = "@SP\n\
		\AM=M-1\n\
		\D=M\n\
		\A=A-1\n" ^ s ^ "\n"

		fun auxC j1 j2 n =
			let
				val k = Int.toString n
			in
		"@SP\n\
		\A=M\n\
		\A=A-1\n\
		\D=M\n\
		\A=A-1\n\
		\D=M-D\n\
		\@SP\n\
		\M=M-1\n\
		\M=M-1\n\
		\@" ^ filename ^ "." ^ k ^ "A\n\
		\D;" ^ j1 ^ "\n\
		\@" ^ filename ^ "." ^ k ^ "B\n\
		\D;" ^ j2 ^ "\n\
		\(" ^ filename ^ "." ^ k ^ "A)\n\
		\@SP\n\
		\A=M\n\
		\M=-1\n\
		\@" ^ filename ^ "." ^ k ^ "\n\
		\0;JMP\n\
		\(" ^ filename ^ "." ^ k ^ "B)\n\
		\@SP\n\
		\A=M\n\
		\M=0\n\
		\(" ^ filename ^ "." ^ k ^ ")\n\
		\@SP\n\
		\M=M+1\n"
			end
	in
	case operation of
		Add => auxD "M=D+M"
	  | Sub => auxD "M=M-D"
	  | And => auxD "M=M&D"
	  | Or => auxD "M=M|D"
	  | Eq => auxC "JEQ" "JNE" n
	  | Gt => auxC "JGT" "JLE" n
	  | Lt => auxC "JLT" "JGE" n
	  | Not => auxU "!M"
	  | Neg => auxU "-M"
	end

fun writeFunctionOps fop =
	let
		val putDRegisterInTheStack = "@SP\n\
		\A=M\n\
		\M=D\n\
		\@SP\n\
		\M=M+1\n"

		val stackValueIntoDRegister = "@SP\n\
		\AM=M-1\n\
		\D=M\n"

		fun pushFunctionStack seg =
			"@" ^ seg ^ "\n\
			\D=M\n" ^ putDRegisterInTheStack 
										 
		fun initializeArgs n = writeStackMemOp (Push, Constant, Index 0) ^ 
							   (writeStackMemOp (Pop, Local, Index n))

		val concatenateList = foldr (fn (x, y) => x ^ y) ""
		(* TODO 		    *)
		val returnAddress = "placeholder"

		fun restoreStack (seg, k) = "@FRAME\n\
		\D=M\n\
		\@" ^ k ^ "\n\
		\D=D-A\n\
		\@" ^ seg ^ "\n\
		\M=D\n"

		val functionStack = [returnAddress, "LCL", "ARG", "THIS", "THAT"]

		val restoreSegments = rev (tl functionStack)

		val restoreOffset = ["1", "2", "3", "4"]

		val restorePairs = ListPair.zip (restoreSegments, restoreOffset)
	in
	case fop of
		Declaration (Name fname, Localargs k) => "(" ^ fname ^ ")\n\
		\" ^ concatenateList (List.tabulate (k, initializeArgs))
	  | Call (Name fname, Localargs k) => concatenateList (map pushFunctionStack functionStack) ^ 
	  "@SP\n\
	  \D=M\n\
	  \@5\n\
	  \D=D-A\n\
	  \@" ^ Int.toString k ^ "\n\
	  \D=D-A\n\
	  \@ARG\n\
	  \M=D\n\
	  \@SP\n\
	  \D=M\n\
	  \@LCL\n\
	  \M=D\n\
	  \0;JMP\n\
	  \(" ^ returnAddress ^ ")\n"
	  | Return => "@LCL\n\
	  \D=A\n\
	  \@FRAME\n\
	  \M=D\n\
	  \@5\n\
	  \D=D-A\n\
	  \@RET\n\
	  \M=D\n" ^ stackValueIntoDRegister ^ "@ARG\n\
	  \M=D\n" ^ "@ARG\n\
	  \D=M+1\n\
	  \@SP\n\
	  \M=D\n" ^ concatenateList (map restoreStack restorePairs)  ^
	  "@RET\n\
	  \0;JMP\n"
	end

fun codeWriter line n =
	case line of
		Operation f => writeLogArith f n
	  | Memory s => writeStackMemOp s
	  | Labelop lop => writeLabelops lop
	  | FunctionCommand fop => writeFunctionOps fop
	  | Empty => "\n"

val getOperation = operation o remCommGetTokens

fun getLineWriteCode s n = codeWriter (getOperation s) n

fun readfile (input, output) =
	let
		val instream = TextIO.openIn input
		val outstream = TextIO.openOut output
		val readline = TextIO.inputLine instream
		fun aux readline n =
			let
				(* val _ = print (Int.toString n) *)
			in
			case readline of
				NONE => (TextIO.closeIn instream; TextIO.closeOut outstream)
			  | SOME s => (TextIO.output(outstream, (getLineWriteCode s n)); aux (TextIO.inputLine instream) (n + 1))
			end
	in
		aux readline 0
	end

val filePath = hd (String.tokens (fn x => x = #".") (hd args))
(* val _ print hd parsedDirPathArgs *)
val _ = readfile ((hd args), filePath ^ ".asm")
val _ = OS.Process.exit(OS.Process.success)
