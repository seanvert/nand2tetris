exception Error
exception segmentError
exception logicalError
exception stackopError
exception TODO


datatype segment = Argument 
				 | Local 
				 | Static 
				 | Constant 
				 | This 
				 | That 
				 | Pointer 
				 | Temp 


datatype arithmlogi = Add 
					| Sub 
					| Neg 
					| Eq 
					| Gt 
					| Lt 
					| And 
					| Or 
					| Not 


datatype stackop = Push 
				 | Pop 


datatype index = Index of int


datatype line = Operation of arithmlogi
			  | Memory of stackop * segment * index
			  | Empty


fun removeComments (s : string) =
	case s of
		"\r\n" => NONE
	  | "\n" => NONE
	  | _ => if substring (s, 0, 2) = "//" then NONE else SOME s


fun getTokens s =
	case s of
		NONE => []
	  | SOME s  => String.tokens (fn x => x = #" ") s 


val remCommGetTokens = getTokens o removeComments
								
fun logicalIdentifier s =
	let
		(* val _ = print s *)
	in
	case s of
		"add\r\n" => Add
	  | "sub\r\n" => Sub
	  | "neg\r\n" => Neg
	  | "eq\r\n" => Eq
	  | "gt\r\n" => Gt
	  | "lt\r\n" => Lt
	  | "and\r\n" => And
	  | "or\r\n" => Or
	  | "not\r\n" => Not
	  | _ => raise logicalError
	end


fun segmentIdentifier s =
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


fun memOperations (q, w, e) =
	let
		val SOME i = Int.fromString e
		val s = segmentIdentifier w
	in
		case q of
			"push" => (Push, s, Index i)
		  | "pop" => (Pop, s, Index i)
		  | _ => raise stackopError
	end


fun operation (p : string list) =
	case p of
		(p1::[]) => Operation (logicalIdentifier p1)
	  | (p1::p2::p3::[]) => Memory (memOperations (p1, p2, p3))
	  | _ => Empty

val getOperationsFromTokens = operation

(* TODO  *)
fun writePush seg (Index i)  =
	let
		val n = Int.toString i
		fun aux seg i = "@" ^ seg ^ "\n\
		\D=M\n\
		\@" ^ i ^ "\n\
		\A=D+A\n\
		\D=M\n\
		\@SP\n\
		\A=M-1\n\
		\M=D\n"
	in
	case seg of
		Constant => "@" ^ n ^ "\n\
		\D=A\n\
		\@SP\n\
		\A=M\n\
		\M=D\n\
		\@SP\n\
		\AM=M+1\n"
	  | Argument => aux "ARG" n
	  | Local => aux "LCL" n
	  | Static => raise TODO
	  | This => aux "THIS" n
	  | That => aux "THAT" n
	  | Pointer => raise TODO
	  | Temp => raise TODO
	end


fun writePop seg (Index i) =
	let
		val n = Int.toString i
		fun aux seg i stackAddress = "@" ^ seg ^ "\n\
		\D=M\n\
		\@" ^ n ^ "\n\
		\D=D+A\n\
		\@" ^ seg ^ n ^ "\n\
		\M=D\n\
		\@SP\n\
		\AM=M-1\n\
		\D=M\n\
		\@" ^ seg ^ n ^ "\n\
		\M=D\n"
	in
	case seg of
		Argument => aux "ARG" n 0
	  | Local => aux "LCL" n 0
	  | Static => raise TODO
	  | Constant => raise Error
	  | This => aux "THIS" n 0
	  | That => aux "THAT" n 0
	  | Pointer => raise TODO
	  | Temp => raise TODO
	end
		

											  
(* TODO				   *)
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

		(* consertar o nome e arrumar  *)
		fun auxD s = "@SP\n\
		\AM=M-1\n\
		\D=M\n\
		\A=A-1\n" ^ s ^ "\n\
		\A=A+1\n"

		fun auxC j1 j2 n = "@SP\n\
		\A=M\n\
		\A=A-1\n\
		\D=M\n\
		\A=A-1\n\
		\D=M-D\n\
		\@SP\n\
		\M=M-1\n\
		\M=M-1\n\
		\@TESTE" ^ Int.toString n ^ "A\n\
		\D;" ^ j1 ^ "\n\
		\@TESTE" ^ Int.toString n ^ "B\n\
		\D;" ^ j2 ^ "\n\
		\(TESTE" ^ Int.toString n ^ "A)\n\
		\@SP\n\
		\A=M\n\
		\M=-1\n\
		\@END" ^ Int.toString n ^ "\n\
		\0;JMP\n\
		\(TESTE" ^ Int.toString n ^ "B)\n\
		\@SP\n\
		\A=M\n\
		\M=0\n\
		\(END" ^ Int.toString n ^ ")\n\
		\@SP\n\
		\M=M+1\n"
	in
	case operation of
		Add => auxD "M=D+M"
	  (* aqui não tenho certeza de como funciona a subtração da vm *)
	  (* do jeito que está fica o segundo menos o primeiro da pilha *)
	  | Sub => auxD "M=M-D"
	  | And => auxD "M=M&D"
	  | Or => auxD "M=M|D"
	  (* arruma esse n depois  *)
	  | Eq => auxC "JEQ" "JNE" n
	  | Gt => auxC "JGT" "JLE" n
	  | Lt => auxC "JLT" "JGE" n
	  | Not => auxU "!M"
	  | Neg => auxU "-M"
	end

(* todo 		 *)
fun codeWriter line n =
	case line of
		Operation f => writeLogArith f n
	  | Memory s => writeStackMemOp s
	  | Empty => "\n"

val getOperation = operation o remCommGetTokens

fun getLineWriteCode s n = codeWriter (getOperation s) n
								   
fun f s n =
	let
		val x = getLineWriteCode s n
	in
		x
	end
		


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
			  | SOME s => (TextIO.output(outstream, (f s n)); aux (TextIO.inputLine instream) (n + 1))
			end
	in
		aux readline 0
	end

val args = CommandLine.arguments()
val filename = hd (String.tokens (fn x => x = #".") (hd args))

val _ = readfile ((hd args), filename ^ ".asm")
val _ = OS.Process.exit(OS.Process.success)
