module CodeGen.AssemblyFormatters.Nasm(nasmFormatter) where

import CodeGen.Assembly
import CodeGen.AssemblyFormatters


nasmFormatter :: AssemblyFormatter
nasmFormatter = AssemblyFormatter { formatterName = "NASM"
                                  , formatterBoilerplate = boilerplate
                                  , formatterConverter = convert
                                  }


boilerplate :: String
boilerplate = "global main\n"
           ++ "extern printInt\n"
           ++ "extern __alloc_array\n"
           ++ "extern error\n"
           ++ "section .text\n"


convert :: Instruction -> String
convert (I0 o)       = opToString o
convert (I1 o a)     = opToString o ++ " " ++ argumentToString a
convert (I2 Lea a1 (ArgumentAddress Size64 a2))
                     = opToString Lea ++ " " ++ argumentToString a1
                     ++ ", [" ++ argumentToString a2 ++ "]"
convert (I2 o a1 a2) = opToString o ++ " " ++ argumentToString a1 ++ ", " ++ argumentToString a2


argumentToString :: Argument -> String
argumentToString (ArgumentRegister r)       = registerToString r
argumentToString (Argument8 i)              = show i
argumentToString (Argument16 i)             = show i
argumentToString (Argument32 i)             = show i
argumentToString (Argument64 i)             = show i
argumentToString (ArgumentAddress Size8 a)  = "BYTE [" ++ argumentToString a ++ "]"
argumentToString (ArgumentAddress Size16 a) = "WORD [" ++ argumentToString a ++ "]"
argumentToString (ArgumentAddress Size32 a) = "DWORD [" ++ argumentToString a ++ "]"
argumentToString (ArgumentAddress Size64 a) = "QWORD [" ++ argumentToString a ++ "]"
argumentToString (ArgumentAdd a1 a2)        = argumentToString a1 ++ " + " ++ argumentToString a2
argumentToString (ArgumentMul a1 a2)        = argumentToString a1 ++ " * " ++ argumentToString a2
argumentToString (ArgumentLabel s)          = s


opToString :: OpCode -> String
opToString Nop     = "nop"
opToString Mov     = "mov"
opToString Xchg    = "xchg"
opToString Lea     = "lea"
opToString Push    = "push"
opToString Pop     = "pop"
opToString Add     = "add"
opToString Sub     = "sub"
opToString Mul     = "mul"
opToString Div     = "div"
opToString And     = "and"
opToString Or      = "or"
opToString Xor     = "xor"
opToString Not     = "not"
opToString Neg     = "neg"
opToString Test    = "test"
opToString Cmp     = "cmp"
opToString (J s)   = 'j' : switchToString s
opToString (Set s) = "set" ++ switchToString s
opToString Jmp     = "jmp"
opToString Call    = "call"
opToString Leave   = "leave"
opToString Ret     = "ret"


switchToString :: Switch -> String
switchToString E  = "e"
switchToString NE = "ne"
switchToString Z  = "z"
switchToString NZ = "nz"
switchToString L  = "l"
switchToString LE = "le"
switchToString G  = "g"
switchToString GE = "ge"
switchToString B  = "b"
switchToString BE = "be"
switchToString A  = "a"
switchToString AE = "ae"


registerToString :: Register -> String
registerToString RAX = "rax"
registerToString RCX = "rcx"
registerToString RDX = "rdx"
registerToString RBX = "rbx"
registerToString RSI = "rsi"
registerToString RDI = "rdi"
registerToString RSP = "rsp"
registerToString RBP = "rbp"
registerToString R8  = "r8"
registerToString R9  = "r9"
registerToString R10 = "r10"
registerToString R11 = "r11"
registerToString R12 = "r12"
registerToString R13 = "r13"
registerToString R14 = "r14"
registerToString R15 = "r15"
registerToString EAX = "eax"
registerToString ECX = "ecx"
registerToString EDX = "edx"
registerToString EBX = "ebx"
registerToString ESI = "esi"
registerToString EDI = "edi"
registerToString ESP = "esp"
registerToString EBP = "ebp"
registerToString AX  = "ax"
registerToString CX  = "cx"
registerToString DX  = "dx"
registerToString BX  = "bx"
registerToString SI  = "si"
registerToString DI  = "di"
registerToString SP  = "sp"
registerToString BP  = "bp"
registerToString AL  = "al"
registerToString CL  = "cl"
registerToString DL  = "dl"
registerToString BL  = "bl"

