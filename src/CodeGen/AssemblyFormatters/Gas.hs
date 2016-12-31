module CodeGen.AssemblyFormatters.Gas(gasFormatter) where

import Data.List
import Data.Maybe

import CodeGen.Assembly
import CodeGen.AssemblyFormatters


gasFormatter :: AssemblyFormatter
gasFormatter = AssemblyFormatter { formatterName = "GAS"
                                 , formatterBoilerplate = boilerplate
                                 , formatterConverter = convert
                                 }


boilerplate :: String
boilerplate = ".globl main\n"
           -- ++ "extern printInt\n"
           -- ++ "extern __alloc_array\n"
           -- ++ "extern error\n"
           ++ ".text\n"


convert :: Instruction -> String
convert (I0 o)       = opToString o
convert (I1 o a)     = opToString o ++ " " ++ argumentToString a
convert (I2 o a1 a2) = opToString o ++ instructionSuffix a1 a2 ++  " "
                     ++ argumentToString a2 ++ ", " ++ argumentToString a1


-- TODO: lea is probably wrong here
instructionSuffix :: Argument -> Argument -> String
instructionSuffix a1 a2 = if s1 /= s2
                            then error $ "Argument size mismatch " ++ show a1 ++ " | " ++ show a2
                            else sufForSize s1
    where
        argumentSize :: Argument -> OperandSize
        argumentSize (ArgumentRegister r)  = registerSize r
        argumentSize (ArgumentInteger s _) = s
        argumentSize (ArgumentAddress s _) = s
        argumentSize _                     = error "Wrong argument to two-argument opcode"
        s1 = argumentSize a1
        s2 = argumentSize a2
        sufForSize Size8  = "b"
        sufForSize Size16 = "s"
        sufForSize Size32 = "l"
        sufForSize Size64 = "q"


argumentToString :: Argument -> String
argumentToString (ArgumentRegister r)       = registerPrefixed r
argumentToString (ArgumentInteger _ i)      = '$' : show i
argumentToString (ArgumentAddress _ a)      = addressToString a
argumentToString (ArgumentLabel s)          = s
argumentToString _                          = error "Wrong top-level argument"


-- TODO: This assumes the address is correct, implement checking maybe?
addressToString :: Argument -> String
addressToString a =
    let components = let work (ArgumentAdd a1 a2) = work a1 ++ work a2
                         work aa                  = [aa]
                     in work a
        extractInteger (ArgumentInteger _ i) = Just $ show i
        extractInteger _                     = Nothing
        constant = safeHead $ mapMaybe extractInteger components
        extractMul (ArgumentMul (ArgumentInteger _ i) (ArgumentRegister r)) =
            Just $ registerPrefixed r ++ ", " ++ show i
        extractMul (ArgumentMul (ArgumentRegister r) (ArgumentInteger _ i)) =
            Just $ registerPrefixed r ++ ", " ++ show i
        extractMul _                                                        = Nothing
        mul = safeHead $ mapMaybe extractMul components
        extractRegs (ArgumentRegister r) = Just $ registerPrefixed r
        extractRegs _                    = Nothing
        regs = mapMaybe extractRegs components
        outer = fromMaybe "" constant
        inner = case (constant, regs, mul) of 
                  (_, [r1, r2], Nothing) -> outer ++ "(" ++ r1 ++ ", " ++ r2 ++ ", 1)"
                  (_, [r], Nothing)      -> outer ++ "(" ++ r ++ ")"
                  (_, [r], Just s)       -> outer ++ "(" ++ r ++ ", " ++ s ++ ")"
                  (_, [], Just s)        -> outer ++ "(" ++ s ++ ")"
                  (Just n, [], Nothing)  -> "(" ++ n ++ ")"
        in inner


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


registerPrefixed :: Register -> String
registerPrefixed r = '%' : registerToString r


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


registerSize :: Register -> OperandSize
registerSize RAX = Size64
registerSize RCX = Size64
registerSize RDX = Size64
registerSize RBX = Size64
registerSize RSI = Size64
registerSize RDI = Size64
registerSize RSP = Size64
registerSize RBP = Size64
registerSize R8  = Size64
registerSize R9  = Size64
registerSize R10 = Size64
registerSize R11 = Size64
registerSize R12 = Size64
registerSize R13 = Size64
registerSize R14 = Size64
registerSize R15 = Size64
registerSize EAX = Size32
registerSize ECX = Size32
registerSize EDX = Size32
registerSize EBX = Size32
registerSize ESI = Size32
registerSize EDI = Size32
registerSize ESP = Size32
registerSize EBP = Size32
registerSize AX  = Size16
registerSize CX  = Size16
registerSize DX  = Size16
registerSize BX  = Size16
registerSize SI  = Size16
registerSize DI  = Size16
registerSize SP  = Size16
registerSize BP  = Size16
registerSize AL  = Size8
registerSize CL  = Size8
registerSize DL  = Size8
registerSize BL  = Size8


safeHead :: [a] -> Maybe a
safeHead l = fst <$> uncons l

