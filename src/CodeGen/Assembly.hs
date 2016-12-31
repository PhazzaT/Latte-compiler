module CodeGen.Assembly where

import Data.Int

data Register 
    = RAX | RCX | RDX | RBX | RSI | RDI | RSP | RBP
    | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
    | EAX | ECX | EDX | EBX | ESI | EDI | ESP | EBP
    | AX  | CX  | DX  | BX  | SI  | DI  | SP  | BP
    | AL  | CL  | DL  | BL
    deriving (Eq, Ord)

data Switch
    = E | NE | Z | NZ
    | L | LE | G | GE
    | B | BE | A | AE
    deriving (Eq, Ord)

type Label = String

data OpCode
    = Nop
    | Mov
    | Xchg
    | Lea
    | Push
    | Pop
    | Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Xor
    | Not
    | Neg
    | Test
    | Cmp
    | J Switch
    | Set Switch
    | Jmp
    | Call
    | Leave
    | Ret
    deriving (Eq, Ord)

data Instruction
    = I0 OpCode
    | I1 OpCode Argument
    | I2 OpCode Argument Argument

data Argument
    = ArgumentRegister Register
    | Argument8 Int8
    | Argument16 Int16
    | Argument32 Int32
    | Argument64 Int64
    | ArgumentAddress OperandSize Argument
    | ArgumentAdd Argument Argument
    | ArgumentMul Argument Argument
    | ArgumentLabel String
    deriving (Eq, Ord)

data OperandSize = Size8 | Size16 | Size32 | Size64 deriving (Eq, Ord)
data AddressForm = BYTE [Argument] | WORD [Argument] | DWORD [Argument] | QWORD [Argument]

-- A helper class used to implement Intel syntax-like DSL
class ToArgument a where
    toArgument :: a -> Argument

instance ToArgument Argument where
    toArgument = id

instance ToArgument Int where
    toArgument i = 
        let checkSize s = -2^s <= i && i < 2^s
         in if      checkSize 8  then Argument8  $ fromIntegral i
            else if checkSize 16 then Argument16 $ fromIntegral i
            else if checkSize 32 then Argument32 $ fromIntegral i
            else if checkSize 64 then Argument64 $ fromIntegral i
            else error "Integral size too big to represent as offset"

instance ToArgument Register where
    toArgument = ArgumentRegister

instance ToArgument AddressForm where
    toArgument (BYTE  [a]) = ArgumentAddress Size8  a
    toArgument (WORD  [a]) = ArgumentAddress Size16 a
    toArgument (DWORD [a]) = ArgumentAddress Size32 a
    toArgument (QWORD [a]) = ArgumentAddress Size64 a

-- I1 MOV (DWORD [EAX !+ 8 !* ECX]) EAX

infixl 6 ^+
(^+) :: (ToArgument a1, ToArgument a2) => a1 -> a2 -> Argument
a1 ^+ a2 = ArgumentAdd (toArgument a1) (toArgument a2)

infixl 7 ^*
(^*) :: (ToArgument a1, ToArgument a2) => a1 -> a2 -> Argument
a1 ^* a2 = ArgumentMul (toArgument a1) (toArgument a2)

