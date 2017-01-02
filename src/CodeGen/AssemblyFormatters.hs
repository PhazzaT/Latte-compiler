module CodeGen.AssemblyFormatters where

import CodeGen.Assembly(Instruction, GlobalConstant)

data AssemblyFormatter = AssemblyFormatter { formatterName :: String
                                           , formatterBoilerplate :: String
                                           , formatterConverter :: Instruction -> String
                                           , formatterConstantConverter :: GlobalConstant -> String
                                           }

