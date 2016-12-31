module CodeGen.AssemblyFormatters where

import CodeGen.Assembly(Instruction)

data AssemblyFormatter = AssemblyFormatter { formatterName :: String
                                           , formatterBoilerplate :: String
                                           , formatterConverter :: Instruction -> String
                                           }

