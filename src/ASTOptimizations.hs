module ASTOptimizations where

import AST
import ASTOptimizations.ConstantFolding
import ASTOptimizations.DeadCodeElimination


astOptimizations :: ProgramTyped -> ProgramTyped
astOptimizations = deadCodeElimination . constantFolding

