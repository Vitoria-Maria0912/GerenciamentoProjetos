module Haskell.Usuario where


data Usuario = Usuario {
    idUsuario:: Int, nome:: String, senha:: String} deriving (Show, Read, Eq)



