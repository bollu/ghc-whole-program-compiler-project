{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Foo where
import GHC.Prim (Int#, (+#), (-#), (*#))


data Int  = MkInt Int#
one :: Int; one = MkInt 1#

(+) :: Int -> Int -> Int
(+) (MkInt a) (MkInt b) = MkInt (a +# b)

(*) :: Int -> Int -> Int
(*) (MkInt a) (MkInt b) = MkInt (a *# b)


(-) :: Int -> Int -> Int
(-) (MkInt a) (MkInt b) = MkInt (a -# b)

factorial :: Int -> Int
factorial n@(MkInt n#) = 
    case n# of 
        0# -> one
        _ -> n * (factorial (n - one))
