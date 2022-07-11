module Test where

open import Data.Bool

neg : Bool → Bool
neg true = false
neg false = true
-- {-# COMPILE GHC neg as neg #-}
