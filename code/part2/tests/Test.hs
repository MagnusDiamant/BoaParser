-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [ 
   testCase "numConst" $
      parseString "2" @?= Right [SExp (Const (IntVal 2))]
  , testCase "numConst minus" $
      parseString "-2" @?= Right [SExp (Const (IntVal (-2)))]
  , testCase "numConst whitespace" $
      parseString "  2  " @?= Right [SExp (Const (IntVal 2))]
  , testCase "plus" $
      parseString " 2+2 " @?= Right [SExp (Oper Plus (Const (IntVal 2)) (Const (IntVal 2)))]
  , testCase "minus" $
      parseString " 2 - 2 " @?= Right [SExp (Oper Minus (Const (IntVal 2)) (Const (IntVal 2)))]
  , testCase "multiply" $
      parseString " 2*2 " @?= Right [SExp (Oper Times (Const (IntVal 2)) (Const (IntVal 2)))]
  , testCase "division" $
      parseString " 2 // 2 " @?= Right [SExp (Oper Div (Const (IntVal 2)) (Const (IntVal 2)))]
  , testCase "Modulo" $
      parseString " 2 % 2 " @?= Right [SExp (Oper Mod (Const (IntVal 2)) (Const (IntVal 2)))]
  , testCase "2 == 2" $
      parseString " 2 == 2 " @?= Right [SExp (Oper Eq (Const (IntVal 2)) (Const (IntVal 2)))]
  , testCase "2 != 2" $
      parseString " 2 != 2 " @?= Right [SExp (Not (Oper Eq (Const (IntVal 2)) (Const (IntVal 2))))]
  , testCase "2 > 2" $
      parseString " 2 > 2 " @?= Right [SExp (Oper Greater (Const (IntVal 2)) (Const (IntVal 2)))]
  , testCase " 2 < -2 " $
      parseString " 2 < -2 " @?= Right [SExp (Oper Less(Const (IntVal 2)) (Const (IntVal (-2))))]
  , testCase "2 <= -2" $
      parseString " 2 <= -2 " @?= Right [SExp (Not (Oper Greater (Const (IntVal 2)) (Const (IntVal (-2)))))]
  , testCase "2 >= -2" $
      parseString " 2 >= -2 " @?= Right [SExp (Not (Oper Less (Const (IntVal 2)) (Const (IntVal (-2)))))]
  , testCase "2+2+2*3" $
      parseString "2+2+2*3" @?= Right [SExp (Oper Plus (Oper Plus (Const (IntVal 2)) (Const (IntVal 2))) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))]
  , testCase "2+2==3" $
      parseString "2+2==3" @?= Right [SExp (Oper Eq (Oper Plus (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 3)))]
  , testCase "pBool None" $
      parseString "None" @?= Right [SExp (Const NoneVal)]
  , testCase "pBool True" $
      parseString "True" @?= Right [SExp (Const TrueVal)]
  , testCase "pBool False" $
      parseString "False" @?= Right [SExp (Const FalseVal)]
  , testCase "hej (2+2)" $
      parseString "hej (2+2)" @?= Right [SExp (Call "hej" [Oper Plus (Const (IntVal 2)) (Const (IntVal 2))])]
  , testCase "hej (2+2,2*2)" $
      parseString "hej (2+2,2*2)" @?= Right [SExp (Call "hej" [Oper Plus (Const (IntVal 2)) (Const (IntVal 2)),Oper Times (Const (IntVal 2)) (Const (IntVal 2))])]
  , testCase "[2+2,2+2]" $ 
      parseString "[2+2,2+2]" @?= Right [SExp (List [Oper Plus (Const (IntVal 2)) (Const (IntVal 2)),Oper Plus (Const (IntVal 2)) (Const (IntVal 2))])]
  , testCase "x" $ 
      parseString "x" @?= Right [SExp (Var "x")]
  , testCase "x = (2+2)" $ 
      parseString "x = (2+2)" @?= Right [SDef "x" (Oper Plus (Const (IntVal 2)) (Const (IntVal 2)))] 
  , testCase "2 in [2,2]" $ 
      parseString "2 in [2,2]" @?= Right [SExp (Oper In (Const (IntVal 2)) (List [Const (IntVal 2),Const (IntVal 2)]))]
  , testCase "2 not in [2,2]" $ 
      parseString "2 not in [2,2]" @?= Right [SExp (Not (Oper In (Const (IntVal 2)) (List [Const (IntVal 2),Const (IntVal 2)])))]
  , testCase "String 'Hej'" $ 
      parseString "'Hej'" @?= Right [SExp (Const (StringVal "hej"))]
  , testCase "x in4" $ 
      parseString "x in4" @?= Left "<message>"
  , testCase "[x for ynot in z]" $ 
      parseString "[x for ynot in z]"  @?= Right [SExp (Compr (Var "x") [CCFor "ynot" (Var "z")])]
  ] 

