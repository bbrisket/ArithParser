module Tests where

import ArithParser

data TestResult
    = Success
    | Failure String

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _       = False

message :: TestResult -> String
message Success           = "Success!"
message (Failure message) = message

-- Test a function that takes one argument.
-- Usage: expect1 "myFunc" myFunc arg expectedOutput
expect1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> TestResult
expect1 funcName func input expectedOutput =
    if expectedOutput == actual then
        Success
    else
        Failure $
            "Expected " ++ evaledStr ++
            " to be " ++ show expectedOutput ++
            ", but got " ++ show actual
    where
        actual    = func input
        evaledStr = funcName ++ " " ++ show input

tests :: [TestResult]
tests =
    [ expect1 "eval" eval
        (Frac ((-3),1))
        ((-3),1)
    , expect1 "eval" eval
        (Mult (Plus (Frac (2,1)) (Frac ((-6),1))) (Plus (Frac (3,1)) (Frac (2,1))))
        ((-20),1)
    , expect1 "eval" eval
        (Div (Frac (13,1)) (Frac (6,1)))
        (13,6)
    , expect1 "eval" eval
        (Plus (Mult (Frac (2,1)) (Frac (3,1))) (Div (Frac (13,1)) (Frac ((-6),1))))
        (23,6)
     , expect1 "tokenize" tokenize
         "1+2"
         [NumToken (1,1), PlusToken, NumToken (2,1)]
     , expect1 "tokenize" tokenize
         "1 + 20"
         [NumToken (1,1), PlusToken, NumToken (20,1)]
     , expect1 "tokenize" tokenize
         "1 * -2"
         [NumToken (1,1), MultToken, NumToken ((-2),1)]
     , expect1 "tokenize" tokenize
         "1 + 2 * 3 + 4"
         [NumToken (1,1), PlusToken, NumToken (2,1), MultToken, NumToken (3,1), PlusToken, NumToken (4,1)]
     , expect1 "tokenize" tokenize
         "1 * 2 + 3 + 4"
         [NumToken (1,1), MultToken, NumToken (2,1), PlusToken, NumToken (3,1), PlusToken, NumToken (4,1)]
     , expect1 "tokenize" tokenize
         "(1+2)"
         [LParenToken, NumToken (1,1), PlusToken, NumToken (2,1), RParenToken]
     , expect1 "tokenize" tokenize
         " (1 + 2 )"
         [LParenToken, NumToken (1,1), PlusToken, NumToken (2,1), RParenToken]
     , expect1 "tokenize" tokenize
         "(1 * 2 + 3)"
         [LParenToken, NumToken (1,1), MultToken, NumToken (2,1), PlusToken, NumToken (3,1), RParenToken]
     , expect1 "tokenize" tokenize
         "(-10 + 2) * 5"
         [LParenToken, NumToken ((-10),1), PlusToken, NumToken (2,1), RParenToken, MultToken, NumToken (5,1)]
     , expect1 "tokenize" tokenize
         "5 * (-10 + (2 + 4) * 3)"
         [NumToken (5,1), MultToken, LParenToken, NumToken ((-10),1), PlusToken, LParenToken, NumToken (2,1),
            PlusToken, NumToken (4,1), RParenToken, MultToken, NumToken (3,1), RParenToken]
     , expect1 "tokenize" tokenize
         "5 * (-10 + (2 + 4) * 3) * (3 + 2)"
         [NumToken (5,1), MultToken, LParenToken, NumToken ((-10),1), PlusToken, LParenToken, NumToken (2,1),
            PlusToken, NumToken (4,1), RParenToken, MultToken, NumToken (3,1), RParenToken, MultToken,
            LParenToken, NumToken (3,1), PlusToken, NumToken (2,1), RParenToken]

     , expect1 "parse" (eval . parse)
        [NumToken (1,1), PlusToken, NumToken (2,1)]
        (3,1)
     , expect1 "parse" (eval . parse)
        [NumToken (1,1), PlusToken, NumToken (20,1)]
        (21,1)
     , expect1 "parse" (eval . parse)
        [NumToken (1,1), MultToken, NumToken ((-2),1)]
        ((-2),1)
     , expect1 "parse" (eval . parse)
        [NumToken (1,1), PlusToken, NumToken (2,1), MultToken, NumToken (3,1), PlusToken, NumToken (4,1)]
        (11,1)
     , expect1 "parse" (eval . parse)
        [LParenToken, NumToken (1,1), PlusToken, NumToken (2,1), RParenToken]
        (3,1)
     , expect1 "parse" (eval . parse)
        [LParenToken, NumToken ((-10),1), PlusToken, NumToken (2,1), RParenToken, MultToken, NumToken (5,1)]
        ((-40),1)
     , expect1 "parse" (eval . parse)
        [LParenToken, NumToken (1,1), MultToken, NumToken (2,1), PlusToken, NumToken (3,1), RParenToken]
        (5,1)
     , expect1 "parse" (eval . parse)
        [NumToken (5,1), MultToken, LParenToken, NumToken ((-10),1), PlusToken, LParenToken, NumToken (2,1),
          PlusToken, NumToken (4,1), RParenToken, MultToken, NumToken (3,1), RParenToken]
        (40,1)
      , expect1 "eval . parse . tokenize $" (eval . parse . tokenize)
        "(15 /    -6)"
        ((-5),2)
      , expect1 "eval . parse . tokenize $" (eval . parse . tokenize)
        "1/3 + 1/2"
        (5,6)
      , expect1 "eval . parse . tokenize $" (eval . parse . tokenize)
        "1/2 * 2/3"
        (1,3)
      , expect1 "eval . parse . tokenize $" (eval . parse . tokenize)
        "1/(2 + 3/4)"
        (4,11)
      , expect1 "eval . parse . tokenize $" (eval . parse . tokenize)
        "1 + -8 / (8 + -2)"
        ((-1),3)
    ]

successes       = filter isSuccess tests
failures        = filter (not . isSuccess) tests
failureMessages = map message failures

results =
    ( length successes
    , length failures
    , failureMessages
    )

showFailures :: IO ()
showFailures = mapM_ putStrLn failureMessages
