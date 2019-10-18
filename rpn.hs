-- Reverse Polish calculator

data Operation = OpAdd | OpSub | OpMul | OpDiv deriving Show


stringToOperation :: (Fractional a, Read a) => String -> Either Operation a
stringToOperation v =
  case v of "+" -> Left OpAdd
            "-" -> Left OpSub
            "*" -> Left OpMul
            "/" -> Left OpDiv
            x -> Right (read x)


-- | Split the calculation elements in tokens
splitCalculations = words

-- | Decode the split calculations in arrays of operations and operands
decodeTokens = map stringToOperation


-- | Consume a list of operations and bring the result
-- | Each iteration will consume an operation and put the result on
-- | top of the list
-- |
-- | For example, it will go like this:
-- |   3 2 + 5 *
-- |   6 5 *
-- |   30
-- |
-- | When we have an unique value, it will return said value
consumeOperations :: (Fractional a) => [Either Operation a] -> a
consumeOperations [] = 0
consumeOperations [Left _] = 0
consumeOperations [Right val] = val
consumeOperations (val1:val2:op:xs) =
  let Right v1 = val1
      Right v2 = val2
  in
    case op of Left OpAdd -> consumeOperations $ (Right (v1 + v2)):xs
               Left OpSub -> consumeOperations $ (Right (v1 - v2)):xs
               Left OpMul -> consumeOperations $ (Right (v1 * v2)):xs
               Left OpDiv -> consumeOperations $ (Right (v1 / v2)):xs
               Right v -> consumeOperations $ xs
  
  






