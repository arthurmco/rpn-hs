-- Reverse Polish calculator
-- Does not support functions with more than 2 parameters

data Operation = OpAdd | OpSub | OpMul | OpDiv deriving Show

-- | Get the parameter count of an operation
-- |
-- | We use `Num` instead of an integer so we can combine this operation
-- | with anothers and not get a type error
-- |
-- | If there is no limit, return Nothing, else return the count
getParameterLimit :: (Num a) => Operation -> Maybe a
getParameterLimit _ = Nothing


stringToToken :: (Fractional a, Read a) => String -> Either Operation a
stringToToken v =
  case v of "+" -> Left OpAdd
            "-" -> Left OpSub
            "*" -> Left OpMul
            "/" -> Left OpDiv
            x -> Right (read x)


-- | Split the calculation elements in tokens
splitCalculations = words

-- | Decode the split calculations in arrays of operations and operands
decodeTokens = map stringToToken


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
consumeOperations :: (Fractional a) => [a] -> [Either Operation a] -> a
consumeOperations _ [] = 1000
consumeOperations _ [Right val] = val
consumeOperations values [Left op] =  consumeOperation values op
consumeOperations values (token:xs) =
  case token of Right val -> let res = val:values
                             in
                               consumeOperations res xs
                Left op ->
                  let opres = consumeOperation values op
                      res = (Right opres):xs
                  in
                    consumeOperations [] res


-- | Consume one single operation, for unlimited parameters
-- |
-- | The list of fractionals are the values
consumeOperation :: (Fractional a) => [a] -> Operation -> a
consumeOperation [] _ = 0
consumeOperation [v] _ = v
consumeOperation (x:xs) op =
  case op of OpAdd -> x + (consumeOperation xs op)
             OpSub -> x - (consumeOperation xs op)
             OpMul -> x * (consumeOperation xs op)
             OpDiv -> x / (consumeOperation xs op)





