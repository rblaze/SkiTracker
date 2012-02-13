module MinQueue (MinQueue, empty, MinQueue.minimum, push, pop, peek, MinQueue.null) where

data MinStack a = MinStack { values :: [a], minimums :: [a] }
    deriving Show

data MinQueue a = MinQueue { inStack :: MinStack a, outStack :: MinStack a }
    deriving Show

stackPush :: Ord a => a -> MinStack a -> MinStack a
stackPush a stack
    | Prelude.null mins = MinStack [a] [a]
    | head mins < a = MinStack (a : vals) mins
    | otherwise = MinStack (a : vals) (a : mins)
    where
        mins = minimums stack
        vals = values stack

stackPop :: Eq a => MinStack a -> MinStack a
stackPop stack
    | Prelude.null mins = error "Stack underflow"
    | head mins == head vals = MinStack (tail vals) (tail mins)
    | otherwise = MinStack (tail vals) mins
    where
        mins = minimums stack
        vals = values stack

stackMin :: MinStack a -> Maybe a
stackMin stack
    | Prelude.null $ minimums stack = Nothing
    | otherwise = Just (head $ minimums stack)

stackPeek :: MinStack a -> Maybe a
stackPeek stack
    | Prelude.null vals = Nothing
    | otherwise = Just (head vals)
    where
        vals = values stack

stackBottom :: MinStack a -> Maybe a
stackBottom stack
    | Prelude.null vals = Nothing
    | otherwise = Just (last vals)
    where
        vals = values stack

stackNull :: MinStack a -> Bool
stackNull = Prelude.null . values 

empty :: MinQueue a
empty = MinQueue (MinStack [] []) (MinStack [] [])

null :: MinQueue a -> Bool
null queue = stackNull (inStack queue) && stackNull (outStack queue) 

minimum :: Ord a => MinQueue a -> Maybe a
minimum queue
    | stackNull st_in = stackMin st_out
    | stackNull st_out = stackMin st_in
    | otherwise = min (stackMin st_in) (stackMin st_out)
    where
        st_in = inStack queue
        st_out = outStack queue

push :: Ord a => a -> MinQueue a -> MinQueue a
push v queue = MinQueue (stackPush v (inStack queue)) (outStack queue)

pop ::  Ord a => MinQueue a -> MinQueue a
pop queue
    | not $ stackNull st_out = MinQueue st_in $ stackPop st_out
    | not $ stackNull st_in = MinQueue (MinStack [] []) (stackPop revstack)
    | otherwise = error "Queue underflow"
    where
        st_in = inStack queue
        st_out = outStack queue
        revstack = foldl (flip stackPush) (MinStack [] []) (values st_in)

peek :: Eq a => MinQueue a -> Maybe a
peek queue
    | not $ stackNull st_out = stackPeek st_out 
    | not $ stackNull st_in = stackBottom st_in
    | otherwise = Nothing
    where
        st_in = inStack queue
        st_out = outStack queue