module Compare
    ( makeTSCompare
    ) where

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
    where newFunc (i1, Nothing ) (i2, Nothing ) = (i1, Nothing )
          newFunc (_, Nothing ) (i, val) = (i, val)
          newFunc (i, val) (_, Nothing ) = (i, val)
          newFunc (i1, Just val1) (i2, Just val2 ) = 
                                                if func val1 val2 == val1
                                                then (i1, Just val1)    
                                                else (i2, Just val2)
