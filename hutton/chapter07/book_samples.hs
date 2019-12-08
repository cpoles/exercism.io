module BookSamples where

twice :: (a -> b) -> a -> b
twice f x = f (f x)


