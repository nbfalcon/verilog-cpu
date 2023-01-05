{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module ErrorCollectorM where

data ErrorCollectorM e a = ErrorCollectorM { runErrorCollector1 :: [e] -> ([e], a) }

runErrorCollector :: ErrorCollectorM e b -> ([e], b)
runErrorCollector m = let (errs, a) = runErrorCollector1 m [] in (reverse errs, a)

instance Functor (ErrorCollectorM e) where
    fmap f ErrorCollectorM { runErrorCollector1 } = ErrorCollectorM $ \t -> let (e', r) = runErrorCollector1 t in (e', f r)

instance Applicative (ErrorCollectorM e) where
    pure v = ErrorCollectorM $ (,v)
    ErrorCollectorM { runErrorCollector1=lhs } <*> ErrorCollectorM { runErrorCollector1=rhs } = ErrorCollectorM $ \e ->
        let (errorsL, mapper) = lhs e
            (errorsR, v) = rhs errorsL
          in (errorsR, mapper v)

instance Monad (ErrorCollectorM e) where
    ErrorCollectorM { runErrorCollector1=run } >>= f = ErrorCollectorM $ \e -> let (e', v) = run e in (runErrorCollector1 (f v) e')


newError :: e -> ErrorCollectorM e ()
newError err  = ErrorCollectorM $ \e -> (err:e, ())