{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module UseCases.KVSasTF where

class Monad m => Persistence k v m where
  listAllKvs :: m [(k, v)]
  getKvs     :: k -> m (Maybe v)
  insertKvs  :: k -> v -> m ()
  deleteKvs  :: k -> m ()
 
class Monad m => Trace m where
  trace :: String -> m ()
  
class Monad m => Input m a where
  input :: m a  
  