{-# LANGUAGE ScopedTypeVariables #-}
module Data.Tacview.Annotate where

import Control.Exception

-- Waiting not-so-patiently for GHC 9.12
data ExceptionAnnotation e = Annotation String (ExceptionAnnotation e) | Ex e
    deriving anyclass (Exception)

instance (Show e) => Show (ExceptionAnnotation e) where
    show (Annotation s ea) = s <> ":\n" <> show ea
    show (Ex e) = show e

whileIO :: String -> IO a -> IO a
whileIO ctxt f = f `catches` [Handler skipAs, Handler appendEx, Handler baseEx] where
    -- Don't bother annotating async exceptions that are killing this thread.
    skipAs :: SomeAsyncException -> IO a
    skipAs = throwIO
    -- Otherwise, if it's an existing annotation, append to it.
    appendEx :: ExceptionAnnotation SomeException -> IO a
    appendEx ea = throwIO $ Annotation ctxt ea
    -- If it's anything else, start the annotation chain.
    baseEx :: SomeException -> IO a
    baseEx ex = throwIO $ Annotation ctxt $ Ex ex
