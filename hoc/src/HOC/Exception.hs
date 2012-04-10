{-# LANGUAGE DeriveDataTypeable #-}
module HOC.Exception where

import Control.Exception
import Data.Typeable        ( Typeable )
import Foreign.Ptr          ( Ptr, castPtr, nullPtr )
import Foreign.StablePtr
import Foreign.C.String     ( withCString )
import HOC.Arguments        ( importArgument, exportArgument )
import HOC.CBits
import HOC.ID               ( {- instances -} )

newtype WrappedNSException = WrappedNSException (ID ())
    deriving Typeable

instance Exception WrappedNSException
instance Show WrappedNSException where
    show (WrappedNSException ex) = "<<NSException>>"

-- |get the exception pointer figure out if it is a NSException
-- or a haskell exception and throw it.
exceptionObjCToHaskell :: Ptr ObjCException -> IO a
exceptionObjCToHaskell exception = do
    sptr <- unwrapHaskellException exception
    if (castStablePtrToPtr sptr == nullPtr)
        then do
            exc <- importArgument (castPtr exception)
            throwIO $ WrappedNSException exc
        else do
            exc <- deRefStablePtr sptr
            throwIO exc

exceptionHaskellToObjC :: IO a -> IO (Ptr ObjCException)
exceptionHaskellToObjC action = 
    (action >> return nullPtr)
        `catches` [
            Handler $ \(WrappedNSException exc) -> fmap castPtr (exportArgument exc),
            Handler $ \exc -> withCString (show exc) $ \cstr ->
                                (newStablePtr (exc :: SomeException) 
                                    >>= wrapHaskellException cstr)
        ]