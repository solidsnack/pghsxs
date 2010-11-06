{-# LANGUAGE EmptyDataDecls
           , ForeignFunctionInterface
  #-}


{-| Low-level interface to Postgres function call construction and execution.

    We use 'Ptr' everywhere because Postgres will deallocate the pointers.
 -}

module PGLowerSketch where

import Foreign
import Foreign.C

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   FmgrInfo                             flinfo;
   FunctionCallInfoData                 fcinfo;
   Datum                                result;
   fmgr_info(functionId, &flinfo);
   InitFunctionCallInfoData(fcinfo, &flinfo, 1, NULL, NULL);

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

-- | A template for generating calls to a specific function. 
newtype Oid                  =  Oid CUInt

-- | A template for generating calls to a specific function. 
data FMGRInfo

foreign import ccall unsafe "fmgr_info"
  fmgr_pg :: Oid -> Ptr FMGRInfo -> IO ()

-- | Wrapper around native FMGRInfo routine.
fmgr                        ::  Oid -> Ptr FMGRInfo


-- | A box for arguments of any type.
data Datum

-- | Specification of a single function call. 
data FCInfo

-- | Call the procedure specification within Postgres. 
foreign import ccall unsafe "InitFunctionCallInfoData"
  fcinit_pg :: Ptr FCInfo -> Ptr FMGRInfo -> ? -> ? -> ? -> IO ()

-- | Call the procedure specification within Postgres. 
foreign import ccall unsafe "FunctionCallInvoke"
  invoke_pg :: Ptr FCInfo -> IO ()




{-| Class of things that Postgres can allocate for us. 
 -}
class PAlloc t where
  palloc                    ::  Ptr t


