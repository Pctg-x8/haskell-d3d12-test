{-# LANGUAGE TypeFamilies #-}

module Windows.Com.IUnknown where

import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr)
import Foreign.C.Types (CULong(..), CLong(..))
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Windows.Struct.GUID (GUID(..))
import Windows.Types (HRESULT)
import qualified Windows.Const.HResult as HR
import Windows.ComBase (ComInterface(..))
import Control.Exception (bracket)
import Debug.Trace (trace)

data IUnknownVtbl
data IUnknown = IUnknown (Ptr IUnknownVtbl)
instance ComInterface IUnknown where
  type VTable IUnknown = IUnknownVtbl

  guid _ = GUID 0 0 0 0xc000000000000046
instance Storable IUnknown where
  sizeOf _ = 8
  alignment _ = 8
  peek p = IUnknown <$> peek (castPtr p)
  poke p (IUnknown pv) = flip poke pv $ castPtr p

_VTBL_INDEX_QUERY_INTERFACE = 0
_VTBL_INDEX_ADD_REF = 1
_VTBL_INDEX_RELEASE = 2

foreign import ccall "dynamic" dcall_refs :: FunPtr (Ptr IUnknown -> IO CULong) -> Ptr IUnknown -> IO CULong
foreign import ccall "dynamic" dcall_qifs :: FunPtr (Ptr IUnknown -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT) -> Ptr IUnknown -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT

addRef, release :: Ptr IUnknown -> IO CULong
addRef this = getFunctionPtr _VTBL_INDEX_ADD_REF this >>= flip dcall_refs this
release this = getFunctionPtr _VTBL_INDEX_RELEASE this >>= flip dcall_refs this

queryInterface :: (ComInterface i, Storable i) => Ptr IUnknown -> IO (Either HRESULT (Ptr i))
queryInterface this = do
  fn <- getFunctionPtr _VTBL_INDEX_QUERY_INTERFACE this

  alloca $ \ptr ->
    alloca $ \refiid -> do
      peek ptr >>= poke refiid . guid
      hr <- dcall_qifs fn this refiid (castPtr ptr)
      if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

withInterface :: ComInterface i => Ptr i -> (Ptr i -> IO a) -> IO a
withInterface p = bracket (pure p) (release . castPtr)
