module Windows.DirectComposition.Device3 (createDevice) where

import Windows.Types (HRESULT)
import Windows.Struct.GUID (GUID(..))
import Windows.Com.IUnknown (IUnknown)
import Windows.ComBase (ComInterface(..))
import qualified Windows.Const.HResult as HR
import Windows.LibLoader (Lib, getProcAddress)
import Foreign.Ptr (Ptr, castPtr, nullPtr, FunPtr)
import Foreign.C.Types (CLong(..))
import Foreign.Storable (peek, poke)
import Foreign.Marshal.Alloc (alloca)

type PFN_CreateDevice = Ptr IUnknown -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createDevice :: FunPtr PFN_CreateDevice -> PFN_CreateDevice

createDevice :: ComInterface i => Lib -> Maybe (Ptr a) -> IO (Either HRESULT (Ptr i))
createDevice lib renderDevice = 
  alloca $ \ptr ->
  alloca $ \refiid -> do
    fn <- dcall_createDevice <$> getProcAddress "DCompositionCreateDevice3" lib
    peek ptr >>= poke refiid . guid
    hr <- fn (maybe nullPtr castPtr renderDevice) refiid $ castPtr ptr
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr
