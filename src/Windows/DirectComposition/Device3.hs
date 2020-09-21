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
import qualified Foreign.Marshal.Utils as Marshal
import Windows.Com.Monad (ComT, comT, runComT, handleHRESULT)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)

type PFN_CreateDevice = Ptr IUnknown -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createDevice :: FunPtr PFN_CreateDevice -> PFN_CreateDevice

createDevice :: ComInterface i => Lib -> Maybe (Ptr a) -> ComT IO (Ptr i)
createDevice lib renderDevice = flip runContT pure $ do
  ptr <- ContT $ \f -> comT $ alloca $ runComT . f
  refiid <- ContT $ \f -> comT $ peek ptr >>= flip Marshal.with (runComT . f) . guid
  fn <- liftIO $ dcall_createDevice <$> getProcAddress "DCompositionCreateDevice3" lib
  lift $ liftIO (fn (maybe nullPtr castPtr renderDevice) refiid $ castPtr ptr) >>= handleHRESULT >> lift (peek ptr)
