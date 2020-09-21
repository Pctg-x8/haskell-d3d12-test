{-# LANGUAGE TypeFamilies #-}

module Windows.Direct3D12.Debug (ID3D12Debug, enableDebugLayer, getDebugInterface, withDebugInterface) where

import Windows.ComBase (ComInterface(..))
import Windows.Com.IUnknown (withInterface)
import Windows.Struct.GUID (GUID(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr)
import Windows.LibLoader (Lib, getProcAddress)
import Windows.Types (HRESULT)
import qualified Windows.Const.HResult as HR
import Foreign.C.Types (CLong(..))
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Marshal.Utils as Marshal
import Windows.Com.Monad (ComT, comT, runComT, handleHRESULT)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)

data ID3D12DebugVtbl
data ID3D12Debug = ID3D12Debug (Ptr ID3D12DebugVtbl)
instance ComInterface ID3D12Debug where
  type VTable ID3D12Debug = ID3D12DebugVtbl
  guid _ = GUID 0x344488b7 0x6846 0x474b 0xb989f027448245e0
instance Storable ID3D12Debug where
  sizeOf _ = 8
  alignment _ = 8
  peek p = ID3D12Debug <$> peek (castPtr p)
  poke p (ID3D12Debug vp) = castPtr p `poke` vp

foreign import ccall "dynamic" dcall_f1 :: FunPtr (Ptr ID3D12Debug -> IO ()) -> Ptr ID3D12Debug -> IO ()
enableDebugLayer :: Ptr ID3D12Debug -> IO ()
enableDebugLayer this = flip dcall_f1 this =<< getFunctionPtr 3 this

type PFN_D3D12GetDebugInterface = Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_getdebuginterface :: FunPtr PFN_D3D12GetDebugInterface -> PFN_D3D12GetDebugInterface
getDebugInterface :: Lib -> ComT IO (Ptr ID3D12Debug)
getDebugInterface lib = flip runContT pure $ do
  ptr <- ContT $ \f -> comT $ alloca $ runComT . f
  refiid <- ContT $ \f -> comT $ Marshal.with (guid (undefined :: Ptr ID3D12Debug)) $ runComT . f
  fn <- liftIO $ dcall_getdebuginterface <$> getProcAddress "D3D12GetDebugInterface" lib
  hr <- liftIO $ fn refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

withDebugInterface :: Lib -> (Ptr ID3D12Debug -> IO a) -> ComT IO a
withDebugInterface lib action = comT $ do
  e <- runComT $ getDebugInterface lib
  case e of
    Left e -> pure $ Left e
    Right p -> Right <$> withInterface p action
