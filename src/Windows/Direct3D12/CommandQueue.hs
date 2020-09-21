{-# LANGUAGE TypeFamilies #-}

module Windows.Direct3D12.CommandQueue
  ( ID3D12CommandQueue, signal, executeCommandLists, CommandQueueDesc(..), defaultCommandQueueDesc
  , CommandListType, commandListTypeDirect, commandListTypeBundle, commandListTypeCompute, commandListTypeCopy
  , CommandQueueFlags
  ) where

import Windows.Types (HRESULT)
import Windows.Struct.GUID (GUID(..))
import Windows.ComBase (ComInterface(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr)
import Foreign.C.Types (CInt(..), CUInt(..), CLong(..))
import Data.Word (Word64(..))
import Windows.Direct3D12.Fence (ID3D12Fence)
import Windows.Direct3D12.GraphicsCommandList (ID3D12GraphicsCommandList)
import Data.Ix (Ix)
import Data.Array.Base (getNumElements)
import Data.Array.Storable (StorableArray, withStorableArray)
import Windows.Com.Monad (ComT, handleHRESULT)
import Control.Monad.IO.Class (liftIO)

data ID3D12CommandQueueVtbl
data ID3D12CommandQueue = ID3D12CommandQueue (Ptr ID3D12CommandQueueVtbl)
instance ComInterface ID3D12CommandQueue where
  type VTable ID3D12CommandQueue = ID3D12CommandQueueVtbl
  guid _ = GUID 0x0ec870a6 0x5d7e 0x4c22 0x8cfc5baae07616ed
instance Storable ID3D12CommandQueue where
  sizeOf _ = 8
  alignment _ = 8
  peek p = ID3D12CommandQueue <$> peek (castPtr p)
  poke p (ID3D12CommandQueue vp) = castPtr p `poke` vp

type CommandListType = CUInt
commandListTypeDirect, commandListTypeBundle, commandListTypeCompute, commandListTypeCopy :: CommandListType
commandListTypeDirect = 0
commandListTypeBundle = 1
commandListTypeCompute = 2
commandListTypeCopy = 3
type CommandQueueFlags = CUInt
data CommandQueueDesc = CommandQueueDesc { descType :: CommandListType, descPriority :: CInt, descFlags :: CommandQueueFlags, descNodeMask :: CUInt }
instance Storable CommandQueueDesc where
  sizeOf _ = 4 * 4
  alignment _ = 4
  peek p = CommandQueueDesc <$>
    peek (castPtr p) <*>
    peek (plusPtr p 4) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 12)
  poke p (CommandQueueDesc ty priority flags nodeMask) = do
    castPtr p `poke` ty
    plusPtr p 4 `poke` priority
    plusPtr p 8 `poke` flags
    plusPtr p 12 `poke` nodeMask

defaultCommandQueueDesc :: CommandListType -> CommandQueueDesc
defaultCommandQueueDesc ty = CommandQueueDesc ty 0 0 0

_VTBL_INDEX_EXECUTE_COMMAND_LISTS = 10
type PFN_ExecuteCommandLists = Ptr ID3D12CommandQueue -> CUInt -> Ptr (Ptr ID3D12GraphicsCommandList) -> IO ()
foreign import ccall "dynamic" dcall_executeCommandLists :: FunPtr PFN_ExecuteCommandLists -> PFN_ExecuteCommandLists
executeCommandLists :: Ix i => Ptr ID3D12CommandQueue -> StorableArray i (Ptr ID3D12GraphicsCommandList) -> IO ()
executeCommandLists this lists = withStorableArray lists $ \listref -> do
  fn <- dcall_executeCommandLists <$> getFunctionPtr _VTBL_INDEX_EXECUTE_COMMAND_LISTS this
  count <- fromIntegral <$> getNumElements lists
  fn this count listref

_VTBL_INDEX_SIGNAL = 14
type PFN_Signal = Ptr ID3D12CommandQueue -> Ptr ID3D12Fence -> Word64 -> IO HRESULT
foreign import ccall "dynamic" dcall_signal :: FunPtr PFN_Signal -> PFN_Signal
signal :: Ptr ID3D12CommandQueue -> Ptr ID3D12Fence -> Word64 -> ComT IO ()
signal this fence value = liftIO (getFunctionPtr _VTBL_INDEX_SIGNAL this >>= \f -> dcall_signal f this fence value) >>= handleHRESULT
