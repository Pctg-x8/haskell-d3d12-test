{-# LANGUAGE TypeFamilies #-}
module Windows.Direct3D12.Device
  ( ID3D12Device, _D3D_FEATURE_LEVEL_12_0, createDevice, createCommandQueue, createCommandAllocator, createCommandList
  , createDescriptorHeap, getDescriptorHandleIncrementSize, createRenderTargetView, createFence
  ) where

import Windows.Types (HRESULT)
import Windows.ComBase (ComInterface(..))
import Windows.Com.IUnknown (IUnknown)
import Windows.Struct.GUID (GUID(..))
import Windows.LibLoader (Lib, getProcAddress)
import qualified Windows.Const.HResult as HR
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, WordPtr(..))
import Foreign.C.Types (CInt(..), CLong(..), CUInt(..))
import Foreign.Marshal.Alloc (alloca)
import Data.Word (Word64(..))
import Windows.Direct3D12.CommandQueue (ID3D12CommandQueue, CommandQueueDesc, CommandListType)
import Windows.Direct3D12.CommandAllocator (ID3D12CommandAllocator)
import Windows.Direct3D12.DescriptorHeap (ID3D12DescriptorHeap, DescriptorHeapDesc, DescriptorHeapType)
import Windows.Direct3D12.Resource (ID3D12Resource)
import Windows.Direct3D12.Fence (ID3D12Fence, FenceFlags)
import Windows.Struct.Direct3D12 (CPUDescriptorHandle(..))

data ID3D12DeviceVtbl
data ID3D12Device = ID3D12Device (Ptr ID3D12DeviceVtbl)
instance ComInterface ID3D12Device where
    type VTable ID3D12Device = ID3D12DeviceVtbl
    guid _ = GUID 0x189819f1 0x1db6 0x4b57 0xbe541821339b85f7
instance Storable ID3D12Device where
    sizeOf _ = 8
    alignment _ = 8
    peek p = ID3D12Device <$> peek (castPtr p)
    poke p (ID3D12Device vp) = castPtr p `poke` vp

_D3D_FEATURE_LEVEL_12_0 = 0xc000 :: Int

type PFN_D3D12CreateDevice = Ptr IUnknown -> CInt -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createDevice :: FunPtr PFN_D3D12CreateDevice -> PFN_D3D12CreateDevice
createDevice :: Lib -> Ptr a -> Int -> IO (Either HRESULT (Ptr ID3D12Device))
createDevice lib adapter minFeatureLevel =
  alloca $ \refiid ->
  alloca $ \ptr -> do
    fn <- dcall_createDevice <$> getProcAddress "D3D12CreateDevice" lib
    poke refiid $ guid (undefined :: Ptr ID3D12Device)
    hr <- fn (castPtr adapter) (fromIntegral minFeatureLevel) refiid (castPtr ptr)
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

type PFN_CreateDescribedObject desc = Ptr ID3D12Device -> Ptr desc -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT

_VTBL_INDEX_CREATE_COMMAND_QUEUE = 8
foreign import ccall "dynamic" dcall_createCommandQueue :: FunPtr (PFN_CreateDescribedObject CommandQueueDesc) -> PFN_CreateDescribedObject CommandQueueDesc
createCommandQueue :: Ptr ID3D12Device -> CommandQueueDesc -> IO (Either HRESULT (Ptr ID3D12CommandQueue))
createCommandQueue this desc =
  alloca $ \refiid ->
  alloca $ \ptr ->
  alloca $ \descref -> do
    fn <- dcall_createCommandQueue <$> getFunctionPtr _VTBL_INDEX_CREATE_COMMAND_QUEUE this
    poke refiid $ guid (undefined :: Ptr ID3D12CommandQueue)
    poke descref desc
    hr <- fn this descref refiid $ castPtr ptr
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

_VTBL_INDEX_CREATE_COMMAND_ALLOCATOR = 9
type PFN_CreateCommandAllocator = Ptr ID3D12Device -> CommandListType -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_CreateCommandAllocator :: FunPtr PFN_CreateCommandAllocator -> PFN_CreateCommandAllocator
createCommandAllocator :: Ptr ID3D12Device -> CommandListType -> IO (Either HRESULT (Ptr ID3D12CommandAllocator))
createCommandAllocator this ty =
  alloca $ \ptr ->
  alloca $ \refiid -> do
    fn <- dcall_CreateCommandAllocator <$> getFunctionPtr _VTBL_INDEX_CREATE_COMMAND_ALLOCATOR this
    poke refiid $ guid (undefined :: Ptr ID3D12CommandAllocator)
    hr <- fn this ty refiid $ castPtr ptr
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

_VTBL_INDEX_CREATE_COMMAND_LIST = 12
type PFN_CreateCommandList = Ptr ID3D12Device -> CUInt -> CommandListType -> Ptr ID3D12CommandAllocator -> Ptr () -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createCommandList :: FunPtr PFN_CreateCommandList -> PFN_CreateCommandList
createCommandList :: ComInterface c => Ptr ID3D12Device -> CUInt -> CommandListType -> Ptr ID3D12CommandAllocator -> Maybe (Ptr ()) -> IO (Either HRESULT (Ptr c))
createCommandList this nodeMask commandListType allocator initialState =
  alloca $ \ptr ->
  alloca $ \refiid -> do
    fn <- dcall_createCommandList <$> getFunctionPtr _VTBL_INDEX_CREATE_COMMAND_LIST this
    peek ptr >>= poke refiid . guid
    hr <- fn this nodeMask commandListType allocator (maybe nullPtr id initialState) refiid $ castPtr ptr
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

_VTBL_INDEX_CREATE_DESCRIPTOR_HEAP = 14
foreign import ccall "dynamic" dcall_createDescriptorHeap :: FunPtr (PFN_CreateDescribedObject DescriptorHeapDesc) -> PFN_CreateDescribedObject DescriptorHeapDesc
createDescriptorHeap :: Ptr ID3D12Device -> DescriptorHeapDesc -> IO (Either HRESULT (Ptr ID3D12DescriptorHeap))
createDescriptorHeap this desc =
  alloca $ \refiid ->
  alloca $ \ptr ->
  alloca $ \descref -> do
    fn <- dcall_createDescriptorHeap <$> getFunctionPtr _VTBL_INDEX_CREATE_DESCRIPTOR_HEAP this
    poke refiid $ guid (undefined :: Ptr ID3D12DescriptorHeap)
    poke descref desc
    hr <- fn this descref refiid $ castPtr ptr
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr

_VTBL_INDEX_GET_DESCRIPTOR_HANDLE_INCREMENT_SIZE = 15
foreign import ccall "dynamic" dcall_getDescriptorHandleIncrementSize :: FunPtr (Ptr ID3D12Device -> DescriptorHeapType -> IO CUInt) -> Ptr ID3D12Device -> DescriptorHeapType -> IO CUInt
getDescriptorHandleIncrementSize :: Ptr ID3D12Device -> DescriptorHeapType -> IO CUInt
getDescriptorHandleIncrementSize this ty = getFunctionPtr _VTBL_INDEX_GET_DESCRIPTOR_HANDLE_INCREMENT_SIZE this >>= \f -> dcall_getDescriptorHandleIncrementSize f this ty

_VTBL_INDEX_CREATE_RENDER_TARGET_VIEW = 20
type PFN_CreateRenderTargetView = Ptr ID3D12Device -> Ptr ID3D12Resource -> Ptr () -> CPUDescriptorHandle -> IO ()
foreign import ccall "dynamic" dcall_createRenderTargetView :: FunPtr PFN_CreateRenderTargetView -> PFN_CreateRenderTargetView
createRenderTargetView :: Ptr ID3D12Device -> Ptr ID3D12Resource -> CPUDescriptorHandle -> IO ()
createRenderTargetView this resource dest = getFunctionPtr _VTBL_INDEX_CREATE_RENDER_TARGET_VIEW this >>= \f -> dcall_createRenderTargetView f this resource nullPtr dest

_VTBL_INDEX_CREATE_FENCE = 36
type PFN_CreateFence = Ptr ID3D12Device -> Word64 -> FenceFlags -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createFence :: FunPtr PFN_CreateFence -> PFN_CreateFence
createFence :: Ptr ID3D12Device -> Word64 -> FenceFlags -> IO (Either HRESULT (Ptr ID3D12Fence))
createFence this initValue flags =
  alloca $ \refiid ->
  alloca $ \ptr -> do
    fn <- dcall_createFence <$> getFunctionPtr _VTBL_INDEX_CREATE_FENCE this
    poke refiid $ guid (undefined :: Ptr ID3D12Fence)
    hr <- fn this initValue flags refiid $ castPtr ptr
    if HR.isSucceeded hr then Right <$> peek ptr else pure $ Left hr
