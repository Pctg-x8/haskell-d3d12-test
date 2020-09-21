{-# LANGUAGE TypeFamilies #-}
module Windows.Direct3D12.Device
  ( ID3D12Device, _D3D_FEATURE_LEVEL_12_0, createDevice, createCommandQueue, createCommandAllocator, createCommandList
  , createDescriptorHeap, getDescriptorHandleIncrementSize, createRenderTargetView, createFence
  , createHeap, createPlacedResource, createRootSignature, createGraphicsPipelineState
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
import qualified Foreign.Marshal.Utils as Marshal
import Data.Word (Word64(..))
import Data.Functor (($>))
import Windows.Direct3D12.CommandQueue (ID3D12CommandQueue, CommandQueueDesc, CommandListType)
import Windows.Direct3D12.CommandAllocator (ID3D12CommandAllocator)
import Windows.Direct3D12.DescriptorHeap (ID3D12DescriptorHeap, DescriptorHeapDesc, DescriptorHeapType)
import Windows.Direct3D12.Resource (ID3D12Resource, ResourceDesc, ResourceStates)
import Windows.Direct3D12.Fence (ID3D12Fence, FenceFlags)
import Windows.Direct3D12.Heap (ID3D12Heap, HeapDesc)
import Windows.Direct3D12.RootSignature (ID3D12RootSignature)
import Windows.Direct3D12.PipelineState (ID3D12PipelineState, GraphicsPipelineStateDesc)
import Windows.Struct.Direct3D12 (CPUDescriptorHandle(..), ClearValue)
import Control.Monad.Cont (ContT(..))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Windows.Com.Monad (ComT(..), comT, runComT, handleHRESULT)

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

allocPtr :: Storable a => ContT r (ComT IO) (Ptr a)
allocPtr = ContT $ \f -> comT $ alloca $ runComT . f
withPtr :: Storable a => a -> ContT r (ComT IO) (Ptr a)
withPtr v = ContT $ \f -> comT $ Marshal.with v $ runComT . f

type PFN_D3D12CreateDevice = Ptr IUnknown -> CInt -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createDevice :: FunPtr PFN_D3D12CreateDevice -> PFN_D3D12CreateDevice
createDevice :: Lib -> Ptr a -> Int -> ComT IO (Ptr ID3D12Device)
createDevice lib adapter minFeatureLevel = flip runContT pure $ do
  refiid <- withPtr $ guid (undefined :: Ptr ID3D12Device)
  ptr <- allocPtr
  fn <- lift $ lift $ dcall_createDevice <$> getProcAddress "D3D12CreateDevice" lib
  hr <- lift $ lift $ fn (castPtr adapter) (fromIntegral minFeatureLevel) refiid (castPtr ptr)
  lift $ handleHRESULT hr >> lift (peek ptr)

-- Common Interface for Factory methods with its Description Structure
type PFN_CreateDescribedObject desc = Ptr ID3D12Device -> Ptr desc -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createDescribedObject :: FunPtr (PFN_CreateDescribedObject d) -> (PFN_CreateDescribedObject d)
createDescribedObject :: (ComInterface i, Storable d) => Int -> Ptr ID3D12Device -> d -> ComT IO (Ptr i)
createDescribedObject fnIndex this desc = flip runContT pure $ do
  ptr <- allocPtr
  descref <- withPtr desc
  refiid <- liftIO (peek ptr) >>= withPtr . guid
  fn <- liftIO $ dcall_createDescribedObject <$> getFunctionPtr fnIndex this
  hr <- liftIO $ fn this descref refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

_VTBL_INDEX_CREATE_COMMAND_QUEUE = 8
createCommandQueue :: Ptr ID3D12Device -> CommandQueueDesc -> ComT IO (Ptr ID3D12CommandQueue)
createCommandQueue = createDescribedObject _VTBL_INDEX_CREATE_COMMAND_QUEUE

_VTBL_INDEX_CREATE_COMMAND_ALLOCATOR = 9
type PFN_CreateCommandAllocator = Ptr ID3D12Device -> CommandListType -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createCommandAllocator :: FunPtr PFN_CreateCommandAllocator -> PFN_CreateCommandAllocator
createCommandAllocator :: Ptr ID3D12Device -> CommandListType -> ComT IO (Ptr ID3D12CommandAllocator)
createCommandAllocator this ty = flip runContT pure $ do
  ptr <- allocPtr
  refiid <- withPtr $ guid (undefined :: Ptr ID3D12CommandAllocator)
  fn <- liftIO $ dcall_createCommandAllocator <$> getFunctionPtr _VTBL_INDEX_CREATE_COMMAND_ALLOCATOR this
  hr <- liftIO $ fn this ty refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

_VTBL_INDEX_CREATE_GRAPHICS_PIPELINE_STATE = 10
createGraphicsPipelineState :: Ptr ID3D12Device -> GraphicsPipelineStateDesc -> ComT IO (Ptr ID3D12PipelineState)
createGraphicsPipelineState = createDescribedObject _VTBL_INDEX_CREATE_GRAPHICS_PIPELINE_STATE

_VTBL_INDEX_CREATE_COMMAND_LIST = 12
type PFN_CreateCommandList = Ptr ID3D12Device -> CUInt -> CommandListType -> Ptr ID3D12CommandAllocator -> Ptr () -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createCommandList :: FunPtr PFN_CreateCommandList -> PFN_CreateCommandList
createCommandList :: ComInterface c => Ptr ID3D12Device -> CUInt -> CommandListType -> Ptr ID3D12CommandAllocator -> Maybe (Ptr ID3D12PipelineState) -> ComT IO (Ptr c)
createCommandList this nodeMask commandListType allocator initialState = flip runContT pure $ do
  ptr <- allocPtr
  refiid <- liftIO (peek ptr) >>= withPtr . guid
  fn <- liftIO $ dcall_createCommandList <$> getFunctionPtr _VTBL_INDEX_CREATE_COMMAND_LIST this
  hr <- liftIO $ fn this nodeMask commandListType allocator (maybe nullPtr castPtr initialState) refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

_VTBL_INDEX_CREATE_DESCRIPTOR_HEAP = 14
createDescriptorHeap :: Ptr ID3D12Device -> DescriptorHeapDesc -> ComT IO (Ptr ID3D12DescriptorHeap)
createDescriptorHeap = createDescribedObject _VTBL_INDEX_CREATE_DESCRIPTOR_HEAP

_VTBL_INDEX_GET_DESCRIPTOR_HANDLE_INCREMENT_SIZE = 15
foreign import ccall "dynamic" dcall_getDescriptorHandleIncrementSize :: FunPtr (Ptr ID3D12Device -> DescriptorHeapType -> IO CUInt) -> Ptr ID3D12Device -> DescriptorHeapType -> IO CUInt
getDescriptorHandleIncrementSize :: Ptr ID3D12Device -> DescriptorHeapType -> IO CUInt
getDescriptorHandleIncrementSize this ty = getFunctionPtr _VTBL_INDEX_GET_DESCRIPTOR_HANDLE_INCREMENT_SIZE this >>= \f -> dcall_getDescriptorHandleIncrementSize f this ty

_VTBL_INDEX_CREATE_ROOT_SIGNATURE = 16
type PFN_CreateRootSignature = Ptr ID3D12Device -> CUInt -> Ptr () -> WordPtr -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createRootSignature :: FunPtr PFN_CreateRootSignature -> PFN_CreateRootSignature
createRootSignature :: Ptr ID3D12Device -> CUInt -> Ptr () -> WordPtr -> ComT IO (Ptr ID3D12RootSignature)
createRootSignature this nodeMask blob blobLength = flip runContT pure $ do
  ptr <- allocPtr
  refiid <- withPtr $ guid (undefined :: Ptr ID3D12RootSignature)
  fn <- liftIO $ dcall_createRootSignature <$> getFunctionPtr _VTBL_INDEX_CREATE_ROOT_SIGNATURE this
  hr <- liftIO $ fn this nodeMask blob blobLength refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

_VTBL_INDEX_CREATE_RENDER_TARGET_VIEW = 20
type PFN_CreateRenderTargetView = Ptr ID3D12Device -> Ptr ID3D12Resource -> Ptr () -> CPUDescriptorHandle -> IO ()
foreign import ccall "dynamic" dcall_createRenderTargetView :: FunPtr PFN_CreateRenderTargetView -> PFN_CreateRenderTargetView
createRenderTargetView :: Ptr ID3D12Device -> Ptr ID3D12Resource -> CPUDescriptorHandle -> IO ()
createRenderTargetView this resource dest = getFunctionPtr _VTBL_INDEX_CREATE_RENDER_TARGET_VIEW this >>= \f -> dcall_createRenderTargetView f this resource nullPtr dest

_VTBL_INDEX_CREATE_HEAP = 28
createHeap :: Ptr ID3D12Device -> HeapDesc -> ComT IO (Ptr ID3D12Heap)
createHeap = createDescribedObject _VTBL_INDEX_CREATE_HEAP

_VTBL_INDEX_CREATE_PLACED_RESOURCE = 29
type PFN_CreatePlacedResource = Ptr ID3D12Device -> Ptr ID3D12Heap -> Word64 -> Ptr ResourceDesc -> ResourceStates -> Ptr ClearValue -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createPlacedResource :: FunPtr PFN_CreatePlacedResource -> PFN_CreatePlacedResource
createPlacedResource :: Ptr ID3D12Device -> Ptr ID3D12Heap -> Word64 -> ResourceDesc -> ResourceStates -> Maybe ClearValue -> ComT IO (Ptr ID3D12Resource)
createPlacedResource this heap offset desc initState optimalClearValue = flip runContT pure $ do
  ptr <- allocPtr
  refiid <- withPtr $ guid (undefined :: Ptr ID3D12Resource)
  refdesc <- withPtr desc
  refcv <- maybe (pure nullPtr) withPtr optimalClearValue
  fn <- liftIO $ dcall_createPlacedResource <$> getFunctionPtr _VTBL_INDEX_CREATE_PLACED_RESOURCE this
  hr <- liftIO $ fn this heap offset refdesc initState refcv refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)

_VTBL_INDEX_CREATE_FENCE = 36
type PFN_CreateFence = Ptr ID3D12Device -> Word64 -> FenceFlags -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT
foreign import ccall "dynamic" dcall_createFence :: FunPtr PFN_CreateFence -> PFN_CreateFence
createFence :: Ptr ID3D12Device -> Word64 -> FenceFlags -> ComT IO (Ptr ID3D12Fence)
createFence this initValue flags = flip runContT pure $ do
  refiid <- withPtr $ guid (undefined :: Ptr ID3D12Fence)
  ptr <- allocPtr
  fn <- liftIO $ dcall_createFence <$> getFunctionPtr _VTBL_INDEX_CREATE_FENCE this
  hr <- liftIO $ fn this initValue flags refiid $ castPtr ptr
  lift $ handleHRESULT hr >> lift (peek ptr)
