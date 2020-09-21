module Main where

import Windows
import qualified Windows.Struct.WndClassEx as WndClassEx
import Windows.Struct.Msg (MSG)
import qualified Windows.Struct.Msg as MsgStruct
import qualified Windows.Flags.ExWindowStyle as ExWindowStyle
import qualified Windows.Flags.WindowStyle as WindowStyle
import Windows.Types
import Windows.LibLoader
import qualified Windows.Const.WindowMessage as WindowMessage
import Windows.ComBase (ComInterface)
import Windows.Com.IUnknown (withInterface)
import qualified Windows.Const.Dxgi as DxgiConst
import qualified Windows.Struct.Dxgi as DxgiStruct
import qualified Windows.Dxgi.Factory2 as IDXGIFactory2
import qualified Windows.Dxgi.Adapter as IDXGIAdapter
import qualified Windows.Dxgi.SwapChain3 as IDXGISwapChain3
import qualified Windows.Direct3D12.Debug as ID3D12Debug
import qualified Windows.Direct3D12.Device as ID3D12Device
import qualified Windows.Direct3D12.CommandQueue as ID3D12CommandQueue
import qualified Windows.Direct3D12.DescriptorHeap as ID3D12DescriptorHeap
import qualified Windows.Direct3D12.GraphicsCommandList as ID3D12GraphicsCommandList
import qualified Windows.Direct3D12.Fence as ID3D12Fence
import qualified Windows.Direct3D12.Heap as ID3D12Heap
import qualified Windows.Direct3D12.Resource as ID3D12Resource
import qualified Windows.Direct3D12.RootSignature as ID3D12RootSignature
import qualified Windows.Direct3D12.PipelineState as ID3D12PipelineState
import qualified Windows.D3DBlob as ID3DBlob
import Windows.Direct3D12.GraphicsCommandList (ResourceBarrier(ResourceTransitionBarrier))
import Windows.Direct3D12.Resource (resourceStatePresent, resourceStateRenderTarget)
import Windows.Const.Direct3D12 (primitiveTopologyTriangleList)
import Windows.Struct.Direct3D12 (plusCPUDescriptorHandle, Viewport(..), Range(..), VertexBufferView(..))
import Windows.Struct.Rect (Rect(..))
import Windows.Com.Monad (ComT, runComT, ComError(..))
import qualified Windows.DirectComposition.DesktopDevice as IDCompositionDesktopDevice
import qualified Windows.DirectComposition.Device3 as IDCompositionDevice3
import qualified Windows.DirectComposition.Target as IDCompositionTarget
import qualified Windows.DirectComposition.Visual2 as IDCompositionVisual2
import Data.Array.MArray (newListArray)
import Data.Array.Storable (touchStorableArray)
import Data.Functor (($>))
import Control.Monad (when, forM, forM_)
import Control.Exception (Exception, throwIO)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import Foreign.C.String (withCString)
import Foreign.C.Types (CUInt, CULong, CFloat)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Data.Bits ((.|.))
import Data.Maybe (maybe)
import Data.IORef (newIORef, modifyIORef, readIORef)
import qualified Data.ByteString as B
import Numeric (showHex)
import Data.Bifunctor (bimap)

data PlatformException = RegisterClassExFailed | WindowCreationFailed | ComErrorDetail String ComError | EventCreationFailed
instance Show PlatformException where
  show RegisterClassExFailed = "RegisterClassExFailed"
  show WindowCreationFailed = "WindowCreationFailed"
  show (ComErrorDetail info e) = "ComError[" ++ info ++ "] " ++ show e
  show EventCreationFailed = "EventCreationFailed"
instance Exception PlatformException

ensureSuccessComIO :: String -> Either ComError a -> IO a
ensureSuccessComIO info = either (throwIO . ComErrorDetail info) pure

runComTWithDiverge :: String -> ComT IO a -> IO a
runComTWithDiverge info a = runComT a >>= either (throwIO . ComErrorDetail info) pure

-- | using interface while continuation chain. auto released after chain was finished
useInterface :: ComInterface a => Ptr a -> ContT r IO (Ptr a)
useInterface = ContT . withInterface

data ColorVertex = ColorVertex CFloat CFloat CFloat CFloat CFloat CFloat
instance Storable ColorVertex where
  sizeOf _ = 4 * 6
  alignment _ = 4
  peek p = ColorVertex <$>
    peek (castPtr p) <*>
    peek (plusPtr p 4) <*>
    peek (plusPtr p 8) <*>
    peek (plusPtr p 12) <*>
    peek (plusPtr p 16) <*>
    peek (plusPtr p 20)
  poke p (ColorVertex x y r g b a) = do
    poke (castPtr p   ) x
    poke (plusPtr p  4) y
    poke (plusPtr p  8) r
    poke (plusPtr p 12) g
    poke (plusPtr p 16) b
    poke (plusPtr p 20) a

main :: IO ()
main = do
  wcb <- WndClassEx.makeWndProc callback
  window <- withCString "jp.ct2.hs.d3d12.MainWindow" $ \className -> do
    wc <- WndClassEx.setWndProc wcb <$> WndClassEx.defaultWndClassEx className
    registerClassEx wc >>= maybe (throwIO RegisterClassExFailed) pure
    wh <- withCString "d3d12 on Haskell" $ \windowName -> createWindowEx
      (ExWindowStyle.appWindow .|. ExWindowStyle.noRedirectionBitmap)
      className windowName
      WindowStyle.overlappedWindow
      Nothing Nothing Nothing Nothing
      Nothing Nothing (WndClassEx.hInstance wc)
    maybe (throwIO WindowCreationFailed) pure wh
  
  withLibrary "dxgi" $ \d -> flip runContT pure $ do
    dxgifactory <- useInterface =<< lift (runComTWithDiverge "IDXGIFactory2.createDxgiFactory2" $ IDXGIFactory2.createDxgiFactory2 d True :: IO (Ptr IDXGIFactory2.IDXGIFactory2))
    adapter <- useInterface =<< lift (runComTWithDiverge "IDXGIFactory2.enumAdapters" $ IDXGIFactory2.enumAdapters dxgifactory 0)
    d12 <- ContT $ withLibrary "d3d12"
    lift $ runComTWithDiverge "Enabling D3D12 Debug" $ ID3D12Debug.withDebugInterface d12 ID3D12Debug.enableDebugLayer
    device12 <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createDevice" $ ID3D12Device.createDevice d12 adapter ID3D12Device._D3D_FEATURE_LEVEL_12_0)
    let cqDesc = ID3D12CommandQueue.defaultCommandQueueDesc ID3D12CommandQueue.commandListTypeDirect
    q <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createCommandQueue" $ ID3D12Device.createCommandQueue device12 cqDesc)
    let scDesc = DxgiStruct.defaultSwapChainDesc1 (640, 480) DxgiConst.formatR8G8B8A8Unorm DxgiConst.usageRenderTargetOutput 2 DxgiConst.swapEffectFlipDiscard
    swapchain <- useInterface =<< lift (runComTWithDiverge "IDXGIFactory2.createSwapChainForComposition" $ IDXGIFactory2.createSwapChainForComposition dxgifactory q scDesc)
    let rtvHeapDesc = ID3D12DescriptorHeap.defaultDescriptorHeapDesc ID3D12DescriptorHeap.descriptorHeapTypeRTV 2
    rtvHeaps <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createDescriptorHeap" $ ID3D12Device.createDescriptorHeap device12 rtvHeapDesc)
    rtvDescSize <- lift $ ID3D12Device.getDescriptorHandleIncrementSize device12 ID3D12DescriptorHeap.descriptorHeapTypeRTV
    rtvDescBase <- lift $ ID3D12DescriptorHeap.getCPUDescriptorHandleForHeapStart rtvHeaps
    forM_ [0..1] $ \bbx -> do
      bb <- useInterface =<< lift (runComTWithDiverge "IDXGISwapChain3.getBuffer" $ IDXGISwapChain3.getBuffer swapchain bbx)
      let destHandle = plusCPUDescriptorHandle rtvDescBase $ bbx * fromIntegral rtvDescSize
      lift $ ID3D12Device.createRenderTargetView device12 bb destHandle
    
    let heapDesc = ID3D12Heap.HeapDesc (fromIntegral $ sizeOf (undefined :: ColorVertex) * 3) ID3D12Heap.heapDefaultProperties 0 ID3D12Heap.heapFlagAllowOnlyBuffers 
    heap <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createHeap" $ ID3D12Device.createHeap device12 heapDesc)
    let rdBuffer = ID3D12Resource.ResourceDesc
          ID3D12Resource.resourceDimensionBuffer 0 (fromIntegral $ sizeOf (undefined :: ColorVertex) * 3) 1 1 1 DxgiConst.formatUnknown DxgiStruct.defaultSampleDesc
          ID3D12Resource.textureLayoutRowMajor 0
    res <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createPlacedResource" $ ID3D12Device.createPlacedResource device12 heap 0 rdBuffer ID3D12Resource.resourceStateCopyDest Nothing)
    let stgSize = sizeOf (undefined :: ColorVertex) * 3
    let stgHeapDesc = ID3D12Heap.HeapDesc (fromIntegral stgSize) ID3D12Heap.heapUploadProperties 0 ID3D12Heap.heapFlagAllowOnlyBuffers
    stgHeap <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createHeap stg" $ ID3D12Device.createHeap device12 stgHeapDesc)
    let rdStgBuffer = ID3D12Resource.ResourceDesc
          ID3D12Resource.resourceDimensionBuffer 0 (fromIntegral stgSize) 1 1 1 DxgiConst.formatUnknown DxgiStruct.defaultSampleDesc
          ID3D12Resource.textureLayoutRowMajor 0
    resStg <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createPlacedResource stg" $ ID3D12Device.createPlacedResource device12 stgHeap 0 rdStgBuffer ID3D12Resource.resourceStateGenericRead Nothing)
    ptr <- lift $ runComTWithDiverge "ID3D12Resource.map" $ ID3D12Resource.map resStg 0 (Range 0 0)
    lift $ poke (castPtr ptr) $ ColorVertex 0.0 0.5 1.0 1.0 1.0 1.0
    lift $ poke (plusPtr ptr $ sizeOf (undefined :: ColorVertex)) $ ColorVertex 0.5 (-0.5) 1.0 0.0 1.0 1.0
    lift $ poke (plusPtr ptr $ sizeOf (undefined :: ColorVertex) * 2) $ ColorVertex (-0.5) (-0.5) 0.0 1.0 1.0 1.0
    lift $ ID3D12Resource.unmap resStg 0 $ Range 0 $ fromIntegral stgSize
    initWait <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createFence" $ ID3D12Device.createFence device12 0 0)
    initWaitEvent <- (ContT . withEvent) =<< (lift $ createEvent Nothing False False "InitWait" >>= maybe (throwIO EventCreationFailed) pure)
    initCmdAlloc <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createCommandAllocator" $ ID3D12Device.createCommandAllocator device12 ID3D12CommandQueue.commandListTypeDirect)
    initCmdBuffer <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createCommandList" $ ID3D12Device.createCommandList device12 0 ID3D12CommandQueue.commandListTypeDirect initCmdAlloc Nothing)
    outBarriers <- lift $ newListArray (0, 0) [ResourceTransitionBarrier 0 res 0 ID3D12Resource.resourceStateCopyDest ID3D12Resource.resourceStateVertexAndConstantBuffer]
    lift $ ID3D12GraphicsCommandList.copyBufferRegion initCmdBuffer res 0 resStg 0 (fromIntegral $ sizeOf (undefined :: ColorVertex) * 3)
    lift $ ID3D12GraphicsCommandList.resourceBarrier initCmdBuffer outBarriers
    lift $ runComTWithDiverge "ID3D12GraphicsCommandList.close transfer" $ ID3D12GraphicsCommandList.close initCmdBuffer
    lists <- lift $ newListArray (0, 0) [initCmdBuffer]
    lift $ ID3D12CommandQueue.executeCommandLists q lists
    lift $ runComTWithDiverge "ID3D12CommandQueue.signal" $ ID3D12CommandQueue.signal q initWait 1
    lift $ runComTWithDiverge "ID3D12Fence.setEventOnCompletion" $ ID3D12Fence.setEventOnCompletion initWait 1 $ eventAsHandle initWaitEvent
    vbufLocation <- lift $ ID3D12Resource.getGPUVirtualAddress res
    let vbufView = VertexBufferView vbufLocation (fromIntegral $ sizeOf (undefined :: ColorVertex) * 3) (fromIntegral $ sizeOf (undefined :: ColorVertex))

    vshCode <- lift $ B.readFile "shaders/pass_color.vsho"
    pshCode <- lift $ B.readFile "shaders/fill.psho"
    vshCodePacked <- ContT $ ID3D12PipelineState.withNewShaderBytecode vshCode
    pshCodePacked <- ContT $ ID3D12PipelineState.withNewShaderBytecode pshCode
    let emptyRootSignatureDesc = ID3D12RootSignature.RootSignatureDesc 0 nullPtr 0 nullPtr ID3D12RootSignature.rootSignatureFlagAllowInputAssemblerInputLayout
    emptyRootSignature <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.serializeRootSignature" (ID3D12RootSignature.serializeRootSignature d12 emptyRootSignatureDesc ID3D12RootSignature.rootSignatureVersion1) >>= flip withInterface (\blob -> do
      ptr <- ID3DBlob.getBufferPointer blob
      size <- ID3DBlob.getBufferSize blob
      runComTWithDiverge "ID3D12Device.createRootSignature" $ ID3D12Device.createRootSignature device12 0 ptr size))
    semanticNamePosition <- ContT $ withCString "POSITION"
    semanticNameColor <- ContT $ withCString "COLOR"
    inputElements <- ContT $ withArray
      [ ID3D12PipelineState.InputElementDesc semanticNamePosition 0 DxgiConst.formatR32G32Float 0 0 ID3D12PipelineState.inputClassificationPerVertex 0
      , ID3D12PipelineState.InputElementDesc semanticNameColor 0 DxgiConst.formatR32G32B32A32Float 0 (4 * 2) ID3D12PipelineState.inputClassificationPerVertex 0
      ]
    let pipelineStateDesc = ID3D12PipelineState.GraphicsPipelineStateDesc
          emptyRootSignature
          vshCodePacked
          pshCodePacked
          ID3D12PipelineState.emptyShaderBytecode
          ID3D12PipelineState.emptyShaderBytecode
          ID3D12PipelineState.emptyShaderBytecode
          ID3D12PipelineState.streamOutputDescDisabled
          (ID3D12PipelineState.BlendDesc False False [ID3D12PipelineState.renderTargetBlendDescDisabled])
          -- ここはこれじゃないとBlendタイミングでなにもサンプリングされなくなって黒くなる
          0xffffffff
          ID3D12PipelineState.defaultRasterizerState
          ID3D12PipelineState.depthStencilDescDisabled
          (ID3D12PipelineState.InputLayoutDesc inputElements 2)
          ID3D12PipelineState.indexBufferStripCutValueDisabled
          ID3D12PipelineState.primitiveTopologyTypeTriangle
          1
          [DxgiConst.formatR8G8B8A8Unorm]
          DxgiConst.formatUnknown
          DxgiStruct.defaultSampleDesc
          0
          ID3D12PipelineState.emptyCachedPipelineState
          0
    pipeline <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createGraphicsPipelineState" $ ID3D12Device.createGraphicsPipelineState device12 pipelineStateDesc)
    
    cmdwait <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createFence" $ ID3D12Device.createFence device12 0 0)
    cmdwaitEvent <- (ContT . withEvent) =<< (lift $ createEvent Nothing False True "CommandWait" >>= maybe (throwIO EventCreationFailed) pure)
    cmdalloc <- useInterface =<< lift (runComTWithDiverge "ID3D12Device.createCommandAllocator" $ ID3D12Device.createCommandAllocator device12 ID3D12CommandQueue.commandListTypeDirect)
    viewports <- lift $ newListArray (0, 0) [Viewport 0.0 0.0 640.0 480.0 0.0 1.0]
    scissors <- lift $ newListArray (0, 0) [Rect 0 0 640 480]
    renderCommands <- forM [0..1] $ \bbx ->
      useInterface =<< lift (runContT (useInterface =<< lift (runComTWithDiverge "IDXGISwapChain3.getBuffer Rendering" $ IDXGISwapChain3.getBuffer swapchain bbx)) (\bb ->
        runComTWithDiverge "Building RenderCommands" $ do
          cb <- ID3D12Device.createCommandList device12 0 ID3D12CommandQueue.commandListTypeDirect cmdalloc (Just pipeline)
          let rtvMain = plusCPUDescriptorHandle rtvDescBase $ bbx * fromIntegral rtvDescSize
          inBarriers <- lift $ newListArray (0, 0) [ResourceTransitionBarrier 0 bb 0 resourceStatePresent resourceStateRenderTarget]
          outBarriers <- lift $ newListArray (0, 0) [ResourceTransitionBarrier 0 bb 0 resourceStateRenderTarget resourceStatePresent]
          renderTargets <- lift $ newListArray (0, 0) [rtvMain]
          vbufs <- lift $ newListArray (0, 0) [vbufView]
          lift $ sequence_
            [ ID3D12GraphicsCommandList.resourceBarrier cb inBarriers
            , ID3D12GraphicsCommandList.setRenderTargets cb renderTargets Nothing
            , ID3D12GraphicsCommandList.setViewports cb viewports
            , ID3D12GraphicsCommandList.setScissorRects cb scissors
            , ID3D12GraphicsCommandList.clearEntireRenderTargetView cb rtvMain (0.0, 0.0, 0.0, 1.0)
            , ID3D12GraphicsCommandList.setGraphicsRootSignature cb emptyRootSignature
            , ID3D12GraphicsCommandList.setPrimitiveTopology cb primitiveTopologyTriangleList
            , ID3D12GraphicsCommandList.setVertexBuffers cb 0 vbufs
            , ID3D12GraphicsCommandList.drawInstanced cb 3 1 0 0
            , ID3D12GraphicsCommandList.resourceBarrier cb outBarriers
            ]
          ID3D12GraphicsCommandList.close cb
          pure cb))
    
    dclib <- ContT $ withLibrary "dcomp"
    dcDevice <- useInterface =<< lift (runComTWithDiverge "IDCompositionDevice3.createDevice" $ IDCompositionDevice3.createDevice dclib Nothing)
    dcTarget <- useInterface =<< lift (runComTWithDiverge "IDCompositionDesktopDevice.createTargetForHwnd" $ IDCompositionDesktopDevice.createTargetForHwnd dcDevice window False)
    dcVisual <- useInterface =<< lift (runComTWithDiverge "IDCompositionDesktopDevice.createVisual" $ IDCompositionDesktopDevice.createVisual dcDevice)
    lift $ runComTWithDiverge "Setting up Composition" $ do
      IDCompositionVisual2.setContent dcVisual swapchain
      IDCompositionTarget.setRoot dcTarget dcVisual
      IDCompositionDesktopDevice.commit dcDevice
    
    lift $ waitForSingleObject (eventAsHandle initWaitEvent) Nothing
    frameCounter <- lift $ newIORef 0

    lift $ showWindow window _SW_SHOWNORMAL
    lift $ alloca $ \x -> loop x cmdwaitEvent $ runComTWithDiverge "Rendering" $ do
      IDXGISwapChain3.present swapchain 0 0
      frameCount <- lift $ readIORef frameCounter
      current <- lift $ IDXGISwapChain3.getCurrentBackBufferIndex swapchain
      lists <- lift $ newListArray (0, 0) [renderCommands !! current]
      lift (ID3D12CommandQueue.executeCommandLists q lists) >> ID3D12CommandQueue.signal q cmdwait frameCount
      ID3D12Fence.setEventOnCompletion cmdwait frameCount $ eventAsHandle cmdwaitEvent
      lift $ modifyIORef frameCounter (+ 1)

pumpMessages :: Ptr MSG -> IO Bool
pumpMessages lpmsg = do
  r <- peekMessage lpmsg nullPtr Nothing Remove
  if r then do
    msgType <- MsgStruct.peekMessage lpmsg
    if msgType == WindowMessage.quit then
      pure False
    else
      translateMessage lpmsg >> dispatchMessage lpmsg >> pumpMessages lpmsg
  else
    pure True

loop :: Ptr MSG -> Event -> IO () -> IO ()
loop lpmsg renderSignal renderfn = do
  a <- newListArray (0, 0) [eventAsHandle renderSignal]
  r <- msgWaitForMultipleObjects a Nothing wakeMaskAllEvents waitFlagInputAvailable
  case r of
    NewInputAvailable -> do
      cont <- pumpMessages lpmsg
      when cont $ loop lpmsg renderSignal renderfn
    Signal 0 -> renderfn >> loop lpmsg renderSignal renderfn
    _ -> loop lpmsg renderSignal renderfn

callback :: HWND -> CUInt -> WPARAM -> LPARAM -> IO LRESULT
callback hWnd uMsg wParam lParam
  | uMsg == WindowMessage.destroy = postQuitMessage 0 >> pure (fromIntegral 0)
  | otherwise = defWindowProc hWnd uMsg wParam lParam
