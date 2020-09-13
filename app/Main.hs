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
import Windows.Direct3D12.GraphicsCommandList (ResourceBarrier(ResourceTransitionBarrier), resourceStatePresent, resourceStateRenderTarget)
import Windows.Struct.Direct3D12 (plusCPUDescriptorHandle, Viewport(..))
import Windows.Struct.Rect (Rect(..))
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
import Foreign.C.Types (CUInt, CULong)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Data.Bits ((.|.))
import Data.Maybe (maybe)
import Data.IORef (newIORef, modifyIORef, readIORef)
import Numeric (showHex)

data PlatformException = RegisterClassExFailed | WindowCreationFailed | ComError HRESULT | EventCreationFailed
instance Show PlatformException where
  show RegisterClassExFailed = "RegisterClassExFailed"
  show WindowCreationFailed = "WindowCreationFailed"
  show (ComError e) = "ComError " ++ showHex (fromIntegral e :: CULong) ""
  show EventCreationFailed = "EventCreationFailed"
instance Exception PlatformException

ensureSuccessComIO :: Either HRESULT a -> IO a
ensureSuccessComIO = either (throwIO . ComError) pure

-- | using interface while continuation chain. auto released after chain was finished
useInterface :: ComInterface a => Ptr a -> ContT r IO (Ptr a)
useInterface = ContT . withInterface

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
    dxgifactory <- useInterface =<< lift ((IDXGIFactory2.createDxgiFactory2 d True >>= ensureSuccessComIO) :: IO (Ptr IDXGIFactory2.IDXGIFactory2))
    adapter <- useInterface =<< (lift $ IDXGIFactory2.enumAdapters dxgifactory 0 >>= ensureSuccessComIO)
    d12 <- ContT $ withLibrary "d3d12"
    lift (ID3D12Debug.withDebugInterface d12 ID3D12Debug.enableDebugLayer >>= ensureSuccessComIO)
    device12 <- useInterface =<< (lift $ ID3D12Device.createDevice d12 adapter ID3D12Device._D3D_FEATURE_LEVEL_12_0 >>= ensureSuccessComIO)
    let cqDesc = ID3D12CommandQueue.defaultCommandQueueDesc ID3D12CommandQueue.commandListTypeDirect
    q <- useInterface =<< (lift $ ID3D12Device.createCommandQueue device12 cqDesc >>= ensureSuccessComIO)
    let scDesc = DxgiStruct.defaultSwapChainDesc1 (640, 480) DxgiConst.formatR8G8B8A8Unorm DxgiConst.usageRenderTargetOutput 2 DxgiConst.swapEffectFlipDiscard
    swapchain <- useInterface =<< (lift $ IDXGIFactory2.createSwapChainForComposition dxgifactory q scDesc >>= ensureSuccessComIO)
    let rtvHeapDesc = ID3D12DescriptorHeap.defaultDescriptorHeapDesc ID3D12DescriptorHeap.descriptorHeapTypeRTV 2
    rtvHeaps <- useInterface =<< (lift $ ID3D12Device.createDescriptorHeap device12 rtvHeapDesc >>= ensureSuccessComIO)
    rtvDescSize <- lift $ ID3D12Device.getDescriptorHandleIncrementSize device12 ID3D12DescriptorHeap.descriptorHeapTypeRTV
    rtvDescBase <- lift $ ID3D12DescriptorHeap.getCPUDescriptorHandleForHeapStart rtvHeaps
    forM_ [0..1] $ \bbx -> do
      bb <- useInterface =<< (lift $ IDXGISwapChain3.getBuffer swapchain bbx >>= ensureSuccessComIO)
      let destHandle = plusCPUDescriptorHandle rtvDescBase $ bbx * fromIntegral rtvDescSize
      lift $ ID3D12Device.createRenderTargetView device12 bb destHandle
    cmdwait <- useInterface =<< (lift $ ID3D12Device.createFence device12 0 0 >>= ensureSuccessComIO)
    cmdwaitEvent <- (ContT . withEvent) =<< (lift $ createEvent Nothing False True "CommandWait" >>= maybe (throwIO EventCreationFailed) pure)
    cmdalloc <- useInterface =<< (lift $ ID3D12Device.createCommandAllocator device12 ID3D12CommandQueue.commandListTypeDirect >>= ensureSuccessComIO)
    viewports <- lift $ newListArray (0, 0) [Viewport 0.0 0.0 640.0 480.0 0.0 1.0]
    scissors <- lift $ newListArray (0, 0) [Rect 0 0 640 480]
    renderCommands <- forM [0..1] $ \bbx -> do
      bb <- useInterface =<< (lift $ IDXGISwapChain3.getBuffer swapchain bbx >>= ensureSuccessComIO)
      cb <- lift (ID3D12Device.createCommandList device12 0 ID3D12CommandQueue.commandListTypeDirect cmdalloc Nothing >>= ensureSuccessComIO)
      let rtvMain = plusCPUDescriptorHandle rtvDescBase $ bbx * fromIntegral rtvDescSize
      inBarriers <- lift $ newListArray (0, 0) [ResourceTransitionBarrier 0 bb 0 resourceStatePresent resourceStateRenderTarget]
      outBarriers <- lift $ newListArray (0, 0) [ResourceTransitionBarrier 0 bb 0 resourceStateRenderTarget resourceStatePresent]
      renderTargets <- lift $ newListArray (0, 0) [rtvMain]
      lift $ sequence_
        [ ID3D12GraphicsCommandList.resourceBarrier cb inBarriers
        , ID3D12GraphicsCommandList.setRenderTargets cb renderTargets Nothing
        , ID3D12GraphicsCommandList.setViewports cb viewports
        , ID3D12GraphicsCommandList.setScissorRects cb scissors
        , ID3D12GraphicsCommandList.clearEntireRenderTargetView cb rtvMain (0.0, 0.0, 0.0, 1.0)
        , ID3D12GraphicsCommandList.resourceBarrier cb outBarriers
        ]
      lift $ ID3D12GraphicsCommandList.close cb >>= ensureSuccessComIO
      pure cb
    
    dclib <- ContT $ withLibrary "dcomp"
    dcDevice <- useInterface =<< lift (IDCompositionDevice3.createDevice dclib Nothing >>= ensureSuccessComIO)
    dcTarget <- useInterface =<< lift (IDCompositionDesktopDevice.createTargetForHwnd dcDevice window False >>= ensureSuccessComIO)
    dcVisual <- useInterface =<< lift (IDCompositionDesktopDevice.createVisual dcDevice >>= ensureSuccessComIO)
    lift $ IDCompositionVisual2.setContent dcVisual swapchain >>= ensureSuccessComIO
    lift $ IDCompositionTarget.setRoot dcTarget dcVisual >>= ensureSuccessComIO
    lift $ IDCompositionDesktopDevice.commit dcDevice >>= ensureSuccessComIO
    
    frameCounter <- lift $ newIORef 0

    lift $ showWindow window _SW_SHOWNORMAL
    lift $ alloca $ \x -> loop x cmdwaitEvent $ do
      IDXGISwapChain3.present swapchain 0 0
      frameCount <- readIORef frameCounter
      current <- IDXGISwapChain3.getCurrentBackBufferIndex swapchain
      lists <- newListArray (0, 0) [renderCommands !! current]
      ID3D12CommandQueue.executeCommandLists q lists
      ID3D12CommandQueue.signal q cmdwait frameCount
      ID3D12Fence.setEventOnCompletion cmdwait frameCount $ eventAsHandle cmdwaitEvent
      modifyIORef frameCounter (+ 1)
    pure ()

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
