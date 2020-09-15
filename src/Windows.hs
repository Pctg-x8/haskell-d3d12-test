{-# LANGUAGE ForeignFunctionInterface #-}

module Windows
  ( registerClassEx, createWindowEx, getMessage, GetMessageResult(..), translateMessage, dispatchMessage
  , msgWaitForMultipleObjects, WaitResult(..), peekMessage, RemoveType(..), wakeMaskAllEvents, waitFlagInputAvailable
  , showWindow, _SW_SHOWNORMAL, postQuitMessage, defWindowProc
  , Event, createEvent, destroyEvent, withEvent, eventAsHandle, waitForSingleObject
  ) where

import Data.Maybe (maybe)
import Foreign.Ptr (Ptr, nullPtr, IntPtr(..), WordPtr(..))
import Foreign.C.Types (CInt(..), CUInt(..), CULong(..))
import Foreign.C.String (withCString)
import Foreign.Storable (poke)
import Foreign.Marshal.Alloc (alloca)
import Data.Ix (Ix)
import Data.Array.Base (getNumElements)
import Data.Array.Storable (StorableArray, withStorableArray)
import Windows.Types
import Windows.Struct.WndClassEx (WNDCLASSEXA)
import Windows.Struct.Msg (MSG)
import Control.Monad (void)
import Control.Exception (bracket)

foreign import ccall "RegisterClassExA" c_RegisterClassExA :: Ptr WNDCLASSEXA -> IO ATOM
registerClassEx :: WNDCLASSEXA -> IO (Maybe ATOM)
registerClassEx def = fmap discarder $ alloca $ \p -> poke p def >> c_RegisterClassExA p where
  discarder 0 = Nothing
  discarder a = Just a

_CW_USEDEFAULT = fromIntegral 0x80000000
foreign import ccall "CreateWindowExA" c_CreateWindowExA :: DWORD -> LPCSTR -> LPCSTR -> DWORD -> CInt -> CInt -> CInt -> CInt -> HWND -> HMENU -> HINSTANCE -> LPVOID -> IO HWND
createWindowEx :: DWORD -> LPCSTR -> LPCSTR -> DWORD -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe HWND -> Maybe HMENU -> HINSTANCE -> IO (Maybe HWND)
createWindowEx dwExStyle lpClassName lpWindowName dwStyle x y nWidth nHeight hWndParent hMenu hInstance =
  let
    discarder p = if p == nullPtr then Nothing else Just p
  in fmap discarder $ c_CreateWindowExA
    dwExStyle lpClassName lpWindowName dwStyle
    (maybe _CW_USEDEFAULT fromIntegral x)
    (maybe _CW_USEDEFAULT fromIntegral y)
    (maybe _CW_USEDEFAULT fromIntegral nWidth)
    (maybe _CW_USEDEFAULT fromIntegral nHeight)
    (maybe nullPtr id hWndParent)
    (maybe nullPtr id hMenu)
    hInstance
    nullPtr

_SW_SHOWNORMAL :: Int
_SW_SHOWNORMAL = 1
foreign import ccall "ShowWindow" c_ShowWindow :: HWND -> CInt -> IO BOOL
showWindow :: HWND -> Int -> IO BOOL
showWindow hWnd nCmdShow = c_ShowWindow hWnd (fromIntegral nCmdShow)

data GetMessageResult = Continue | HasReceivedQuit | Errored deriving (Show, Eq)

foreign import ccall "GetMessageA" c_GetMessageA :: Ptr MSG -> HWND -> CUInt -> CUInt -> IO BOOL
getMessage :: Ptr MSG -> HWND -> Maybe Int -> Maybe Int -> IO GetMessageResult
getMessage lpMsg hWnd wMsgFilterMin wMsgFilterMax =
  let
    trans x
      | x == 0 = HasReceivedQuit
      | x > 0 = Continue
      | otherwise = Errored
  in fmap trans $ c_GetMessageA lpMsg hWnd (fromIntegral $ maybe 0 id wMsgFilterMin) (fromIntegral $ maybe 0 id wMsgFilterMax)

data WaitResult = Signal Int | NewInputAvailable | Abandoned Int | IOCompletion | Timeout | Failed

_WAIT_FAILED = 0xffffffff :: DWORD
_WAIT_OBJECT_0 = 0 :: DWORD
_WAIT_ABANDONED_0 = 0x80 :: DWORD
_WAIT_IO_COMPLETION = 0xc0 :: DWORD
_WAIT_TIMEOUT = 258 :: DWORD
type WakeMask = DWORD
wakeMaskAllEvents :: WakeMask
wakeMaskAllEvents = 0x04bf
type WaitFlags = DWORD
waitFlagInputAvailable :: WaitFlags
waitFlagInputAvailable = 0x04
foreign import ccall "MsgWaitForMultipleObjectsEx" c_MsgWaitForMultipleObjectsEx :: DWORD -> Ptr HANDLE -> DWORD -> DWORD -> DWORD -> IO DWORD
msgWaitForMultipleObjects :: Ix i => StorableArray i HANDLE -> Maybe Int -> WakeMask -> WaitFlags -> IO WaitResult
msgWaitForMultipleObjects objects timeout wakeMask flags = withStorableArray objects $ \oref -> do
  objectCount <- fromIntegral <$> getNumElements objects
  r <- c_MsgWaitForMultipleObjectsEx objectCount oref (maybe 0xffffffff fromIntegral timeout) wakeMask flags
  if _WAIT_OBJECT_0 <= r && r < _WAIT_OBJECT_0 + objectCount then
    pure $ Signal $ fromIntegral $ r - _WAIT_OBJECT_0
  else if r == _WAIT_OBJECT_0 + objectCount then
    pure NewInputAvailable
  else if _WAIT_ABANDONED_0 <= r && r < _WAIT_ABANDONED_0 + objectCount then
    pure $ Abandoned $ fromIntegral $ r - _WAIT_ABANDONED_0
  else if r == _WAIT_IO_COMPLETION then
    pure IOCompletion
  else if r == _WAIT_TIMEOUT then
    pure Timeout
  else
    pure Failed

data RemoveType = NoRemove | Remove | NoYield
foreign import ccall "PeekMessageA" c_PeekMessageA :: Ptr MSG -> HWND -> CUInt -> CUInt -> CUInt -> IO BOOL
-- | Returns True if message is available
peekMessage :: Ptr MSG -> HWND -> Maybe (Int, Int) -> RemoveType -> IO Bool
peekMessage msgref hwnd range removeType =
  let
    removeTypeNum = case removeType of
      NoRemove -> 0
      Remove -> 1
      NoYield -> 2
    (minMessage, maxMessage) = maybe (0, 0) id range
  in (/= 0) <$> c_PeekMessageA msgref hwnd (fromIntegral minMessage) (fromIntegral maxMessage) removeTypeNum

foreign import ccall "TranslateMessage" c_TranslateMessage :: Ptr MSG -> IO BOOL
translateMessage :: Ptr MSG -> IO BOOL
translateMessage = c_TranslateMessage

foreign import ccall "DispatchMessageA" c_DispatchMessageA :: Ptr MSG -> IO LRESULT
dispatchMessage :: Ptr MSG -> IO LRESULT
dispatchMessage = c_DispatchMessageA

foreign import ccall "PostQuitMessage" c_PostQuitMessage :: CInt -> IO ()
postQuitMessage :: Int -> IO ()
postQuitMessage = c_PostQuitMessage . fromIntegral

foreign import ccall "DefWindowProcA" defWindowProc :: HWND -> CUInt -> WPARAM -> LPARAM -> IO LRESULT

foreign import ccall "CloseHandle" closeHandle :: HANDLE -> IO BOOL
foreign import ccall "CreateEventA" c_CreateEvent :: Ptr () -> BOOL -> BOOL -> LPCSTR -> IO HANDLE

newtype Event = Event HANDLE
eventAsHandle :: Event -> HANDLE
eventAsHandle (Event h) = h
createEvent :: Maybe () -> Bool -> Bool -> String -> IO (Maybe Event)
createEvent Nothing manualReset initialSignaled name =
  let
    trans x
      | x == nullPtr = Nothing
      | otherwise = Just $ Event x
  in withCString name $ \nameref -> trans <$> c_CreateEvent nullPtr (if manualReset then 1 else 0) (if initialSignaled then 1 else 0) nameref
createEvent (Just securityAttributes) manualReset initialSignaled name =
  let
    trans x
      | x == nullPtr = Nothing
      | otherwise = Just $ Event x
    manualResetFlag = if manualReset then 1 else 0
    initialSignaledFlag = if initialSignaled then 1 else 0
  in withCString name $ \nameref -> alloca $ \saref -> do
    poke saref securityAttributes
    trans <$> c_CreateEvent saref manualResetFlag initialSignaledFlag nameref
destroyEvent :: Event -> IO BOOL
destroyEvent = closeHandle . eventAsHandle
withEvent :: Event -> (Event -> IO a) -> IO a
withEvent e = bracket (pure e) destroyEvent

foreign import ccall "WaitForSingleObject" c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD
waitForSingleObject :: HANDLE -> Maybe Int -> IO DWORD
waitForSingleObject handle timeout = c_WaitForSingleObject handle $ maybe 0xffffffff fromIntegral timeout
