
module Windows.Struct.WndClassEx (WNDCLASSEXA(..), defaultWndClassEx, setWndProc, makeWndProc) where

import Foreign.C.Types (CUInt(..), CInt(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr, nullPtr, IntPtr(..), WordPtr(..))
import Foreign.Storable (Storable(..))
import Windows.Types

foreign import ccall "&DefWindowProcA" cref_DefWindowProcA :: FunPtr (HWND -> CUInt -> WPARAM -> LPARAM -> IO LRESULT)
foreign import ccall "GetModuleHandleA" c_GetModuleHandleA :: LPCSTR -> IO HMODULE

data WNDCLASSEXA = WNDCLASSEXA
  {
    cbSize :: CUInt,
    style :: CUInt,
    lpfnWndProc :: WNDPROC,
    cbClsExtra :: CInt,
    cbWndExtra :: CInt,
    hInstance :: HINSTANCE,
    hIcon :: HICON,
    hCursor :: HCURSOR,
    hbrBackground :: HBRUSH,
    lpszMenuName :: LPCSTR,
    lpszClassName :: LPCSTR,
    hIconSm :: HICON
  }
_WNDCLASSEXA_CBSIZE_OFFSET = 0
_WNDCLASSEXA_STYLE_OFFSET = _WNDCLASSEXA_CBSIZE_OFFSET+4
_WNDCLASSEXA_LPFNWNDPROC_OFFSET = _WNDCLASSEXA_STYLE_OFFSET+4
_WNDCLASSEXA_CBCLSEXTRA_OFFSET = _WNDCLASSEXA_LPFNWNDPROC_OFFSET+8
_WNDCLASSEXA_CBWNDEXTRA_OFFSET = _WNDCLASSEXA_CBCLSEXTRA_OFFSET+4
_WNDCLASSEXA_HINSTANCE_OFFSET = _WNDCLASSEXA_CBWNDEXTRA_OFFSET+4
_WNDCLASSEXA_HICON_OFFSET = _WNDCLASSEXA_HINSTANCE_OFFSET+8
_WNDCLASSEXA_HCURSOR_OFFSET = _WNDCLASSEXA_HICON_OFFSET+8
_WNDCLASSEXA_HBRBACKGROUND_OFFSET = _WNDCLASSEXA_HCURSOR_OFFSET+8
_WNDCLASSEXA_LPSZMENUNAME_OFFSET = _WNDCLASSEXA_HBRBACKGROUND_OFFSET+8
_WNDCLASSEXA_LPSZCLASSNAME_OFFSET = _WNDCLASSEXA_LPSZMENUNAME_OFFSET+8
_WNDCLASSEXA_HICONSM_OFFSET = _WNDCLASSEXA_LPSZCLASSNAME_OFFSET+8
_WNDCLASSEXA_SIZE = _WNDCLASSEXA_HICONSM_OFFSET+8
instance Storable WNDCLASSEXA where
  sizeOf _ = _WNDCLASSEXA_SIZE
  -- align to ptr(8 bytes width for x86_64)
  alignment _ = 8
  peek p = WNDCLASSEXA <$>
    (peek $ castPtr p) <*>
    (peek $ plusPtr p _WNDCLASSEXA_STYLE_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_LPFNWNDPROC_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_CBCLSEXTRA_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_CBWNDEXTRA_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_HINSTANCE_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_HICON_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_HCURSOR_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_HBRBACKGROUND_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_LPSZMENUNAME_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_LPSZCLASSNAME_OFFSET) <*>
    (peek $ plusPtr p _WNDCLASSEXA_HICONSM_OFFSET)
  poke p (WNDCLASSEXA cbSize style lpfnWndProc cbClsExtra cbWndExtra hInstance hIcon hCursor hbrBackground lpszMenuName lpszClassName hIconSm) = do
    poke (castPtr p) cbSize
    poke (p `plusPtr` _WNDCLASSEXA_STYLE_OFFSET) style
    poke (p `plusPtr` _WNDCLASSEXA_LPFNWNDPROC_OFFSET) lpfnWndProc
    poke (p `plusPtr` _WNDCLASSEXA_CBCLSEXTRA_OFFSET) cbClsExtra
    poke (p `plusPtr` _WNDCLASSEXA_CBWNDEXTRA_OFFSET) cbWndExtra
    poke (p `plusPtr` _WNDCLASSEXA_HINSTANCE_OFFSET) hInstance
    poke (p `plusPtr` _WNDCLASSEXA_HICON_OFFSET) hIcon
    poke (p `plusPtr` _WNDCLASSEXA_HCURSOR_OFFSET) hCursor
    poke (p `plusPtr` _WNDCLASSEXA_HBRBACKGROUND_OFFSET) hbrBackground
    poke (p `plusPtr` _WNDCLASSEXA_LPSZMENUNAME_OFFSET) lpszMenuName
    poke (p `plusPtr` _WNDCLASSEXA_LPSZCLASSNAME_OFFSET) lpszClassName
    poke (p `plusPtr` _WNDCLASSEXA_HICONSM_OFFSET) hIconSm
defaultWndClassEx :: LPCSTR -> IO WNDCLASSEXA
defaultWndClassEx className = WNDCLASSEXA <$>
  (pure $ fromIntegral $ sizeOf (undefined :: WNDCLASSEXA)) <*>
  (pure $ fromIntegral 0) <*>
  (pure cref_DefWindowProcA) <*>
  (pure $ fromIntegral 0) <*>
  (pure $ fromIntegral 0) <*>
  (c_GetModuleHandleA nullPtr) <*>
  (pure nullPtr) <*>
  (pure nullPtr) <*>
  (pure nullPtr) <*>
  (pure nullPtr) <*>
  (pure className) <*>
  (pure nullPtr)
setWndProc :: WNDPROC -> WNDCLASSEXA -> WNDCLASSEXA
setWndProc f (WNDCLASSEXA cbSize style _ cbClsExtra cbWndExtra hInstance hIcon hCursor hbrBackground lpszMenuName lpszClassName hIconSm) =
  WNDCLASSEXA cbSize style f cbClsExtra cbWndExtra hInstance hIcon hCursor hbrBackground lpszMenuName lpszClassName hIconSm

foreign import ccall "wrapper" makeWndProc :: (HWND -> CUInt -> WPARAM -> LPARAM -> IO LRESULT) -> IO WNDPROC
