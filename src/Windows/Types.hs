module Windows.Types
  ( HWND, HINSTANCE, HMODULE, HICON, HCURSOR, HBRUSH, HMENU
  , LPCSTR, WPARAM, LPARAM, LRESULT, HRESULT, WNDPROC, ATOM, DWORD, LONG, LPVOID, BOOL
  , HANDLE
  ) where

import Data.Word (Word16)
import Foreign.C.Types (CChar, CInt, CUInt, CULong, CLong)
import Foreign.Ptr (Ptr, FunPtr, IntPtr, WordPtr)

data HWND__
type HWND = Ptr HWND__
data HINSTANCE__
type HINSTANCE = Ptr HINSTANCE__
type HMODULE = HINSTANCE
data HICON__
type HICON = Ptr HICON__
type HCURSOR = HICON
data HBRUSH__
type HBRUSH = Ptr HBRUSH__
data HMENU__
type HMENU = Ptr HMENU__
type LPCSTR = Ptr CChar
type WPARAM = WordPtr
type LPARAM = IntPtr
type LRESULT = IntPtr
type HRESULT = CLong
type WNDPROC = FunPtr (HWND -> CUInt -> WPARAM -> LPARAM -> IO LRESULT)
type ATOM = Word16
type DWORD = CULong
type LONG = CLong
type LPVOID = Ptr ()
type BOOL = CInt
type HANDLE = Ptr ()
