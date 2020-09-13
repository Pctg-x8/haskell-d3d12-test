module Windows.LibLoader (Lib, withLibrary, getProcAddress) where

import Windows.Types (HMODULE, LPCSTR, BOOL)
import Control.Exception (bracket)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (FunPtr)

data Lib = Lib HMODULE

foreign import ccall "LoadLibraryA" c_LoadLibrary :: LPCSTR -> IO HMODULE
foreign import ccall "GetProcAddress" c_GetProcAddress :: HMODULE -> LPCSTR -> IO (FunPtr a)
foreign import ccall "FreeLibrary" c_FreeLibrary :: HMODULE -> IO BOOL

withLibrary :: String -> (Lib -> IO a) -> IO a
withLibrary name = bracket (Lib <$> withCString name c_LoadLibrary) (\(Lib p) -> c_FreeLibrary p)

getProcAddress :: String -> Lib -> IO (FunPtr a)
getProcAddress fname (Lib p) = withCString fname $ c_GetProcAddress p
