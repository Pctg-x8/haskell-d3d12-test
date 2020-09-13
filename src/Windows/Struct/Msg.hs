module Windows.Struct.Msg (MSG(..), peekMessage) where

import Windows.Types
import Windows.Struct.Point (POINT(..))
import Foreign.C.Types (CUInt(..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, plusPtr)

data MSG = MSG
  { hwnd :: HWND
  , message :: CUInt
  , wParam :: WPARAM
  , lParam :: LPARAM
  , time :: DWORD
  , pt :: POINT
  , lPrivate :: DWORD
  }
_MSG_HWND_OFFSET = 0
_MSG_MESSAGE_OFFSET = _MSG_HWND_OFFSET+8
-- with alignment (extra 4 bytes: align to 8 bytes)
_MSG_WPARAM_OFFSET = _MSG_MESSAGE_OFFSET+4+4
_MSG_LPARAM_OFFSET = _MSG_WPARAM_OFFSET+8
_MSG_TIME_OFFSET = _MSG_LPARAM_OFFSET+8
_MSG_PT_OFFSET = _MSG_TIME_OFFSET+4
_MSG_LPRIVATE_OFFSET = _MSG_PT_OFFSET+sizeOf (undefined :: POINT)
instance Storable MSG where
  sizeOf _ = _MSG_LPRIVATE_OFFSET+4
  alignment _ = 8
  peek p = MSG <$>
    peek (castPtr p) <*>
    peek (plusPtr p _MSG_MESSAGE_OFFSET) <*>
    peek (plusPtr p _MSG_WPARAM_OFFSET) <*>
    peek (plusPtr p _MSG_LPARAM_OFFSET) <*>
    peek (plusPtr p _MSG_TIME_OFFSET) <*>
    peek (plusPtr p _MSG_PT_OFFSET) <*>
    peek (plusPtr p _MSG_LPRIVATE_OFFSET)
  poke p (MSG hwnd message wParam lParam time pt lPrivate) = do
    poke (castPtr p) hwnd
    poke (plusPtr p _MSG_MESSAGE_OFFSET) message
    poke (plusPtr p _MSG_WPARAM_OFFSET) wParam
    poke (plusPtr p _MSG_LPARAM_OFFSET) lParam
    poke (plusPtr p _MSG_TIME_OFFSET) time
    poke (plusPtr p _MSG_PT_OFFSET) pt
    poke (plusPtr p _MSG_LPRIVATE_OFFSET) lPrivate

-- Direct Access to Message enum
peekMessage :: Ptr MSG -> IO CUInt
peekMessage = peek . flip plusPtr _MSG_MESSAGE_OFFSET
