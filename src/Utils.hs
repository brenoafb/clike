module Utils where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

bs2str :: B.ByteString -> String
bs2str = T.unpack . TE.decodeUtf8
