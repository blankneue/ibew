\begin{code}
module Hashing (argonHash) where

import Crypto.Error (CryptoFailable)
import qualified Crypto.KDF.Argon2 as A2
import Data.ByteString

argonHash :: ByteString -> ByteString -> CryptoFailable ByteString
argonHash a b = A2.hash (A2.Options 8 131072 4
                                    A2.Argon2id A2.Version13)
                        a b 512
\end{code}
