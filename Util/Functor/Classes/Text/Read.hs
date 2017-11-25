module Util.Functor.Classes.Text.Read (ReadMethods (..), lift) where

import Data.Functor.Compose
import Data.Functor.Classes
import Data.Functor.Product
import Text.ParserCombinators.ReadPrec hiding (lift)

data ReadMethods a = ReadMethods { readPrec :: ReadPrec a, readListPrec :: ReadPrec [a] }
  deriving (Functor)

type ReadMethods' = Product ReadPrec (Compose ReadPrec [])

toRep :: ReadMethods a -> ReadMethods' a
toRep (ReadMethods {..}) = Pair readPrec (Compose readListPrec)

fromRep :: ReadMethods' a -> ReadMethods a
fromRep (Pair readPrec (Compose readListPrec)) = ReadMethods
    { readPrec = readPrec, readListPrec = readListPrec }

instance Applicative ReadMethods where
    pure = fromRep . pure
    a <*> b = fromRep (toRep a <*> toRep b)

lift :: Read1 f => ReadMethods a -> ReadMethods (f a)
lift (ReadMethods {..}) = ReadMethods { readPrec = liftReadPrec readPrec readListPrec,
                                        readListPrec = liftReadListPrec readPrec readListPrec }
