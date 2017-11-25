module Util.Functor.Classes.Text.Show (ShowMethods (..), lift) where

import Data.Functor.Classes

data ShowMethods a = ShowMethods { showsPrec :: Int -> a -> ShowS, showList :: [a] -> ShowS }

lift :: Show1 f => ShowMethods a -> ShowMethods (f a)
lift (ShowMethods {..}) = ShowMethods { showsPrec = liftShowsPrec showsPrec showList,
                                        showList = liftShowList showsPrec showList }
