{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.Quoter.HSE
import qualified Language.Haskell.Exts as Exts

main = print $ Exts.prettyPrint $ toIdiom [qe| (+) (Just 1) (Just 2) |]

toIdiom :: Exts.Exp -> Exts.Exp
toIdiom = go []
  where
    go xs (Exts.App f x) = go (x:xs) f
    go xs f = expIdiom f $ reverse xs

expIdiom :: Exts.Exp -> [Exts.Exp] -> Exts.Exp
expIdiom h []     = [qe| pure {{h}} |]
expIdiom h (e:es) = foldr (flip [qe| {{}} <*> {{}} |]) [qe| {{h}} <$> {{e}} |] es
