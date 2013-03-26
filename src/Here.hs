{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Here (here) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

here :: QuasiQuoter
here = QuasiQuoter {
	quoteExp = litE . stringL . tail,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined
 }
