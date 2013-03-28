import Text.RegexPR
import System.Environment
import Control.Applicative

main = do
	[pre, post, file] <- getArgs
	putStr =<< gsubRegexPR pre post <$> readFile file
