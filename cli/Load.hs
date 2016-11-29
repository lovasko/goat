module Load
( storyLoad
) where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import qualified Data.Text as T

import GoatSwim.TimeFrame
import GoatSwim.ValueFrame
import Story
import Variant
import Util


-- | Load a story from a file in the uncompressed format.
loadNormal :: B.ByteString        -- ^ file content
           -> Either T.Text Story -- ^ error | story
loadNormal bs = eitherFirst T.pack (S.runGet rule bs)
  where rule = many $ S.getTwoOf S.getWord32be S.getFloat32be

-- | Load a story from a file in the compressed format.
loadCompressed :: B.ByteString        -- ^ file content
               -> Either T.Text Story -- ^ error | story
loadCompressed bs = case S.runGet (S.getTwoOf S.get S.get) bs of
  Left  err      -> Left  $ T.pack err
  Right (ts, vs) -> Right $ zip (timeDecode ts) (valueDecode vs)

-- | Load any story file into memory.
storyLoad :: FilePath                 -- ^ file path
          -> IO (Either T.Text Story) -- ^ error | story
storyLoad path = do
  content <- B.readFile path
  return $ case varIdentify path of
    Left  err           -> Left err
    Right VarNormal     -> loadNormal content
    Right VarCompressed -> loadCompressed content
