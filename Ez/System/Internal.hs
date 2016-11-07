-- |
-- Module      :  Ez.System.Internal
-- Copyright   :  (c) Eike Ziller
-- License     :  MIT
--
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal tool functions

{-# LANGUAGE FlexibleContexts #-}

module Ez.System.Internal where

  import Text.Parsec

  pNumber :: (Num a, Read a, Stream s m Char) => ParsecT s u m a
  pNumber = fmap read (many1 digit)

  skipRestOfLine :: (Stream s m Char) => ParsecT s u m ()
  skipRestOfLine = do
    manyTill anyChar $ try (lookAhead endOfLine)
    endOfLine
    return ()

  word :: (Stream s m Char) => ParsecT s u m String
  word = manyTill anyChar $ try (lookAhead space)

  eitherToMaybe :: Either a b -> Maybe b
  eitherToMaybe = either (const Nothing) Just
