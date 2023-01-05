{-# LANGUAGE OverloadedStrings #-}
module Main where

import ErrorCollectorM
import Genas
import AsParser
import Encode
import Data.Either
import Text.Megaparsec
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Builder as B
import Control.Monad.Except

import AsParser
import Genas
import qualified CustomIsaAs as CI

mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft mapper (Left lv) = Left $ mapper lv
mapLeft mapper (Right rv) = Right rv

-- runAssembler :: Assembler p -> FilePath -> FilePath -> IO ()
runAssembler asm inputF outputF = runExceptT $ do
    source <- liftIO $ TIO.readFile inputF
    parsed <- liftEither $ mapLeft show $ parse src inputF source
    liftIO $ putStr $ show parsed
    
    let (errors, assembled) = runErrorCollector $ assemble asm parsed
    liftIO $ case errors of
        [] -> putStrLn "Assembly successful"
        errs -> do
             putStrLn "The following errors were encountered (failing assembly):"
             putStr $ errors >>= ((++ "\n") . show)

    liftIO $ B.writeFile (outputF ++ ".bin") assembled
    liftIO $ B.writeFile (outputF ++ ".hex") $ hexEncodeG $ B.toLazyByteString assembled
    return ()

main :: IO ()
main = do
    r <- runAssembler CI.asm "input.S" "output"
    either putStr pure r
    return ()