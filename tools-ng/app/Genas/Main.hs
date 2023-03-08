module Main where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Text.IO qualified as T
import HSBU.Genas.Arch.NBFV3 qualified as ArchNBFV3
import HSBU.Genas.Assembler
import HSBU.Genas.AssemblerCore
import HSBU.Genas.BinWriter
import HSBU.Genas.Dialect.MIPSLike qualified as DialectMIPS
import Options.Applicative
import Paths_HSBU (version)
import Text.Megaparsec
import qualified Data.ByteString as BL
import System.IO

data Options = Options
    { assembleMe :: FilePath
    , outputObject :: FilePath
    , debugDumpLL :: Bool
    }

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strArgument (metavar "source" <> help "Assemble this source file")
        <*> strOption (short 'o' <> long "output" <> metavar "outfile" <> help "Binary file to be created after assembly")
        <*> switch (long "dumpLL" <> help "Dump low-level AST")

fullOptionsParser :: ParserInfo Options
fullOptionsParser = info (bonusOptions *> optionsParser) description
  where
    bonusOptions = versionO
    versionO = infoOption (show version) (short 'v' <> long "version" <> help "Display version and exit")
    description = fullDesc <> progDesc "Genas" <> header "Genas - the generic assembler of HSBU"

runProgram :: Options -> IO ()
runProgram Options{assembleMe, outputObject, debugDumpLL} = do
    content <- T.readFile assembleMe
    let result = parse DialectMIPS.sourceFile assembleMe content
    case result of
        Left errors -> putStrLn $ errorBundlePretty errors
        Right sAST -> do
            -- mapM_ print $ uncurry (flip patchUnwrapArgs) $ unwrapWrapArgs sAST
            let (assembled, errors) = runAssembler $ assemble ArchNBFV3.assembler sAST
            mapM_ print errors
            let status
                    | isNothing assembled = "Assembly failed."
                    | not $ null errors = "Assembly succeeded with errors."
                    | otherwise = "Assembly succeeded."
            (if outputObject == "-" then hPutStrLn stderr else putStrLn) status
            when debugDumpLL $ mapM_ (mapM_ print) (snd <$> assembled)
            forM_ (fst <$> assembled) $ (if outputObject == "-" then BL.putStr else BS.writeFile outputObject) . hexEncodeWordsBE

main :: IO ()
main = do
    options <- execParser fullOptionsParser
    runProgram options