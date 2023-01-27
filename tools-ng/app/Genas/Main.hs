module Main where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Text.IO qualified as T
import HSBU.Genas.Arch.NBFV3 qualified as ArchNBFV3
import HSBU.Genas.Assembler
import HSBU.Genas.AssemblerCore
import HSBU.Genas.BinWriter
import HSBU.Genas.Dialect.MIPSLike qualified as DialectMIPS
import Options.Applicative
import Paths_HSBU (version)
import Text.Megaparsec
import Data.Maybe

data Options = Options
    { assembleMe :: FilePath
    , outputObject :: FilePath
    }

optionsParser :: Parser Options
optionsParser =
    Options
        <$> strArgument (metavar "source" <> help "Assemble this source file")
        <*> strOption (short 'o' <> long "output" <> metavar "outfile" <> help "Binary file to be created after assembly")

fullOptionsParser :: ParserInfo Options
fullOptionsParser = info (bonusOptions *> optionsParser) description
  where
    bonusOptions = versionO
    versionO = infoOption (show version) (short 'v' <> long "version" <> help "Display version and exit")
    description = fullDesc <> progDesc "Genas" <> header "Genas - the generic assembler of HSBU"

runProgram :: Options -> IO ()
runProgram opts = do
    content <- T.readFile (assembleMe opts)
    let result = parse DialectMIPS.sourceFile (assembleMe opts) content
    case result of
        Left errors -> putStrLn $ errorBundlePretty errors
        Right sAST -> do
            -- print sAST
            let (assembled, errors) = runAssembler $ assemble ArchNBFV3.assembler sAST
            mapM_ print errors
            forM_ assembled $ BS.writeFile (outputObject opts) . hexEncodeWordsBE
            let status
                    | isNothing assembled = "Assembly failed."
                    | not $ null errors = "Assembly succeeded with errors."
                    | otherwise = "Assembly failed"
            putStrLn status

main :: IO ()
main = do
    options <- execParser fullOptionsParser
    runProgram options