
-- | Parses command-line arguments.
module Scat.Options
    (
    -- * Type
      Options

    -- * Accessors
    , password
    , key
    , schema
    , verbose
    , confirm

    -- * Execution
    , getOptions
    ) where

import Data.Monoid
import Options.Applicative

-- | All program options.
data Options = Options
    { password :: Maybe String
    -- ^ Password, optionally provided.
    , key      :: String
    -- ^ Key or category for the password.
    , schema   :: String
    -- ^ Name of the schema to use.
    , verbose_ :: Bool
    -- ^ Verbosity. If false, do not print anything but the generated password.
    , confirm  :: Bool
    -- ^ Indicates if the password must be confirmed. Activates verbosity.
    }

{- | Verbosity. If false, do not print anything but the generated password.
     True when @--verbose@ or @--confirmation@ are specified. -}
verbose :: Options -> Bool
verbose opts = verbose_ opts || confirm opts

-- | Parses the arguments from the command line.
getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (helper <*> options)
        (fullDesc
        <> progDesc "Safely generate passwords derived from a unique password."
        <> header "scat - a password scatterer")

-- | Option parser.
options :: Parser Options
options = Options
    <$> optional
          (strOption (short 'p'
        <> long "password"
        <> help "The password"
        <> metavar "PASSWORD"))
    <*> strOption
          (short 'k'
        <> long "key"
        <> help "Key associated (website, email address, ...) (mandatory)"
        <> metavar "KEY")
    <*> strOption
          (short 's'
        <> long "schema"
        <> help "Schema for the generated password"
        <> metavar "SCHEMA"
        <> value "safe"
        <> showDefault)
    <*> switch
          (short 'v'
        <> long "verbose"
        <> help "Prints instructions and information")
    <*> switch
          (short 'c'
        <> long "confirmation"
        <> help "Asks for password confirmation")
