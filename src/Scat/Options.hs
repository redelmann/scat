
-- | Parses command-line arguments.
module Scat.Options
    (
    -- * Type
      Options

    -- * Accessors
    , password
    , service
    , useCode
    , code
    , schema
    , size
    , verbose
    , confirm
    , ansi

    -- * Execution
    , getOptions
    ) where

import Data.Monoid
import Options.Applicative

-- | All program options.
data Options = Options
    { password :: Maybe String
    -- ^ Password, optionally provided.
    , service  :: Maybe String
    -- ^ Service for which to generate the password.
    , useCode  :: Bool
    -- ^ Indicates if extra code should be used.
    , code     :: Maybe String
    -- ^ Extra code.
    , schema   :: String
    -- ^ Name of the schema to use.
    , size     :: Maybe Int
    -- ^ Size parameter of the schema.
    , verbose  :: Bool
    -- ^ Verbosity. If false, do not print anything but the generated password.
    , confirm  :: Bool
    -- ^ Indicates if the password must be confirmed.
    , ansi     :: Bool
    -- ^ Indicates if ANSI escape sequences can be used.
    }

-- | Parses the arguments from the command line.
getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (helper <*> options)
        (fullDesc
        <> progDesc (unwords
            [ "Safely generate passwords derived "
            , "from a unique password and code." ])
        <> header "scat - a password scatterer")

-- | Option parser.
options :: Parser Options
options = Options
    <$> optional
          (strOption (short 'p'
        <> long "password"
        <> help "The password"
        <> metavar "PASSWORD"))
    <*> optional (strOption
          (short 'S'
        <> long "service"
        <> help "Service associated (website, email address, ...)"
        <> metavar "SERVICE"))
    <*> flag True False
          (long "nocode"
        <> help "Indicates that extra code should be not be used")
    <*> optional
          (strOption (short 'x'
        <> long "code"
        <> help "The extra code to use"
        <> metavar "CODE"))
    <*> strOption
          (short 's'
        <> long "schema"
        <> help "Schema for the generated password"
        <> metavar "SCHEMA"
        <> value "safe"
        <> showDefault)
    <*> optional (option
          (short 'n'
        <> long "size"
        <> help "Size parameter"
        <> metavar "SIZE"))
    <*> flag True False
          (long "silent"
        <> help "Do not print anything but the generated password")
    <*> switch
          (short 'c'
        <> long "confirmation"
        <> help "Asks for password confirmation")
    <*> flag True False
          (long "noansi"
        <> help "Do not use ANSI escape sequences to format output")
