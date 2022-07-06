module Commands.Compile where

import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Data.Text.IO qualified as TIO
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative
import System.Environment
import System.Process qualified as P

minijuvixBuildDir :: FilePath
minijuvixBuildDir = ".minijuvix-build"

data CompileTarget = TargetC | TargetWasm
  deriving stock (Show)

data CompileRuntime = RuntimeStandalone | RuntimeLibC
  deriving stock (Show)

data CompileOptions = CompileOptions
  { _compileTarget :: CompileTarget,
    _compileRuntime :: CompileRuntime,
    _compileOutputFile :: Maybe FilePath
  }

makeLenses ''CompileOptions

parseCompile :: Parser CompileOptions
parseCompile = do
  _compileTarget <-
    option
      (eitherReader parseTarget)
      ( long "target"
          <> short 't'
          <> metavar "TARGET"
          <> value TargetWasm
          <> showDefaultWith targetShow
          <> help "select a target: wasm, c"
      )

  _compileRuntime <-
    option
      (eitherReader parseRuntime)
      ( long "runtime"
          <> short 'r'
          <> metavar "RUNTIME"
          <> value RuntimeStandalone
          <> showDefaultWith runtimeShow
          <> help "select a runtime: standalone, libc"
      )

  _compileOutputFile <-
    optional $
      option
        str
        ( long "output"
            <> short 'o'
            <> metavar "OUTPUT_FILE"
            <> help "Path to output file"
            <> action "file"
        )
  pure CompileOptions {..}
  where
    parseTarget :: String -> Either String CompileTarget
    parseTarget = \case
      "wasm" -> Right TargetWasm
      "c" -> Right TargetC
      s -> Left $ "unrecognised target: " <> s

    targetShow :: CompileTarget -> String
    targetShow = \case
      TargetC -> "c"
      TargetWasm -> "wasm"

    parseRuntime :: String -> Either String CompileRuntime
    parseRuntime = \case
      "standalone" -> Right RuntimeStandalone
      "libc" -> Right RuntimeLibC
      s -> Left $ "unrecognised runtime: " <> s

    runtimeShow :: CompileRuntime -> String
    runtimeShow = \case
      RuntimeStandalone -> "standalone"
      RuntimeLibC -> "libc"

inputCFile :: FilePath -> FilePath -> FilePath
inputCFile projRoot compileInputFile =
  projRoot </> minijuvixBuildDir </> outputMiniCFile
  where
    outputMiniCFile :: FilePath
    outputMiniCFile = takeBaseName compileInputFile <> ".c"

runCompile :: FilePath -> FilePath -> CompileOptions -> Text -> IO (Either Text ())
runCompile projRoot compileInputFile o minic = do
  createDirectoryIfMissing True (projRoot </> minijuvixBuildDir)
  TIO.writeFile (inputCFile projRoot compileInputFile) minic
  prepareRuntime projRoot o
  case o ^. compileTarget of
    TargetWasm -> clangCompile projRoot compileInputFile o
    TargetC -> return (Right ())

prepareRuntime :: FilePath -> CompileOptions -> IO ()
prepareRuntime projRoot o = do
  mapM_ writeRuntime runtimeProjectDir
  where
    standaloneRuntimeDir :: [(FilePath, BS.ByteString)]
    standaloneRuntimeDir = $(FE.makeRelativeToProject "minic-runtime/standalone" >>= FE.embedDir)

    libCRuntimeDir :: [(FilePath, BS.ByteString)]
    libCRuntimeDir = $(FE.makeRelativeToProject "minic-runtime/libc" >>= FE.embedDir)

    builtinCRuntimeDir :: [(FilePath, BS.ByteString)]
    builtinCRuntimeDir = $(FE.makeRelativeToProject "minic-runtime/builtins" >>= FE.embedDir)

    runtimeProjectDir :: [(FilePath, BS.ByteString)]
    runtimeProjectDir = case o ^. compileRuntime of
      RuntimeStandalone -> standaloneRuntimeDir <> builtinCRuntimeDir
      RuntimeLibC -> libCRuntimeDir <> builtinCRuntimeDir

    writeRuntime :: (FilePath, BS.ByteString) -> IO ()
    writeRuntime (filePath, contents) =
      BS.writeFile (projRoot </> minijuvixBuildDir </> takeFileName filePath) contents

clangCompile :: FilePath -> FilePath -> CompileOptions -> IO (Either Text ())
clangCompile projRoot compileInputFile o = do
  v <- sysrootEnvVar
  case v of
    Left s -> return (Left s)
    Right sysrootPath -> withSysrootPath sysrootPath
  where
    sysrootEnvVar :: IO (Either Text String)
    sysrootEnvVar =
      maybeToEither "Missing environment variable WASI_SYSROOT_PATH"
        <$> lookupEnv "WASI_SYSROOT_PATH"

    withSysrootPath :: String -> IO (Either Text ())
    withSysrootPath sysrootPath = runClang clangArgs
      where
        clangArgs :: [String]
        clangArgs = case o ^. compileRuntime of
          RuntimeStandalone -> standaloneArgs projRoot sysrootPath outputFile inputFile
          RuntimeLibC -> libcArgs sysrootPath outputFile inputFile

        outputFile :: FilePath
        outputFile = fromMaybe (takeBaseName compileInputFile <> ".wasm") (o ^. compileOutputFile)

        inputFile :: FilePath
        inputFile = inputCFile projRoot compileInputFile

standaloneArgs :: FilePath -> FilePath -> FilePath -> FilePath -> [String]
standaloneArgs projRoot sysrootPath wasmOutputFile inputFile =
  commonArgs sysrootPath wasmOutputFile
    <> [projRoot </> minijuvixBuildDir </> "walloc.c", inputFile]

libcArgs :: FilePath -> FilePath -> FilePath -> [String]
libcArgs sysrootPath wasmOutputFile inputFile =
  commonArgs sysrootPath wasmOutputFile
    <> ["-lc", inputFile]

commonArgs :: FilePath -> FilePath -> [String]
commonArgs sysrootPath wasmOutputFile =
  [ "-nodefaultlibs",
    "-std=c99",
    "-Oz",
    "-I",
    minijuvixBuildDir,
    "--target=wasm32-wasi",
    "--sysroot",
    sysrootPath,
    "-o",
    wasmOutputFile
  ]

runClang ::
  [String] ->
  IO (Either Text ())
runClang args = do
  (exitCode, _, err) <- P.readProcessWithExitCode "clang" args ""
  case exitCode of
    ExitSuccess -> return (Right ())
    _ -> return (Left (pack err))
