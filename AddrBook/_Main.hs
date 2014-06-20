module Main (main) where

import System.Console.GetOpt
import System.Environment

data Flag = Flag
    { path   :: String
    , create :: Bool }

defaultFlags :: Flag
defaultFlags = Flag
    { path   = []
    , create = False }


flags :: [OptDescr (Flag -> Flag)]
flags =
    [ Option ['p'] [] (ReqArg (\p flg -> flg { path = p }) "PATH") []
    , Option ['c'] [] (NoArg (\flg -> flg { create = True })) [] ]

main :: IO ()
main = do
    argv <- getArgs
    let opts = getOpt RequireOrder flags argv
    case opts of
        (o, _, [])  -> do
            let flgs   = foldl (flip id) defaultFlags o
                source = case path flgs of
                    [] -> Nothing
                    p  -> Just p
            case create flgs of
                True  -> connectDB source >>= createDB >>= mainLoop
                False -> connectDB source >>= mainLoop
        ([], _, []) -> connectDB Nothing >>= mainLoop
        (_, _, _)   -> err 1 usage

connectDB = undefined
createDB = undefined
mainLoop = undefined

usage :: String
usage = "usage: addrbook [-cd filepath]"

err = undefined
