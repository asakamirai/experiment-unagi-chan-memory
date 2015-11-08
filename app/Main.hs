{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent            as CC
import qualified Control.Concurrent.Chan.Unagi as UNG
import qualified Control.Exception             as E
import qualified Control.Monad                 as M

import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L

main :: IO ()
main = do
    (inchan, outchan) <- UNG.newChan
    let buf = BS.replicate (500 * 1024 * 1024) 'X'
    buf `seq` UNG.writeChan inchan buf
    buf `seq` putStrLn $ "write---------------0"
    M.replicateM_ 4 $ M.replicateM_ 1024 $ UNG.writeChan inchan "-"
    putStrLn $ "read---------------1"
    M.replicateM_ 1024 $ UNG.readChan outchan
    putStrLn $ "read---------------1"
    CC.threadDelay $ 5 * 1000000
    putStrLn $ "read---------------2"
    M.replicateM_ 1 $ UNG.readChan outchan
    putStrLn $ "read---------------2"
    CC.threadDelay $ 5 * 1000000

