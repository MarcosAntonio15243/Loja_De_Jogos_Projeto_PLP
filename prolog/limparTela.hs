{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Process (callCommand)
import System.Info (os)

limparTela::IO()
limparTela = case os of
    "linux" -> callCommand "clear"
    "darwin" -> callCommand "clear"
    "mingw32" -> callCommand "cls"
    _ -> callCommand "clear"

main::IO()
main = do
    limparTela
