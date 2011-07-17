#!/usr/bin/runhaskell

{-# LANGUAGE TemplateHaskell #-}

module Foo where

import Bar
import Language.Haskell.TH

main = putStrLn $ show $(version)
