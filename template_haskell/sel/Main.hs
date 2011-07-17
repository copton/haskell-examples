#!/usr/bin/runhaskell

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Sel

foo = $(sel 2 3) ('a','b','c')

main = putStrLn $ show foo
