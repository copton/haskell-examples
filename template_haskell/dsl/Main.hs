#!/usr/bin/runhaskell

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Dsl (code)

main = putStrLn [$code|
			void main()
			{
				printf("hello world");
			}
|]
