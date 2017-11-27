-- Main.hs redirects Gloss package to use CoordinationGame.hs as Main
module Main where

	import CoordinationGame (defaultMain)

	main :: IO()
	main = defaultMain