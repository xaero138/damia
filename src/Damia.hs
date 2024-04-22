{-# LANGUAGE CPP #-}

#define VERSION "0.2.1"

module Damia where

import SqEq
import Options
import StrAtom

import Control.Applicative ((<**>))
import Data.List (intercalate)

import Options.Applicative  -- optparse-applicative
  ( execParser, helper, info, header, progDesc, fullDesc, help, metavar
  , argument, auto, option, value, long, short
  )

main :: IO ()
main = solve =<< execParser parserInfo
  where
    parserInfo =
      info
        (parser <**> helper)
        (  fullDesc
        <> progDesc "Solve square equation."
        <> header ("damia, version " ++ VERSION)
        )
    parser =
      Options
      <$> argument auto (metavar "A" <> help "x^2 coefficient")
      <*> argument auto (metavar "B" <> help "x coefficient")
      <*> argument auto (metavar "C" <> help "free coefficient")
      <*> option   auto (  metavar "LANGUAGE"
                        <> help "Language: En (default) / Ru"
                        <> value En
                        <> long "language"
                        <> short 'l'
                        )
      <*> option   auto (  metavar "FORMAT"
                        <> help "Format of result: Std (default) / Full"
                        <> value Std
                        <> long "format"
                        <> short 'f'
                        )
    solve (Options a b c lang form) =
      let d = discriminant a b c in
        putStrLn $
          if form == Full
            then
              intercalate " = "
                [ saMsgLn (fst (message lang)) "D"
                , "b"    `saDeg` "2" `saSub` "4" `saCnt` "a"    `saCnt` "b"
                , show b `saDeg` "2" `saSub` "4" `saMul` show a `saMul` show c
                , show (b * b)       `saSub` show (4 * a * c)
                , saEnd $ show $ discriminant a b c
                ]
              ++
              case solveSqEq a b c of
                [] -> snd (message lang)
                [x] ->
                  intercalate " = "
                    [ "x"
                    , "-b"      `saDiv` "2" `saCnt` "a"
                    , show (-b) `saDiv` "2" `saMul` show a
                    , show (-b) `saDiv` show (2 * a)
                    , show x
                    ]
                [x1,x2] ->
                  intercalate " = "
                    [ "x1"
                    , "-b"      `saAdd` saSqr "D"      `saDiv` "2" `saCnt` "a"
                    , show (-b) `saAdd` saSqr (show d) `saDiv` "2" `saMul` show a
                    , show (-b) `saAdd` saSqr (show d) `saDiv` show (2 * a)
                    , show (-b + sqrt d)               `saDiv` show (2 * a)
                    , saEnd (show x1)
                    ]
                    ++
                    intercalate " = "
                      [ "x2"
                      , "-b"      `saSub` saSqr "D"      `saDiv` "2" `saCnt` "a"
                      , show (-b) `saSub` saSqr (show d) `saDiv` "2" `saMul` show a
                      , show (-b) `saSub` saSqr (show d) `saDiv` show (2 * a)
                      , show (-b - sqrt d)               `saDiv` show (2 * a)
                      , saEnd (show x2)
                      ]
            else
              saMsg (fst (message lang)) $
                case solveSqEq a b c of
                  [] -> snd (message lang)
                  [x] -> "x" `saEqu` show x
                  [x1,x2] ->
                    intercalate ", "
                      [ "x1" `saEqu` show x1
                      , "x2" `saEqu` show x2
                      ]
    message lang =
      case lang of
        En -> ("Solution", "No solution...")
        Ru -> ("Решение", "Решение отсутствует...")
