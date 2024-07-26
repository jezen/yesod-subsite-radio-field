{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module SubApp
  ( module SubData
  , Route (..)
  ) where

import ClassyPrelude.Yesod
import SubData

instance YesodSubApp m => YesodSubDispatch SubApp m where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesSubApp)

data Foo = Foo | Bar | Baz
  deriving (Eq, Read, Show)

instance PathPiece Foo where
  toPathPiece = toLower . tshow
  fromPathPiece = \case
    "foo" -> Just Foo
    "bar" -> Just Bar
    "baz" -> Just Baz
    _ -> Nothing

fooForm :: YesodSubApp m => Html -> MForm (HandlerFor m) (FormResult Foo, WidgetFor m ())
fooForm extra = do
  foo <-
    let opts = mkOptionList (toOpt <$> [ Foo, Bar, Baz ])
     in mreq (radioField (pure opts)) "" Nothing
  pure (fst foo, [whamlet|
    $newline never
    #{extra}
    <p>Foo
    ^{fvInput (snd foo)}
    |])
  where
  toOpt :: Foo -> Option Foo
  toOpt p = Option
    { optionDisplay = tshow p
    , optionInternalValue = p
    , optionExternalValue = toPathPiece p
    }

getFooR :: YesodSubApp m => SubHandlerFor SubApp m Html
getFooR = do
  ((_, form), enctype) <- liftHandler $ runFormPost fooForm
  liftHandler $ defaultLayout [whamlet|
    $newline never
    <form enctype=#{enctype} method=post>
      ^{form}
      <button type=submit>Next
  |]

postFooR :: YesodSubApp m => SubHandlerFor SubApp m Html
postFooR = do
  toParent <- getRouteToParent
  ((result, _), _) <- liftHandler $ runFormPost fooForm
  print result
  redirect $ toParent FooR
