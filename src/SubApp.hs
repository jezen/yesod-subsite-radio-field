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

-- TODO: Improve, and upstream
--
-- The original implementation in the yesod-form library has two problems.
--
-- 1. The markup it produces is awkward to work with, and doesn't actually work
--    together with the `byLabelExact` function from the yesod-test library.
--    This custom implementation is better for actual use (like with CSS), and
--    also works with the `byLabelExact` function.
--
-- 2. The type signature specialises this function to `HandlerFor site a`, which
--    means it's awkward to use this field (and also checkbox or select fields)
--    in a subsite; you need to lift the entire form into the master site. It's
--    not enough to change the type signature here; this function is implemented
--    in terms of `withRadioField`, which also specialises. And, of course, the
--    `withRadioField` function is implemented in terms of `selectFieldHelp`,
--    which also specialises… ಠ_ಠ
radioField' :: (Eq a, RenderMessage site FormMessage)
           => HandlerFor site (OptionList a)
           -> Field (HandlerFor site) a
radioField' = withRadioField
    (\theId optionWidget -> [whamlet|
$newline never
<.radio>
  ^{optionWidget}
  <label for=#{theId}-none>
    _{MsgSelectNone}
|])
    (\theId value _isSel text optionWidget -> [whamlet|
$newline never
<.radio>
  ^{optionWidget}
  <label for=#{theId}-#{value}>
    \#{text}
|])

data Foo = Foo | Bar | Baz
  deriving (Eq, Read, Show)

instance PathPiece Foo where
  toPathPiece = toLower . tshow
  fromPathPiece = \case
    "foo" -> Just Foo
    "bar" -> Just Bar
    "baz" -> Just Baz
    _ -> Nothing

fooForm :: YesodSubApp m => Html -> MForm (SubHandlerFor SubApp m) (FormResult Foo, WidgetFor m ())
fooForm extra = do
  foo <-
    let opts = mkOptionList (toOpt <$> [ Foo, Bar, Baz ])
     in mreq (radioField' (pure opts)) "" Nothing
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
  ((_, form), enctype) <- runFormPost fooForm
  liftHandler $ defaultLayout [whamlet|
    $newline never
    <form enctype=#{enctype} method=post>
      ^{form}
      <button type=submit>Next
  |]

postFooR :: YesodSubApp m => SubHandlerFor SubApp m Html
postFooR = do
  toParent <- getRouteToParent
  ((result, _), _) <- runFormPost fooForm
  print result
  redirect $ toParent FooR
