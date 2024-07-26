{-# LANGUAGE OverloadedStrings #-}

module SubAppSpec where

import TestImport

-- This is a function I wish existed. Obviously, the value should come from the
-- input associated with the label, and not be hard-coded like we have here.
choose :: Text -> RequestBuilder site ()
choose label = byLabelExact label "foo"

spec :: Spec
spec = withApp $ do

  it "Allows the user to choose an option" $ do
    get $ SubR FooR
    request $ do
      addToken
      setMethod "POST"
      setUrl $ SubR FooR
      choose "Foo"
    statusIs 303
