{-# LANGUAGE OverloadedStrings #-}

module SubAppSpec where

import TestImport

spec :: Spec
spec = withApp $ do

  it "Allows the user to choose an option" $ do
    get $ SubR FooR
    request $ do
      addToken
      setMethod "POST"
      setUrl $ SubR FooR
      byLabelExact "Foo" "foo"
    statusIs 303
