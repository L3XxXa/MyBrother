{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where

import           Data.Text
import           Data.Time
import           Database.Persist.Sql
import           Database.Persist.TH

--There we make smth like schema (or model) for our bot.

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Income
    source Text
    amount Double
    when   UTCTime
    deriving Show Eq
  Expense
    towhom Text
    amount Double
    when   UTCTime
    deriving Show Eq
  |]

-- In this func we create DB
doMigration :: SqlPersistT IO ()
doMigration = runMigration migrateAll
