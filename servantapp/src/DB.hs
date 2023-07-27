{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module DB where

import Database.Beam hiding (insertExpressions)
import qualified Database.Beam as Beam

import Data.Text (Text)    
data UserT f
    = User
    { _user_id     :: Columnar f Text
    , _name :: Columnar f Text
    , _last_name  :: Columnar f Text 
    }
    deriving (Generic)


type User = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Beamable UserT
instance Eq User
instance Show User

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UserId . _user_id

userinsertExpressions cs = Beam.insertExpressions (toRowExpression <$> cs)
  where
    toRowExpression User {..} =
      User
        (Beam.val_ _user_id)
        (Beam.val_ _name)
        (Beam.val_ _last_name)
        
data CurrentDb f = CurrentDb
                      { _usersTable :: f (TableEntity UserT) }
                        deriving (Generic, Database be)

currentDb :: DatabaseSettings be CurrentDb --replace a text with a type
currentDb   =
  defaultDbSettings `withDbModification`
  dbModification
    { _usersTable = userEMod
    }

userEMod :: EntityModification (DatabaseEntity be db) be (TableEntity UserT)
userEMod = setEntityName "ussers" <>
        modifyTableFields
          tableModification {
              _user_id = "user_id"
            , _name  = "name"
            , _last_name = "last_name" }