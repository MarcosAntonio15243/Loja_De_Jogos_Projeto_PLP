{-# LANGUAGE OverloadedStrings #-}

module Models.Usuario where
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Calendar (Day)

data Usuario = Usuario {
    user_id:: Int,
    user_nome:: String,
    user_email:: String,
    user_senha:: String,
    user_tipo:: String,
    user_saldo:: Double,
    user_date:: Day
} deriving (Show, Read, Eq)

instance FromRow Usuario where
    fromRow = Usuario  <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field