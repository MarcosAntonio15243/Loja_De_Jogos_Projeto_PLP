{-# LANGUAGE OverloadedStrings #-}

module Models.Denuncia where
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Calendar (Day)
import Data.Int (Int64, Int)

data Denuncia = Denuncia {
    denuncia_id:: Int64,
    user_id:: Int64,
    game_id:: Int64,
    denuncia_motivo:: String,
    denuncia_descricao:: String,
    denuncia_data:: Day
} deriving (Show, Read, Eq)

instance FromRow Denuncia where
    fromRow = Denuncia  <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
