{-# LANGUAGE OverloadedStrings #-}

module Models.Jogo where
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Calendar (Day)

data Jogo = Jogo {
    game_id:: Int,
    game_nome:: String,
    game_genero:: String,
    game_description:: String,
    game_data_lancamento:: Day,
    game_avaliacao:: Int,
    game_price:: Double
} deriving (Show, Read, Eq)

instance FromRow Jogo where
    fromRow = Jogo  <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
