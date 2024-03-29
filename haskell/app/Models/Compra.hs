module Models.Compra where
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Calendar (Day)

data Compra = Compra {
    compra_id:: Int,
    compra_data:: Day,
    compra_price:: Double,
    user_id:: Int,
    game_id:: Int,
    avaliacao_compra:: Int,
    favoritar:: Bool
} deriving (Show, Read, Eq)

instance FromRow Compra where
    fromRow = Compra  <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field