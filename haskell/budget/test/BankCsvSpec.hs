module BankCsvSpec
    where
import Data.Csv
import Amount
import Expense
import Test.Hspec
import qualified Data.ByteString.Lazy as B
import Bank

spec = do
    describe "import Expenses From Bank" $ do
        it "decode a csv string from the bank including when double negative amount, and amount without decimal" $ do
            let bank = [("posted", "", "02/25/2020", ""                   , "GOOGLE  DOMAINS" , "Online Services", "--12.00")
                       ,("posted", "", "02/24/2020", "Adventures in Clean", "TRANSFERWISE INC", "Training"       , "-1242.26")
                       ,("posted", "", "02/24/2020", "Panera"             , "Panera"          , "Food"           , "42")]
                csv = encode bank
                exps = importExpensesFromBank csv :: Either String [Expense]
            exps `shouldBe` Right [Expense (mkDate 2020 02 25) "Online Services" (mkAmount 12.00)
                                  ,Expense (mkDate 2020 02 24) "Training" (mkAmount (-1242.26))
                                  ,Expense (mkDate 2020 02 24) "Food" (mkAmount 42.00)]
