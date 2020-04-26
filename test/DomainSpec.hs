module DomainSpec where

import Domain

import Control.Monad.Except
import Test.Hspec

spec :: SpecWith () 
spec = describe "values of type Domain" $ do

    it "can be something right" $ do
        v <- runExceptT (return 42 :: Domain Int)
        v  `shouldBe` Right 42

    it "can be a message in case of a failure" $ do
        v <- runExceptT (throwError "foo" :: Domain Int)
        v  `shouldBe` Left "foo"

    it "can be calculated in a chained computation" $ do
        let p :: Domain Int
            p = do
                let n = (return 42 :: Domain Int)
                let m = (return 17 :: Domain Int)
                (*) <$> n <*> m 
        v <- runExceptT p
        v  `shouldBe` Right 714

    describe "can be calculated in a computation " $ do
        let p :: Int -> Int -> Domain Int
            p x y = do
                let n = (return x :: Domain Int)
                let m = (return y :: Domain Int)
                if y /= 0 then div <$> n <*> m else throwError "divide by zero!"

        it "that succeeds" $ do
            v <- runExceptT $ p 714 42
            v  `shouldBe` Right 17

        it "that fails" $ do
            v <- runExceptT $ p 714 0
            v  `shouldBe` Left "divide by zero!"

        it "that uses IO" $ do
            writeFile "./temp.txt" "42"
            let r :: Domain Int
                r = do
                    s <- liftIO (readFile "./temp.txt")
                    case reads s of
                      [] -> throwError "uh?"
                      ((d,_):_) -> return d
            v <- runExceptT r
            v  `shouldBe` Right 42

    describe "catchIODomain" $ do
        it "yields a value from an IO action" $ do
            let a = return 42 :: IO Int
            v <- runExceptT (catchIODomain a)
            v `shouldBe` Right 42

        it "yields a message on IO action failure" $ do
            let a = fail "foo" :: IO Int
            v <- runExceptT (catchIODomain a)
            v `shouldBe` Left "user error (foo)"
