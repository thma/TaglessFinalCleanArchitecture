module UseCaseIOTfSpec where

import           Test.Hspec

import           Control.Exception
import           Control.Monad.Except
import           Data.Function                    ((&))
import           Data.List                        (isSuffixOf)
import qualified Data.Map.Strict                  as M
import           Data.Time.Calendar
import           Domain.ReservationDomain
import           InterfaceAdapters.Config
import           InterfaceAdapters.KVSSqliteTF

import           System.Directory                 (doesFileExist, listDirectory,
                                                   removeFile)
import           UseCases.ReservationUseCaseAsTf
import           UseCases.KVSasTF
import           Data.Aeson.Types (ToJSON, FromJSON)



main :: IO ()
main = hspec spec

instance ToJSON Reservation
instance FromJSON Reservation

-- | helper function to clean the test data files
--deleteAllFiles :: IO [()]
--deleteAllFiles = do
--  allFiles <- listDirectory "."
--  let filteredFiles = filter (isSuffixOf ".json") allFiles
--  mapM removeFile (map (\f -> dataDir ++ f) filteredFiles)


day = fromGregorian 2020 5 2
res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

spec :: Spec
spec =
  --return deleteAllFiles
  describe "Reservation Use Case TF" $ do
    it "needs a file cleaning for repeatable tests in the file system..." $ do    
      removeFile "kvs-TF.db"  
      map <- listAll
      M.size map `shouldBe` 0

    it "returns Nothing if there are no reservations for a given day" $ do
      result <- fetch day
      result `shouldBe` []

    it "can add a reservation if there are enough free seats" $ do
      let goodReservation = head res
      tryReservation goodReservation
      map <- listAll
      M.size map `shouldBe` 1
      reservations <- fetch day
      goodReservation `elem` reservations `shouldBe` True

    it "fetches a list of reservations from the KV store" $ do
      result <- fetch day
      result `shouldBe` res

    it "can retrieve a map of all reservations" $ do
      map <- listAll
      M.size map `shouldBe` 1

    it "throws an error if a reservation is not possible" $ do
      let badReservation = Reservation day "Gabriella. Miller" "gm@example.com" 17
      tryReservation badReservation `shouldThrow` reservationException ("Sorry, only 16 seats left on " ++ show day)

    it "can cancel a reservation" $ do
      let res1 = head res
      tryReservation res1
      reservations <- fetch day
      length reservations `shouldBe` 2

      cancel res1

      reservations' <- fetch day
      length reservations' `shouldBe` 1

reservationException :: String -> Selector ReservationError
reservationException s (ReservationNotPossible msg) = s == msg