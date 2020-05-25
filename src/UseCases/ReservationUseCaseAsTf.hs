module UseCases.ReservationUseCaseAsTf
( listAll
, fetch
, tryReservation
, cancel
, availableSeats
, Persistence (..)
, ReservationError (..)
, Dom.Reservation (..)
, Dom.ReservationMap (..)
)
where

import           Control.Monad            (when)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe)
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Domain.ReservationDomain as Dom (Reservation (..),
                                                  ReservationMap (..),
                                                  addReservation,
                                                  availableSeats,
                                                  cancelReservation,
                                                  isReservationPossible)

import           UseCases.KVSasTF             
import           Numeric.Natural
import           Control.Exception.Base

{--
This module specifies the Use Case layer for the Reservation system.
It coordinates access to Effects and the actual domain logic.
The module exposes service functions that will be used by the REST API in the External layer.

Implemented Use Cases:

1. Display the number of available seats for a given day

2. Enter a reservation for a given day and keep it persistent.
   If the reservation can not be served as all seats are occupies prode a functional error message stating
   the issue.

3. Display the list of reservations for a given day.

4. Delete a given reservation from the system in case of a cancellation.
   NO functional error is required if the reservation is not present in the system.

5. Display a List of all reservation in the system.


All Effects are specified as Polysemy Members.

Interpretation of Effects is implemented on the level of application assembly, or in the context of unit tests.

Please note: all functions in this module are pure and total functions.
This makes it easy to test them in isolation.
--}


-- | The functional error, raised if a reservation is not possible
newtype ReservationError = ReservationNotPossible String deriving (Show, Eq)

instance Exception ReservationError

-- | compute the number of available seats for a given day.
-- | Implements UseCase 1.
availableSeats :: (Persistence Day [Dom.Reservation] m, Trace m) => Day -> m Natural
availableSeats day = do
  trace $ "compute available seats for " ++ show day
  todaysReservations <- fetch day
  return $ Dom.availableSeats maxCapacity todaysReservations

-- | the maximum capacity of the restaurant.
-- | to keep things simple this just a constant value of 20.
-- | In real life this would kept persistent in a database, and would be accessed by yet another abstract effect.
maxCapacity :: Natural
maxCapacity = 20

-- | try to add a reservation to the table.
-- | Return Just the modified table if successful, else return Nothing
-- | implements UseCase 2.
tryReservation :: (Persistence Day [Dom.Reservation] m, Trace m) => Dom.Reservation -> m ()
tryReservation res@(Dom.Reservation date _ _ requestedQuantity)  = do
  trace $ "trying to reservate " ++ show requestedQuantity ++ " more seats on " ++ show date
  todaysReservations <- fetch date
  let availableSeats = Dom.availableSeats maxCapacity todaysReservations
  if Dom.isReservationPossible res todaysReservations maxCapacity
    then persistReservation res
    else throw $ ReservationNotPossible ("Sorry, only " ++ show availableSeats ++ " seats left on " ++ show date)

  where
    -- | persist a reservation to the reservation table.
    persistReservation :: (Persistence Day [Dom.Reservation] m, Trace m)  => Dom.Reservation -> m ()
    persistReservation r@(Dom.Reservation date _ _ _ ) = do
      trace $ "enter a new reservation to KV store: " ++ show r
      rs <- fetch date
      let updated = Dom.addReservation r rs
      trace $ "storing: " ++ show updated
      insertKvs date updated

-- | fetch the list of reservations for a given day from the key value store.
-- | If no match is found, an empty list is returned.
-- | Implements UseCase 3.
fetch :: (Persistence Day [Dom.Reservation] m, Trace m) => Day -> m [Dom.Reservation]
fetch day = do
  trace $ "fetch reservations for " ++ show day
  maybeList <- getKvs day
  return $ fromMaybe [] maybeList

-- | cancel a reservation, that is: delete it from the system.
-- | Implements UseCase 4.
cancel :: (Persistence Day [Dom.Reservation] m, Trace m)  => Dom.Reservation -> m ()
cancel res@(Dom.Reservation date _ _ _) = do
  trace $ "deleting reservation " ++ show res
  reservations <- fetch date
  trace $ "before: " ++ show reservations
  let after = Dom.cancelReservation res reservations
  trace $ "after: " ++ show after
  insertKvs date after


-- | list all entries from the key value store and return them as a ReservationMap
-- | Implements UseCase 5.
listAll :: (Persistence Day [Dom.Reservation] m, Trace m) => m Dom.ReservationMap
listAll = do
  trace "listing all reservation entries"
  fmap M.fromList listAllKvs


