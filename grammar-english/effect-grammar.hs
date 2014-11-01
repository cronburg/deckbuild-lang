Keywords / Haskell functions:
-----------------------------

data Event = Event { 
data Card  = Card CardID CardType 
type Cards = [Card]

thisCard     :: Game -> Card         -- gets a ref / copy of the current card
thisHand     :: Game -> Cards        -- gets a ref / copy of the hand owned by 'thisCard'
reveal       :: Game -> Card -> Game -- reveals the given card in the Game monad
discard      :: Game -> Card -> Game -- discards the given card in the Game monad
playCard     :: Game -> Card -> Game
draw         :: Game -> Cards -> Game

draws        :: Game -> Player -> Int -> Game
gains        :: Game -> Player -> Card -> Game

-- returns a list of the top card of each pile in the supply
getSupply :: Cards

may :: -- ask a player if they want to do something to a specific card

-- ask a player to select 1 or 0 cards from a list of cards
--   i.e. concurrently do multiple may's
manymay ::

-- Getters for the various piles of cards in a deck building game:
getDeck      :: Game -> Player -> Cards
getDiscard   :: Game -> Player -> Cards
getHand      :: Game -> Player -> Cards
getTrash     :: Game -> Cards
getNamedPile :: Game -> String -> Cards -- 
getPlayerPile :: Game -> Player -> String -> Cards

getPlayerName :: Game -> Player -> String

------------------------------------------------------------------------------
-- Only useable by interpreter:

-- A Reaction is a function taking an Event, and returning an altered version of
-- that event (makes it do something else)
type Reaction = (Event -> Event)

