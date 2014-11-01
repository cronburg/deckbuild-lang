------------------------------------------------------------------------------
-- Library functions made from base language features:

{- Creates an Event filter function making the given player immune to Effects
   which affect them: -}
immune :: Player -> (Event -> Event)
immune p = (\ e -> if (e affects p) then (Event NULLEVENT) else e)

notimmune :: (Event -> Event)
notimmune = id

from :: (Cards -> Cards)
from = id

-- May do something to up to 'n' cards from 'pile' of cards
mayUpTo ::
mayUpTo fncn n pile
  | n <= 0    = return
  | otherwise = do
  { didIt <- may fncn (head pile)
  ; if didIt
      then mayUpTo fncn (n - 1) (tail pile)
      else mayUpTo fncn n       (tail pile)
  }

mayExactly' ::
mayExactly' fncn n pile
  | n <= 0     = return (0,pile)
  | pile == [] = return (0,pile)
  { didIt <- may fncn (head pile)
    if didIt
      then (\(x,y) -> (x+1,y)) 1 $ mayExactly' fncn (n - 1) (tail pile)
      else (\(x,y) -> (x+1,y)) 0 $ mayExactly' fncn n       (tail pile)
  }

-- May do something exactly 'n' times from 'pile'
mayExactly ::
mayExactly fncn n pile = do
  { (final_pile,num) <- mayExactly' fncn n pile
  ; return $ case () of _
    | num == 0  -> False
    | num == n  -> True
    | num <  n  -> mayExactly fncn (n - num) final_pile
    | otherwise -> undefined -- unreachable code
  }

-- May ______ up to 'n' cards from 'hand'
mayDiscardUpTo n hand = mayUpTo discard n hand
mayTrashUpTo   n hand = mayUpTo trash   n hand
mayDrawUpTo    n hand = mayUpTo draw    n hand

getPile p s  = (getNamedPile $ getPlayerName p) ++ s
getDeck p    = getPile p "'s Deck"
getDiscard p = getPile p "'s Discard Pile"
getHand p    = getPile p "'s Hand"
getTrash     = getNamedPile "Trash"

thisHand = getHand $ owner thisCard
thisDeck = getDeck $ owner thisCard

------------------------------------------------------------------------------
-- Example effect descriptions for various cards:

-- Cellar:
{ action { do
    -- May discard any number of cards from your hand
  { n <- mayDiscardUpTo (length thisHand) $ from thisHand
  ; -- Draw the number of cards discarded
  ; (owner thisCard) `draws` n
  }
}

-- Chapel:
{ action { mayTrashUpTo n (getHand $ owner thisCard) } }

-- Moat:
{ action   {} -- No special action effect (nothing except +2 cards)
  reaction {
    if (((cardTypeOf lastCardPlayed) is "Attack") and       -- When an attack is played
        (owner lastCardPlayed) != (owner thisCard)) then do -- by someone other than the player holding this card
      { revealed <- may reveal thisCard                   -- this card may be revealed
      ; if revealed                                         -- was this card revealed?
          then return (immune (owner thisCard))             --    TRUE  -> owner of this card is immune to this attack
          else return notimmune                             --    FALSE -> owner of this card is NOT immune
      }
      else return id
  }
}

-- Chancellor:
{ action { do { mayExactly discard (length thisDeck) thisDeck } } }



