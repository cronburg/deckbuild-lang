[deck|
card Cellar  :: Action {
  +1 actions
  "Discard any number of cards."
  " +1 Card per card discarded"
} costs 2

card Chapel  :: Action {
  "Trash up to 4 cards from your hand"
} costs 2

card Village :: Action {
  +1 cards
  +2 actions
} costs 3

card Woodcutter :: Action {
  +1 buys
  +2 coins
} costs 3

card Copper :: Treasure {
  +1 coins
} costs 0

card Silver :: Treasure {
  +2 coins
} costs 3

card Gold :: Treasure {
  +3 coins
} costs 6

card Estate :: Victory {
  +1 victory
} costs 2

card Duchy :: Victory {
  +3 victory
} costs 5

card Province :: Victory {
  +6 victory
} costs 8

turn Dominion_Standard {
  action 1
  buy 1
  discard all
  draw 5
}
|]
