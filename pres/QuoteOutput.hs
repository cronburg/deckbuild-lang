data CardName = CELLAR | CHAPEL | VILLAGE | WOODCUTTER | COPPER   
              | SILVER | GOLD   | ESTATE  | DUCHY      | PROVINCE
deriving (Eq, Typeable, Show, Ord)

data RuntimeCard = RuntimeCard { cID    :: !CardName
                               , cType  :: !CardType
                               , cDescr :: !CardDescr
                               , cCost  :: !CardCost }
deriving (Eq, Typeable, Show, Ord)

kingdomCards = [ RuntimeCard
                   { cID    = CELLAR
                   , cType  = ACTION
                   , cDescr = CardDescr
                                { primary = [Effect {amount = 1, effectType = ACTIONS}]
                                , other   = "Discard any number of cards. +1 Card per card discarded"
                                }
                   , cCost   = 2 }
                
                , ...

                , RuntimeCard
                   { cID    = PROVINCE
                   , cType  = VICTORY
                   , cDescr = CardDescr
                               { primary = [Effect {amount = +6, effectType = VICTORYPOINTS}]
                               , other   = []
                               }
                   , cCost = 8 }
                ]

turnRules = [ Turn
              { turnID     = "Dominion_Standard"
              , turnPhases =
                [ Phase {phaseName = ActionP,  phaseInt = PhaseInt 1 }
                , Phase {phaseName = BuyP,     phaseInt = PhaseInt 1 }
                , Phase {phaseName = DiscardP, phaseInt = All        }
                , Phase {phaseName = DrawP,    phaseInt = PhaseInt 5 }
                ] } ]
 


