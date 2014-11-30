things_derived = [mkName "Eq", mkName "Typeable", mkName "Show", mkName "Ord"]

make_deck_declaration :: [DeckDecl] -> Q [Dec]
make_deck_declaration ds = do
    card_es <- mapM genDeckExp $ filter      isCard  ds :: Q [Exp]
    turn_es <- mapM genDeckExp $ filter (not.isCard) ds
    let kingdomCardBody = NormalB $ ListE card_es
    let turnRulesBody   = NormalB $ ListE turn_es
    let name_constructors = genCons ds
    let card_constructors = genCardCons
    return [ DataD [] (mkName "CardName")     [] name_constructors things_derived
           , DataD [] (mkName "RuntimeCard")  [] card_constructors things_derived
           , FunD     (mkName "kingdomCards") [Clause [] kingdomCardBody []] 
           , FunD     (mkName "turnRules")    [Clause [] turnRulesBody   []] 
           ]   

genCons :: [DeckDecl] -> [Con]
genCons ds = [NormalC (mkCardName d) [] | d <- ds, isCard d]

genCardCons :: [Con]
genCardCons = [ RecC (mkName "RuntimeCard")
                [ (mkName "cID",    IsStrict, ConT $ mkName "CardName")
                , (mkName "cType",  IsStrict, ConT $ mkName "CardType")
                , (mkName "cDescr", IsStrict, ConT $ mkName "CardDescr")
                , (mkName "cCost",  IsStrict, ConT $ mkName "CardCost")
                ]   
              ]

isCard :: DeckDecl -> Bool
isCard (DeckDeclCard c) = True
isCard _                = False

mkCardName :: DeckDecl -> Name
mkCardName (DeckDeclCard (Card {cID = name})) = mkName $ map toUpper name
mkCardName _ = undefined

genDeckExp :: DeckDecl -> Q Exp 
genDeckExp e = liftD e --runQ [| e |]
