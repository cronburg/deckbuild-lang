-- Discards any number of cards, returning the number of cards discarded
cellarEffect' :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m Int 
cellarEffect' = do
  g  <- get 
  c' <- liftIO $ ((mayPick.p1) g) g CELLAR
  case c' of
    Just c  -> if   elem c ((cards.hand.p1) g)
               then discard c >> cellarEffect' >>= \n -> return $ n + 1 
               else return 0
    Nothing -> return 0

-- Discard any number of cards, then draw that many cards:
cellarEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
cellarEffect = addActions 1 >> cellarEffect' >>= \n -> draw n
