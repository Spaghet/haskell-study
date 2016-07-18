safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

safeLast :: [a] -> Maybe a
safeLast xs = safeHead . reverse $ xs

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeInit :: [a] -> Maybe [a]
safeInit xs =
  case safeTail . reverse $ xs of
    Nothing -> Nothing
    Just ys -> Just (reverse ys)
