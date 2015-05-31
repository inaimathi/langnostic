module New where

main = do fmap (length . lines) $ readFile "posts.json"
