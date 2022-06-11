module Play where
import Data.Char ( isPunctuation, toLower, isAlpha ) 

isPalindrome :: String -> Maybe Bool
isPalindrome =
    reverseMaybe . rejectEmpty . normalize 

tReverse :: [Char] -> Bool
tReverse t =
    t == reverse t

rejectEmpty :: [Char] -> Maybe [Char]
rejectEmpty str = 
    case str of
        [] -> Nothing
        _ -> Just str
 
normalize :: [Char] -> [Char]
normalize = filter  tPunctuation . filter noSpace . tLowerCase

-- Maybe a = Nothing | Just a --
reverseMaybe :: Maybe String -> Maybe Bool
reverseMaybe word =
    case word of 
        Nothing -> Nothing
        Just w -> Just (tReverse w)

tLowerCase :: [Char] -> [Char]
tLowerCase = map toLower

noSpace :: Char -> Bool
noSpace t = not (t == ' ')

tPunctuation :: Char -> Bool
tPunctuation x = not (isPunctuation x)

-- Ignore nonAlphabetics
-- Use foldr and all and lambda function
-- Learn Haskell thunks
ignoreNonAlpha :: String -> Maybe String
ignoreNonAlpha x = 
    if all isAlpha x
        then Just x
        else Nothing
        