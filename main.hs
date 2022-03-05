import Play ( isPalindrome )

main :: IO ()
main = 
    do
        word <- getLine
        print (msgMethod word)

msgMethod :: String -> String
msgMethod word = 
    case isPalindrome word of 
        Nothing -> "Enter a palindrome."
        Just False -> "Oops! Not a palindrome"
        Just True -> "Got it!"


