{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import System.Random
import Control.Monad  
import Data.Char  
import Network.Mail.Mime

--takes lists, shuffles it and then copy-creates new list with head as last item and zip lists together 
santaList :: [a] -> [(a,a)]
santaList xs = zip n m 
    where n = myShuffle xs 
          m = tail n ++ [head n]

-- Some helper functions
myShuffle :: [a] -> [a]
myShuffle [] = []
myShuffle xs = tupleToList $ zip n m
    where n = reverse . fst $ splitAt (length xs `div` 2) xs
          m = snd $ splitAt (length xs `div` 2) xs

tupleToList :: [(a,a)] -> [a]
tupleToList ((a,b):xs) = a : b : tupleToList xs
tupleToList _          = []


-- Create a person 
data Person = Person { firstName :: String    
                     , email :: String  
                     } deriving (Show)   

-- Get input
myList xs = do
    putStrLn "Enter your name"
    name <- getLine
    if null name
        then return ()
        else do  
            putStrLn "Enter your e-mail"
            email <- getLine
            if null email
                then return ()
                else do
                    let person = Person name email
                    print person
                    myList (person:xs)
    let c = (santaList xs)
    print c
    mailList c
    
-- (L.fromStrict $ T.pack("Hi " ++ firstName p1 ++ "!\nYour secret santa result is: " ++ firstName p2))

createText :: (Person,Person) -> T.Text
createText (a, b) = T.pack ("Hi " ++ firstName a ++ "!\nYour secret santa result is: " ++ firstName b)

-- Send email to list of tuples: first person in tuple is recipient.
mailList :: [(Person, Person)] -> IO ()
mailList [] = return ()
mailList ((p1, p2):ps) = do
    a <- simpleMail
            (Address Nothing (T.pack "haskelltrial@gmail.com"))
            (Address Nothing (T.pack $ email p1))
            "Secret Santa"
            "TEST"
            "TEST"
            []
    b <- renderMail' a
    sendmail b
    mailList ps
  
main = myList []