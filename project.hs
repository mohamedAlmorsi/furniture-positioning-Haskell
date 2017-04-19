module FurnitureResources where

import System.Random
import System.IO.Unsafe

-- randomly chosen from the range [0 .. x]            
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x))) 

furniture2 = ["couch","table","lamp","tv","e"]

room1 = [["e","e","e","lamp","table"],["tv","e","table","e","couch"],["tv","e","table","e","couch"],["tv","e","e","e","couch"],["e","e","e","lamp","table"]]
room2 = [["couch","couch","couch","table","lamp"],["couch","e","e","e","e"],["couch","e","table","e","tv"],["couch","e","table","e","tv"],["table","e","e","e","e"]]
room3 = [["e","tv","tv","tv"],["e","e","e","e"],["e","table","table","table"],["table","couch","couch","couch"]]

training =  [room1,room2,room3]

-- this method takes a room , elemnt , the elemnt on its right and returns the occurence of this right element on the right of the element
next1 [] _ _ = 0
next1 [x] _ _=0
next1 (x:xs) element right = nexthelper x element right + next1 xs element right
nexthelper [] _ _ = 0
nexthelper [x] _ _ =0
nexthelper (x:xs) element right | x== element && x1==right = 1+ nexthelper xs element right
                                | otherwise = nexthelper xs element right
                                where x1 = head xs 

elementsNextToE (x:xs) = [("e", (next1 (x:xs) "e" "e")),("tv",(next1 (x:xs) "e" "tv")),("lamp",(next1 (x:xs) "e" "lamp")),("couch",(next1 (x:xs) "e" "couch")),("table",(next1 (x:xs) "e" "table"))]
elementsNextToTv (x:xs) = [("e", (next1 (x:xs) "tv" "e")),("tv",(next1 (x:xs) "tv" "tv")),("lamp",(next1 (x:xs) "tv" "lamp")),("couch",(next1 (x:xs) "tv" "couch")),("table",(next1 (x:xs) "tv" "table"))]
elementsNextToLamp (x:xs) = [("e", (next1 (x:xs) "lamp" "e")),("tv",(next1 (x:xs) "lamp" "tv")),("lamp",(next1 (x:xs) "lamp" "lamp")),("couch",(next1 (x:xs) "lamp" "couch")),("table",(next1 (x:xs) "lamp" "table"))]
elementsNextToTable (x:xs) = [("e", (next1 (x:xs) "table" "e")),("tv",(next1 (x:xs) "table" "tv")),("lamp",(next1 (x:xs) "table" "lamp")),("couch",(next1 (x:xs) "table" "couch")),("table",(next1 (x:xs) "table" "table"))]
elementsNextToCouch (x:xs) = [("e", (next1 (x:xs) "couch" "e")),("tv",(next1 (x:xs) "couch" "tv")),("lamp",(next1 (x:xs) "couch" "lamp")),("couch",(next1 (x:xs) "couch" "couch")),("table",(next1 (x:xs) "couch" "table"))]

checkinRB a b c [] =   [(b,c,1)]
checkinRB a b c ((name,position,freq):xs)= 
  if b==name && c==position then (name,position,freq+1):xs else (name,position,freq):checkinRB a b c xs


check a b c (str,[r,bot]) = if  c =="right" then  (str,[(checkinRB a b c r),bot]) else (str,[r,checkinRB a b c bot])

findFurnitureUpdate a b c []=[]
findFurnitureUpdate a b c ((name,[r,bot]):xs) = if(a==name)then (check a b c (name,[r,bot])):xs else (name,[r,bot]):(findFurnitureUpdate a b c xs)

generate [] list=list
generate [x] list = generateHelperRight x list -- tmam
generate (x:xs) list = generate [x1]  (generateHelperRight x list) where x1 = head xs 

generateHelperRight (x:xs) list = findFurnitureUpdate x x1 "right" list where x1 = head xs

statList = helper2 training [] 
helper2 [] list=list
helper2 (x:xs) list = helper2 xs (generate x list)















