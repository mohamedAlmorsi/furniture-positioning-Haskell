findFurnitureUpdate a b c []=[]
findFurnitureUpdate a b c (x:xs) = check a b c x ++ findFurnitureUpdate a b c xs
check _ _ _ (str,[]) = []
check a b c (str,[[(str1,position,i):(str11:xs)]]) | a==str && b== str1 && c== position =[(str1,position,i1)] ++ check a b c (str11, modifyStruct xs )
                                           | otherwise = (str1,position,i) ++ check a b c xs
                                            where i1 = i+1 
modify


generate [] list=list
generate [x] list= findFurnitureUpdate x list 
generate (x:xs) list = generate x1  findFurnitureUpdate x list where x1 = head xs 



