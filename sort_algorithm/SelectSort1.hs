module SelectSort1 where 

--
-- 最初に書いた、何か微妙な方法
_select prov [] = (prov, [])       -- 最小の数と、それを取り除いたリストを返す
_select prov (x:xs)
    | x < prov  = (minInt, (prov:list))
    | otherwise = (minInt, (   x:list))
    where
        (minInt, list) = _select newProv xs
        newProv         = min x prov

select (x:xs) = (minInt, list)
    where
        (minInt, list) = _select x xs

ssort [] = []
ssort xs = e: ssort list
    where
        (e, list) = select xs

