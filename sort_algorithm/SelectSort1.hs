module SelectSort1 where 

--
-- �ŏ��ɏ������A���������ȕ��@
_select prov [] = (prov, [])       -- �ŏ��̐��ƁA�������菜�������X�g��Ԃ�
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

