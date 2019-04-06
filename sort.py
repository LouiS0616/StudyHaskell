"""再帰に依るソーティングアルゴリズムの実装"""
import random


#
# Insert Sort
def insert(elem, sorted_lst) -> list:
    if not sorted_lst:
        return [elem]

    x, *xs = sorted_lst
    if elem <= x:
        return [elem, x, *xs]
    else:
        return [x, *insert(elem, xs)]

def isort(src) -> list:
    if not src:
        return []
    
    x, *xs = src
    return [*insert(x, isort(xs))]



#
# Bubble Sort
def bswap(src) -> list:
    """一番小さな要素を先頭に移動する"""
    
    x, *ys = src
    if not ys:
        return [x]

    y, *zs = bswap(ys)

    if x < y:
        return [x, y, *zs]
    else:
        return [y, x, *zs]


def bsort(src) -> list:
    if not src:
        return []

    x, *ys = bswap(src)
    return [x, *bsort(ys)]
    

#
# Merge Sort
def merge(left, right) -> list:
    if not left:
        return right
    if not right:
        return left

    lx, *lxs = left
    rx, *rxs = right
    if lx < rx:
        return [lx, *merge(lxs, right)]
    else:
        return [rx, *merge(left, rxs)]

def msort(src) -> list:
    x, *xs = src
    if not xs:
        return [x]

    n = len(src) // 2

    left  = msort(src[:n])
    right = msort(src[n:])

    return merge(left, right)


#
# Quick Sort 
def qsort(src) -> list:
    if not src:
        return []

    axis, *xs = src
    left  = [e for e in xs if e < axis]
    right = [e for e in xs if axis <= e]

    return qsort(left) + [axis] + qsort(right)


#
#
def main():
    lst = [i for i in range(1, 10)]
    random.shuffle(lst)

    print(lst, end='\n\n')
    for alg in [isort, bsort, msort, qsort]:
        print(alg(lst))


if __name__ == '__main__':
    main()
