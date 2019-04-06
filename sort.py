import random


def bswap(x, *ys) -> list:
    """一番小さな要素を先頭に移動する"""
    
    if not ys:
        return [x]

    y, *zs = bswap(*ys)

    if x < y:
        return [x, y, *zs]
    else:
        return [y, x, *zs]


def bsort(*xs) -> list:
    if not xs:
        return xs

    x, *ys = bswap(*xs)
    return [x, *bsort(*ys)]
    

def main():
    lst = [i for i in range(10)]
    random.shuffle(lst)

    print(lst)
    print(bsort(*lst))


if __name__ == '__main__':
    main()
