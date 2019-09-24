def test():
    lst = []
    for i in range(100):
        lst.append(i)

def test2():
    [ i for i in range(100) ]

if __name__ == '__main__':
    import timeit
    print(timeit.timeit("test()", setup="from __main__ import test"))
    print(timeit.timeit("test2()", setup="from __main__ import test2"))
