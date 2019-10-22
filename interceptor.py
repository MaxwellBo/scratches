def intercept(f):
    def interceptor(*args, **kwargs):
        print("args", args)
        print("kwargs", kwargs)

        f(*args, **kwargs)

    return interceptor

@intercept
def foo(x, y, bar=True):
    pass

foo(3, 5, bar=False)

