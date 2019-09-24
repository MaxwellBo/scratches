d_min = min([10, 5])
F = 100
u_s = 100

def cs(N):
    a = (N * F) / u_s
    b = F / d_min

    return max(a, b)

def p2p(N):
    a = F / u_s
    b = F / d_min
    u = sum([0.2] * (N//2) + [1] * (N//2))
    c = (N * F) / (u_s + u)

    return max(a, b, c)


print(p2p(10))
print(p2p(100))
print(p2p(1000))

print(cs(10))
print(cs(100))
print(cs(1000))

class Test(object):
    def meth(_):
        return 5

x = Test()
print(x.meth())

class SomeClass(object):
    def x(self):
        return "5"

p1 = SomeClass()

print(SomeClass.x(p1))
print(p1.x())

x = lambda x : x + 1


def external_def(self, x):
    return x

SomeClass.external_def = external_def

print(p1.external_def(5))

def implies(p, q):
    if p and q:
        return True
    elif p and not q:
        return False
    else:
        return True
