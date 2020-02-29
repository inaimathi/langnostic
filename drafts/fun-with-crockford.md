I'm trying something new this time. I've recently watched and followed along with [this video](https://www.youtube.com/watch?v=Aa_OWn03mDo), which is a long talk/exercise lecture by [Douglas Crockford](https://www.crockford.com/add.html). The rest of this post is a giant, literate-ish code-block that encodes my attempt at following along in Python.

```
class TrivialTest:
    # I want to write tests as I go, so I need a bit of plumbing first.
    def __init__(self):
        self.failures = []
        self.tested = 0

    def trivial_test(self, *test_forms):
        self.tested += 1
        print(f"--- [{self.tested}]---------------------------")
        for test_form in test_forms:
            res = eval(test_form)
            msg = f"{res} <= [ {test_form} ]"
            if not res:
                self.failures.append((self.tested, msg))
            print(msg)

    def report(self):
        print(f"==============================")
        if not self.failures:
            print("ALL TESTS PASSED")
        else:
            for block, fail in self.failures:
                print(f"{block} => {fail}")
        self.failures = []
        self.tested = 0

tests = TrivialTest()
tt = tests.trivial_test  # I'm lazy, fuck you

def identity(x):
    return x

tt("identity(3) == 3")


def add(a, b):
    return a + b

def sub(a, b):
    return a - b

def mul(a, b):
    return a * b

def identityf(value):
    return lambda: value

tt("identityf(3)() == 3", "identityf(3)() == identity(3)")


def addf(a):
    return lambda b: a + b

tt("addf(3)(4) == 7", "addf(3)(4) == add(3, 4)")


def liftf(fn):
    return lambda a: lambda b: fn(a, b)

tt("liftf(add)(3)(4) == addf(3)(4) == add(3, 4)")


def curry(fn, a):
    return liftf(fn)(a)

def curry(fn, *args):
    return lambda *brgs: fn(*args, *brgs)

tt(
    "curry(add, 3)(4) == 7",
    "curry(mul, 5)(6) == 30",
    "curry(mul, 5)(6) == liftf(mul)(5)(6)",
)

inc_tests = ["inc(1) == 2", "inc(inc(1)) == 3", "inc(5) == 6"]

inc = curry(add, 1)
tt(*inc_tests)
inc = liftf(add)(1)
tt(*inc_tests)
inc = addf(1)
tt(*inc_tests)


def twice(fn):
    return lambda a: fn(a, a)

doubl = twice(add)
square = twice(mul)

tt("doubl(11) == 22", "square(11) == 121")


def reverse(fn):
    return lambda a, b: fn(b, a)

tt("reverse(sub)(3, 2) == -1")


def composeu(fna, fnb):
    return lambda a: fnb(fna(a))

tt("composeu(doubl, square)(5) == 100")


def composeb(fna, fnb):
    return lambda a, b, c: fnb(fna(a, b), c)

tt("composeb(add, mul)(2, 3, 7) == 35")


def limit(fn, times):
    def _internal(*args):
        nonlocal times
        times -= 1
        if times >= 0:
            return fn(*args)

    return _internal

add_ltd = limit(add, 1)
tt("add_ltd(3, 4) == 7", "add_ltd(3, 5) is None")


def from_(initial):
    initial -= 1

    def _internal():
        nonlocal initial
        initial += 1
        return initial

    return _internal

index = from_(0)
tt("index() == 0", "index() == 1", "index() == 2")


def to_(gen, limit):
    def _internal():
        nonlocal gen
        if gen:
            val = gen()
            if limit > val:
                return val
            else:
                gen = None
                return None

    return _internal

index = to_(from_(1), 3)
tt("index() == 1", "index() == 2", "index() is None")


def fromTo(initial, limit):
    return to_(from_(initial), limit)

index = fromTo(0, 3)
tt("index() == 0", "index() == 1", "index() == 2", "index() is None")


def element(arr, gen):
    def _internal():

        ix = gen()
        #   We need to check for length to avoid errors if the generator returns
        # indices past the end of the array
        return (
            arr[ix] if (ix is not None) and len(arr) >= ix else None
        )

    return _internal

ele = element(["a", "b", "c", "d"], fromTo(1, 3))
tt("ele() == 'b'", "ele() == 'c'", "ele() is None")


def element(arr, gen=None):
    if gen is None:
        gen = fromTo(0, len(arr))

    def _internal():
        ix = gen()
        return arr[ix] if (ix is not None) and len(arr) >= ix else None  # See above

    return _internal

ele = element(["a", "b", "c", "d"])
tt("ele() == 'a'", "ele() == 'b'", "ele() == 'c'", "ele() == 'd'", "ele() is None")


def collect(gen, arr):
    def _internal():
        val = gen()
        if val is not None:  # 0 is falsy, so we need the None check
            arr.append(val)
            return val

    return _internal

ARRAY = []
col = collect(fromTo(0, 2), ARRAY)

tt("col() == 0", "col() == 1", "col() is None", "ARRAY == [0, 1]")


def filter(gen, pred):
    def _internal():  # Crockford has us do a do loop here instead
        val = gen()
        if val is None or pred(val):
            return val
        return _internal()

    return _internal

fil = filter(fromTo(0, 5), lambda v: (v % 3) == 0)
tt("fil() == 0", "fil() == 3", "fil() is None")


def concat(gena, genb):
    gen = gena

    def _internal():
        nonlocal gen
        val = gen()
        if val is not None:
            return val
        gen = genb
        return gen()

    return _internal

con = concat(fromTo(0, 3), fromTo(0, 2))
tt(
    "con() == 0",
    "con() == 1",
    "con() == 2",
    "con() == 0",
    "con() == 1",
    "con() is None",
)


def gensymf(sym):
    gen = from_(1)
    return lambda: f"{sym}{gen()}"

geng = gensymf("G")
genh = gensymf("H")
tt("geng() == 'G1'", "genh() == 'H1'", "geng() == 'G2'", "genh() == 'H2'")


def fibonnacif(a, b):
    def _internal():
        # Crockford starts us off with a switch statement.
        # But ended up using this solution with element instead of fromTo in
        # the concat call below.
        nonlocal a, b
        n = a + b
        a = b
        b = n
        return n

    return concat(fromTo(a, b + 1), _internal)

fib = fibonnacif(0, 1)
tt(
    "fib() == 0",
    "fib() == 1",
    "fib() == 1",
    "fib() == 2",
    "fib() == 3",
    "fib() == 5",
    "fib() == 8",
)

##############################
# Ok, so Python doesn't have an object literal syntax in the same
# sense as Javascript. This is a piece of utility for
# compatibility purposes.

from collections import namedtuple

def Obj(**kwargs):
    return namedtuple("Object", " ".join(kwargs.keys()))(**kwargs)
##############################


def counter(init):
    def _up():
        nonlocal init
        init += 1
        return init

    def _down():
        nonlocal init
        init -= 1
        return init

    return Obj(up=_up, down=_down)


OBJECT = counter(10)
up = OBJECT.up
down = OBJECT.down
tt("up() == 11", "down() == 10", "down() == 9", "up() == 10")


def revocable(fn):
    f = fn

    def revoke():
        nonlocal f
        f = None

    return Obj(invoke=lambda *args: f(*args) if f else None, revoke=revoke)


rev = revocable(add)
tt("rev.invoke(3, 4) == 7", "rev.revoke() is None", "rev.invoke(3, 4) is None")


def m(value, source=None):
    return Obj(value=value, source=source if source is not None else str(value))

import math
tt(
    "m(1).value == 1",
    "m(1).source == '1'",
    "m(math.pi, 'Pi').value == math.pi",
    "m(math.pi, 'Pi').source == 'Pi'",
)


def addm(a, b):
    return m(add(a.value, b.value), f"({a.source}+{b.source})")

tt(
    "addm(m(3), m(4)).value == 7",
    "addm(m(3), m(4)).source == '(3+4)'",
    "addm(m(1), m(math.pi, 'pi')).source == '(1+pi)'",
    "addm(m(1), m(math.pi, 'pi')).value == (1+math.pi)",
)


def liftm(fn, label):
    return lambda *args: m(
        fn(*[a.value for a in args]), f"({label.join([a.source for a in args])})"
    )

addm = liftm(add, "+")
tt(
    "addm(m(3), m(4)).value == 7",
    "addm(m(3), m(4)).source == '(3+4)'",
    "addm(m(1), m(math.pi, 'pi')).source == '(1+pi)'",
    "addm(m(1), m(math.pi, 'pi')).value == (1+math.pi)",
)


def liftm(fn, label):
    # I did a different thing than Crockford here.
    # He coerces incoming arguments into ms, I just have a getter/setter
    # that self-evaluateish if we get passed something that doesn't
    # have a .value or .source
    def val(obj):
        try:
            return obj.value
        except AttributeError:
            return obj

    def src(obj):
        try:
            return obj.source
        except AttributeError:
            return str(obj)

    return lambda *args: m(
        fn(*[val(a) for a in args]), f"({label.join([src(a) for a in args])})"
    )

addm = liftm(add, "+")
tt(
    "addm(m(3), m(4)).value == 7",
    "addm(m(3), m(4)).source == '(3+4)'",
    "addm(m(1), m(math.pi, 'pi')).source == '(1+pi)'",
    "addm(m(1), m(math.pi, 'pi')).value == (1+math.pi)",
    "addm(3, 4).value == 7",
    "addm(3, 4).source == '(3+4)'",
)


def exp(sae):
    if type(sae) is list and callable(sae[0]):
        return sae[0](*sae[1:])
    else:
        return sae

tt("exp([mul, 5, 11]) == 55", "exp(42) == 42")


def exp(nae):
    if type(nae) is list and callable(nae[0]):
        return nae[0](*[exp(arg) for arg in nae[1:]])
    else:
        return nae

nae = [math.sqrt, [add, [square, 3], [square, 4]]]
tt("exp(nae) == 5")


def addg(a=None):
    if a is None:
        return None
    else:
        total = a

        def recur(b=None):
            nonlocal total
            if b is None:
                return total
            else:
                total += b
                return recur
        # This is the first time I heard of the term "retursion",
        # and I'm not sure how I feel about it.

        return recur

tt(
    "addg() is None",
    "addg(2)() == 2",
    "addg(2)(7)() == 9",
    "addg(3)(0)(4)() == 7",
    "addg(1)(2)(4)(8)() == 15",
)


def liftg(fn):
    def fng(a=None):
        if a is None:
            return None
        else:
            acc = a

            def recur(b=None):
                nonlocal acc
                if b is None:
                    return acc
                else:
                    acc = fn(acc, b)
                    return recur

            return recur

    return fng

addg = liftg(add)
tt(
    "addg() is None",
    "addg(2)() == 2",
    "addg(2)(7)() == 9",
    "addg(3)(0)(4)() == 7",
    "addg(1)(2)(4)(8)() == 15",
    "liftg(mul)() is None",
    "liftg(mul)(3)() == 3",
    "liftg(mul)(3)(0)(4)() == 0",
    "liftg(mul)(1)(2)(4)(8)() == 64",
)


def arrayg(a=None):
    if a is None:
        return []
    else:
        return liftg(lambda memo, elem: memo + [elem])([a])  # I got extra credit :p

tt("arrayg() == []", "arrayg(3)() == [3]", "arrayg(3)(4)(5)() == [3, 4, 5]")


def continuize(fn):
    return lambda callback, *args: callback(fn(*args))

tt(
    "continuize(math.sqrt)(identity, 81) == 9",
    "continuize(math.sqrt)(lambda v: str(int(v)), 81) == '9'",
)

def vector():
    vec = []
    def store(ix, elem):
        vec[ix] = elem
    return Obj(append = lambda elem: vec.append(elem),
               store = store,
               get = lambda ix: vec[ix])

#   There's a long discussion regarding the security of the above object.
# Basically, the question is: given this definition of `vector`, is there
# a way that an attacker given the result of `vector()` can get out a
# pointer to `vec`.
#   In Javascript, it turns out there's an attack that exploits a bunch of
# differnet flaws/misfeatures of JavaScript ot accomplish this.
#   1. the fact that arrays in JS aren't really arrays (you can assign things
#      to their method slots through the same interface as you assign things
#      to their storage slots)
#   2. the semantics of the `this` keyword
# python has neither of these vulnerabilities, so you don't have to take the
# extra step of coercing the first input to `store` into an `int`. You may,
# but you don't have to in order to maintain security in the sense
# Crockford is interested in here.
#   That said, I don't know enough about pythons' implementation to claim that
# you automatically get the desired level of security here. There might be ways
# of exploiting `locals` or `globals`, or some implementation detail of my Obj
# function in order to get at a pointer to vec directly.

def pubsub():
    subscribers = []
    def pub(msg):
        for s in subscribers:
            try:
                s(msg)
            except Exception:
                pass
    return Obj(subscribe=lambda fn: subscribers.append(fn),
               publish=pub)

#   This is almost as naive as the first attempt we get shown for debugging in
# Crockford's video. It wraps the call to the subscriber function with a
# `try`/`catch`. There's still attacks from subscribers we're vulnerable to.
# The big one being sleeping forever.
#   Another attack that Crockford mentions (having an adversary mutate pubsub
# so that the `publish` function does something else) also fails here, because
# our Obj function effectively automatically `freeze`s its' output. That is,
# the thing you get out of `namedtuple` is an immutable object.
#   Yet another attack involves `this`, which we thankfully don't have in python,
# and so we can safely disregard it.
#   The last attack he describes is an out-of order attack. Where you subscribe with something that calls `publish` once itself. This way, you can deliver a message of your choice to all subscribers who came after you before they get the message that you get.

def pubsub():
    import multiprocessing, logging, sys
    from queue import SimpleQueue

    logger = logging.getLogger("PUBSUB")
    logger.setLevel(logging.WARN)
    h = logging.StreamHandler(sys.stdout)
    logger.addHandler(h)

    subscribers = multiprocessing.Queue()
    messages = multiprocessing.Queue()
    def proc():
        logger.debug("Starting proc")
        while True:
            msg = messages.get()
            logger.info(f"Got message <<{msg}>>...")
            logger.debug(f"Going through subscribers {subscribers}")
            for _ in range(subscribers.qsize()):
                s = subscribers.get()
                logger.debug(f"  Delivering to {s}")
                try:
                    p = multiprocessing.Process(target=s, args=(msg,))
                    p.start()
                    logger.debug("  Started process...")
                    p.join(10)
                    logger.debug("  Joined...")
                    if p.is_alive():
                        p.terminate()
                        p.join()
                        logger.warn("  Forced to terminate :( ...")
                    subscribers.put(s)
                except Exception:
                    logger.warn("  Subscriber failed in some way...")
                    pass
    runner = multiprocessing.Process(target=proc, args=())
    def stop(delay):
        runner.join(delay)
        if runner.is_alive():
            runner.terminate()
            runner.join()
    runner.start()
    return Obj(subscribe=lambda fn: subscribers.put(fn),
               publish=lambda msg: messages.put(msg),
               stop=stop)

#   That is... pretty horrifically complicated. Especially compared to
# Crockfords' proposed JS solution, which is to use `setTimeout` to delay
# things a bit. It's unfortunate, because that approach opens up another attack,
# but is ridiculously simple.
#   At the beginning of this talk, I thought we'd stick mostly to functional
# concepts. But now that we've broken through into asynchronous functions,
# multiprocessing, and (based on the motivation bits of the talk) distributed
# computing, I'm very tempted to try this exercise again in a Lisp or ML
# at some point.
#   As always, I'll keep you posted.

tests.report()
```
