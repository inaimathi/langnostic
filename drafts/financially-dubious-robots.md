So I have a [Wealthsimple](https://www.wealthsimple.com/) account. And they have a thing called [Wealthsimple Trade](https://www.wealthsimple.com/en-ca/product/trade/), which is a direct stock trading platform. Well, "direct" in the sense that they let you pick the stocks you're trading instead of managing them for you. You still don't go exactly to a particular stock market. Anyway, part of their "stock" offering includes [Bitcoin and Ethereum](https://www.wealthsimple.com/en-ca/product/crypto/), and their fees are much more reasonable than [`coinbase`](https://www.coinbase.com/).

They also have a [not-particularly-well-documented API](https://github.com/MarkGalloway/wealthsimple-trade).

And because the title of this piece is "Financially Dubious Robots", you know exactly where this is going. I'm working in Python this time, because I _don't_ expect these robots to be complex enough that I actually need [macros](https://common-lisp.net/) or [monads](https://www.haskell.org/) to express them. But hey, we'll see what the future holds.

Ok, to start with, here's a simple wrapper around the API.

```
class WealthsimpleApi:
    def __init__(self, email):
        if keyring.get_password("wealthsimple", email) is None:
            password = getpass.getpass(prompt="Password: ")
            keyring.set_password("wealthsimple", email, password)
        else:
            password = keyring.get_password("wealthsimple", email)
        self.email = email
        self.jar = requests.cookies.RequestsCookieJar()
        self._tokens_from(self.login(password))

    def _tokens_from(self, res):
        self.refreshed_at = time.time()
        self.access_token = res.headers["X-Access-Token"]
        self.refresh_token = res.headers["X-Refresh-Token"]
        return True

    def _req(self, method, url, data=None):
        headers = {"Authorization": self.access_token}
        if data is None:
            res = method(url, cookies=self.jar, headers=headers)
        else:
            res = method(url, cookies=self.jar, headers=headers, data=data)
        if res.status_code == 200:
            if res.content == b"OK":  # We're looking at a refresh here
                return res
            try:
                return res.json()
            except Exception:
                return res
        return res

    def _get(self, path):
        return self._req(requests.get, f"https://trade-service.wealthsimple.com/{path}")

    def _post(self, path, data):
        return self._req(
            requests.post, f"https://trade-service.wealthsimple.com/{path}", data
        )

    def login(self, password):
        otp = getpass.getpass(prompt="2FA Code: ")
        return requests.post(
            "auth/login",
            {"email": self.email, "password": password, "otp": otp},
            cookies=self.jar,
        )

    def refresh(self, force=False):
        if force or (time.time() - self.refreshed_at > (60 * 10)):
            res = self._post("auth/refresh", {"refresh_token": self.refresh_token})
            return self._tokens_from(res)
        return False

    def accounts(self):
        self.refresh()
        res = self._get("account/list")
        if type(res) is "dict":
            return res["results"]
        return res

    def orders(self):
        self.refresh()
        return self._get("orders")

    def place_order(self, account_id, security_id, quantity, order_type, dry_run=False):
        order = {
            "account_id": account_id,
            "security_id": security_id,
            "quantity": quantity,
            "order_type": order_type,
            "order_sub_type": "market",
            "time_in_force": "day",
        }
        if dry_run:
            return order
        self.refresh()
        return self._post("orders", order)

    def buy(self, account_id, security_id, quantity=None, value=None, dry_run=False):
        price = self.security(security_id)["quote"]["amount"]
        q, _ = _quant(quantity, value, price)
        return self.place_order(
            account_id, security_id, q, "buy_quantity", dry_run=dry_run
        )

    def sell(self, account_id, security_id, quantity=None, value=None, dry_run=False):
        price = self.security(security_id)["quote"]["amount"]
        q, _ = _quant(quantity, value, price)
        return self.place_order(
            account_id, security_id, quantity, "sell_quantity", dry_run=dry_run
        )

    def activity(self):
        self.refresh()
        return self._get("account/activities")

    def me(self):
        self.refresh()
        return self._get("me")

    def forex(self):
        self.refresh()
        return self._get("forex")

    def security(self, security_id):
        self.refresh()
        return self._get(f"securities/{security_id}")
```

It's entirely straightforward and does nothing more than apply credentials you give it to the exposed API and let you make calls to it. It uses `keyring` to remember your password and makes calls to `getpass` to get your password and 2FA token, and it periodically refreshes your access token on every request you make. Yay.

Ok, now, I'm not planning on being a day trader here. I specifically want to deal with their cryptocurrency offering, so I'll want to specialize the above somewhat.

```
class Crypto:
    def __init__(self, email):
        self.API = WealthsimpleApi(email)
        res = self.API.accounts()["results"][0]
        self.ID = res["id"]
        self.CUSTODIAN = res["custodian_account_number"]

    def buy(self, security_id, quantity=None, value=None, dry_run=False):
        return self.API.buy(self.ID, security_id, quantity, value, dry_run)

    def sell(self, security_id, quantity=None, value=None, dry_run=False):
        return self.API.buy(self.ID, security_id, quantity, value, dry_run)

    def quote(self, sec_id):
        security = self.API.security(sec_id)
        return {
            "id": security["id"],
            "symbol": security["stock"]["symbol"],
            "name": security["stock"]["symbol"],
            "quote": {k: security["quote"][k] for k in ["amount", "ask", "bid"]},
            "date": security["quote"]["quote_date"],
        }

    def quotes(self, security_ids):
        return [self.quote(s) for s in security_ids]

    def summary(self):
        res = self.API.accounts()["results"][0]

        return {
            "balance": res["current_balance"]["amount"],
            "available": res["available_to_withdraw"]["amount"],
            "withdrawn": res["withdrawn_earnings"]["amount"],
            "positions": {
                k: _pos(self, k, v) for k, v in res["position_quantities"].items()
            },
        }

    def run(self, robot, frequency=60, dry_run=True):
        print(f"Starting robot {robot}...")
        print(f"  run every {frequency} seconds")
        if dry_run:
            print("  NO ACTUAL TRANSACTIONS")
        try:
            while True:
                print(".", end="")
                robot(self, dry_run=dry_run)
                time.sleep(frequency)
        except Exception:
            return self
```

So this applies some simplifying views to the information that comes out of the API endpoints and hands you some specialized hooks specifically to make it easier to buy/sell crypto.

Oh, and lets you run robots with it.

The `run` method specifically takes a `robot` (a function of `API -> Bool -> IO()`), a `frequency` and a `dry_run` option, and goes about doing the obvious thing.

## Reading Robot

Here's a simple robot factory.

```
def mk_monitor(path="~/.pytrade/history.json"):
    p = os.path.expanduser(path)

    def _bot(api, dry_run=False):
        with open(p, "a") as f:
            f.write(json.dumps(api.quotes([BTC, ETH])))
            f.write("\n")

    return _bot
```

You call it, optionally with a path, and it returns a function that uses the given `api` to dump Bitcoin and Ethereum data to disk. Oh, right, we also need those constants.

```
BTC = "sec-z-btc-4ca670cac10139ce8678b84836231606"
ETH = "sec-z-eth-dc40261c82a191b11e53426aa25d91af"
```

These are Wealthsimple-assigned IDs for those cryptocurrencies as "securities".

Here's how we run it:

```
>>> c = Crypto("my@email.com")
Password: hunter2
2FA Code: 696420
>>> c.run(mk_monitor())
Starting robot <function mk_monitor.<locals>._bot at 0x7fb101b35048>...
  run every 60 seconds
  NO ACTUAL TRANSACTIONS
..........................................................
```

And it'll now go off and gather data. This robot does nothing to your account balance, so we're not quite at Financially Dubious yet.

## Robots

Here's the simplest possible writing robot.

```
def mk_hold_bot():
    def _bot(api, dry_run=False):
        summary = api.summary()
        if summary["available"] > 0:
            api.buy(BTC, value=summary["available"])

    return _bot
```

It buys bitcoin with its' full balance and holds it forever. Not very interesting, but certainly dubious. Here's another one.

```
def mk_lohi_bot(security_id, lo, hi):
    def _bot(api, dry_run=False):
        quote = api.quote(security_id)
        price = quote["quote"]["amount"]
        summary = api.summary()
        sec = summary["positions"][security_id]
        avail = summary["available"]

        if price >= hi:
            if sec["value"] > 0:
                api.sell(security_id, quantity=sec["quantity"])
        elif lo >= price:
            if avail > 0:
                api.buy(security_id, value=avail)

    return _bot
```

This one takes a pair of target prices, `lo` and `hi`. It buys if the price is `lo` or lower (and it has available balance), and sells if the price is `hi` or higher.

```
def mk_wsb_bot(twitter_auth, classifier, tokenize):
    client = tweepy.API(twitter_auth)
    TSLA = "sec-s-50cdacc9811f407c8dff52e15be08582"

    def _bot(api, dry_run=True):
        avail = api.summary["available"]
        latest = client.user_timeline("@elonmusk", count=1)[0]
        if "Positive" == classifier.classify(dict([token, True] for token in tokenize(latest))):
            api.buy(TSLA)
        else:
            api.sell(TSLA)
```

This one buys or sells Tesla stock based on how Elon's feeling on Twitter. I'll be here all week folks, try the veal.

In all seriousness though, even if we weren't doing batshit insane things like that last one, we'd want to test these things before handing them money and standing back. So we need a testing harness, and some historical (or possibly randomly generated) data to check our results against.

## Testing Robots

```
class Dummy:
    def __init__(self, history_file, starting_balance):
        self.__history = list(util.json_lines(history_file))
        self.__history.reverse()
        self.__state = self.__history.pop()
        self.__summary = {
            "balance": starting_balance,
            "available": starting_balance,
            "withdrawn": 0,
            "positions": {BTC: 0, ETH: 0},
        }

    def buy(self, security_id, quantity=None, value=None, dry_run=False):
        price = self.quote(security_id)["quote"]["amount"]
        q, v = _quant(quantity, value, price)
        self.__summary["balance"] -= v
        self.__summary["available"] -= v
        if security_id not in self.__summary["positions"]:
            self.__summary["positions"][security_id] = 0
        real_q, real_v = _quant(quantity, v, price)
        print(f"\nBUYING {q}({v})[{real_q}{real_v}] at ${price}")
        self.__summary["positions"][security_id] += real_q
        return True

    def sell(self, security_id, quantity=None, value=None, dry_run=False):
        price = self.quote(security_id)["quote"]["amount"]
        q, v = _quant(quantity, value, price)
        print(f"\nSELLING {q}({v}) at ${price}")
        self.__summary["positions"][security_id] -= q
        self.__summary["balance"] += v
        self.__summary["available"] += v
        return True

    def quote(self, security_id):
        if security_id == BTC:
            return self.__state[0]
        elif security_id == ETH:
            return self.__state[1]

    def quotes(self, security_ids):
        return self.__state

    def __tick(self):
        self.__state = self.__history.pop()

    def summary(self):
        s = self.__summary.copy()
        s["positions"] = {k: _pos(self, k, v) for k, v in s["positions"].items()}
        return s

    def run(self, robot):
        print(f"Starting Dummy robot {robot}...")
        while self.__history:
            print(".", end="")
            robot(self)
            self.__tick()
        print("")
        return self
```

This is a dummy framework that exposes _pretty much_ the same interface as the earlier `Crypto` class, but pokes internal state instead of going out to the Wealthsimple servers. It lets you test drive robots if you have historic data[^like-the-stuff]. So, lets do that.

[^like-the-stuff]: Like the stuff we collected with our `monitor` robot earlier. In theory if you got historic by-the-minute data from somewhere else, you could do the same thing with it, but in practice, the only sources I've found online charge for that data and I'm not _that_ bored. As of this writing, you can find _daily_ data [here](https://www.coindesk.com/price/bitcoin), but that's not as useful for our purposes.

```
Python 3.7.0 (default, Sep 14 2018, 10:32:11)
[GCC 7.3.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> python.el: native completion setup loaded
>>> import app
>>> import api
>>> import util
>>> api.Dummy("~/.pytrade/history.json", 500).run(app.mk_lohi_bot(util.BTC, 100, 24500, 25000)).summary()
Starting Dummy robot <function mk_lohi_bot.<locals>._bot at 0x7f9a10f678c8>...
........
BUYING 0.02041566(500)[0.02041566500] at $24491
...
SELLING 0.02041566(516.22017093) at $25285.5
...<snip>...
BUYING 0.02112453(516.22017093)[0.02112453516.22017093] at $24437
...<snip>...
SELLING 0.02112453(529.011042525) at $25042.5
...<snip>...
BUYING 0.02163998(529.011042525)[0.02163998529.011042525] at $24446
...<snip>...
{'balance': 0.0, 'available': 0.0, 'withdrawn': 0, 'positions': {'sec-z-btc-4ca670cac10139ce8678b84836231606': {'quantity': 0.02163998, 'value': 505.76961256, 'price': 23372}, 'sec-z-eth-dc40261c82a191b11e53426aa25d91af': {'quantity': 0, 'value': 0.0, 'price': 707.77}}}
>>> api.Dummy("~/.pytrade/history.json", 500).run(app.mk_lohi_bot(util.BTC, 100, 24000, 25000)).summary()
Starting Dummy robot <function mk_lohi_bot.<locals>._bot at 0x7f9a10f678c8>...
...<snip>...
BUYING 0.0210726(500)[0.0210726500] at $23727.5
...<snip>...
SELLING 0.0210726(532.8312273) at $25285.5
...<snip>...
BUYING 0.02229093(532.8312273)[0.02229093532.8312273] at $23903.5
...<snip>...
SELLING 0.02229093(558.220614525) at $25042.5
...<snip>...
BUYING 0.02330143(558.220614525)[0.02330143558.220614525] at $23956.5
...<snip>...
{'balance': 0.0, 'available': 0.0, 'withdrawn': 0, 'positions': {'sec-z-btc-4ca670cac10139ce8678b84836231606': {'quantity': 0.02330143, 'value': 544.60102196, 'price': 23372}, 'sec-z-eth-dc40261c82a191b11e53426aa25d91af': {'quantity': 0, 'value': 0.0, 'price': 707.77}}}
>>> api.Dummy("~/.pytrade/history.json", 500).run(app.mk_lohi_bot(util.BTC, 100, 24000, 25500)).summary()
Starting Dummy robot <function mk_lohi_bot.<locals>._bot at 0x7f9a10f678c8>...
...<snip>...
BUYING 0.0210726(500)[0.0210726500] at $23727.5
...<snip>...
SELLING 0.0210726(537.6147075) at $25512.5
...<snip>...
BUYING 0.02249105(537.6147075)[0.02249105537.6147075] at $23903.5
...<snip>...
{'balance': 0.0, 'available': 0.0, 'withdrawn': 0, 'positions': {'sec-z-btc-4ca670cac10139ce8678b84836231606': {'quantity': 0.02249105, 'value': 525.6608206, 'price': 23372}, 'sec-z-eth-dc40261c82a191b11e53426aa25d91af': {'quantity': 0, 'value': 0.0, 'price': 707.77}}}
>>>
```

That's a slightly truncated test run of the `lohi` bot against replayed, recent-ish data[^as-of-writing]. We're leaving something out though; Wealthsimple charges a percentage on each transaction, which our `Dummy` class doesn't account for. So, while the simulations _are_ currently showing a fairly healthy profit given the amount of work we've put in, I get the feeling it won't quite be that simple.

[^as-of-writing]: As of this writing, BTC has hit a massive jump up to a new all-time high and doesn't seem to be slowing down today, but I'm not bothering to record that data ¯\_(ツ)_/¯.

```
...
def _taxed(amt, rate):
    return round(amt - (amt * rate), 2)
...

class Dummy:
    ...
    def buy(self, security_id, quantity=None, value=None, dry_run=False):
        ...
        real_q, real_v = _quant(quantity, _taxed(v, 0.0148), price)
		...

    def sell(self, security_id, quantity=None, value=None, dry_run=False):
		...
        print(f"\nSELLING {q}({v})[{_taxed(v, 0.0152)}] at ${price}")
		...
        self.__summary["balance"] += _taxed(v, 0.0152)
        self.__summary["available"] += _taxed(v, 0.0152)
		...

	# Also, just for ease of use:
	def liquidate(self):
		print("LIQUIDATING")
        for k, sec in self.summary()["positions"].items():
            if sec["quantity"]:
                self.sell(k, quantity=sec["quantity"])
        return self
```

Now then...

```
>>> import importlib
>>> importlib.reload(api)
<module 'api' from '/home/inaimathi/projects/pytrade/api.py'>
>>> api.Dummy("~/.pytrade/history.json", 500).run(app.mk_lohi_bot(util.BTC, 100, 24000, 25000)).liquidate().summary()
Starting Dummy robot <function mk_lohi_bot.<locals>._bot at 0x7f9a10f19b70>...
...<snip>...
BUYING 0.0210726(500)[0.02076072492.6] at $23727.5
...<snip>...
SELLING 0.02076072(524.94518556)[516.97] at $25285.5
...<snip>...
BUYING 0.02162738(516.97)[0.02130734509.32] at $23903.5
...<snip>...
SELLING 0.02130734(533.58906195)[525.48] at $25042.5
...<snip>...
BUYING 0.02193476(525.48)[0.02161517.7] at $23956.5
...<snip>...
LIQUIDATING

SELLING 0.02161(505.06892)[497.39] at $23372
{'balance': 497.39, 'available': 497.39, 'withdrawn': 0, 'positions': {'sec-z-btc-4ca670cac10139ce8678b84836231606': {'quantity': 0.0, 'value': 0.0, 'price': 23372}, 'sec-z-eth-dc40261c82a191b11e53426aa25d91af': {'quantity': 0, 'value': 0.0, 'price': 707.77}}}
>>>
```

Woo! We managed to only lose $2.61! Now that's some financially dubious robotics!

## Epilogue

There's almost certainly a way to do better here. Especially given a "security" as volatile as BTC, I'd be quite surprised to hear that you _couldn't_ find enough peaks/valleys over the course of a couple weeks to wring beer money out of a couple hundred bucks. This absolutely _isn't_ something I'd recommend doing with any serious amount of money, or seriously at all. It's just a toy example which I hereby release under the MIT license, which usefully includes no warranty of any kind.

I'm going to leave wringing profit out of this process as well as any general use against the live API, without implying any warranties, as an exercise for the reader.
