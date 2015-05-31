I'll get the interesting tech stuff out of the way first, then write about one thing I don't want to miss, and a personal journal entry. Also, I'll probably be a little scarce around here over the next while. The why of it will hopefully be obvious when I get to it.

## Pipes and Multi-Line REPLs

Before we begin, be aware that I'm not trying to teach you about Pipes. I'm just showing you a use case. If you want the full tutorial and explanation, go check out [this](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Tutorial.html) and possibly [this](https://hackage.haskell.org/package/pipes-concurrency-2.0.1/docs/Pipes-Concurrent-Tutorial.html). The documentation attached to this project is anomalously understandable.

Now then.

I've had a working Haskell-based interpreter for experimentaLISP for a while now. And, in fact, a web-based version of the same, so that you can take the minimal thing there out for a spin without bothering with installing new runtimes or compilers. The web-repl neatly side-stepped this problem, but the command-line one never did.

```haskell
*Main Data.IORef> main
experimentaLISP v0.00001
EXP>> (+ 2 3)
5
EXP>> (+ 2
READ ERROR
EXP>> 
```

Even worse, if I pasted multi-line input from another buffer:

```
;; external buffer "foo"
(def map 
  (fn (f lst) 
    (if (null? lst)
      () 
      (cons (f (car lst)) 
            (map f (cdr lst))))))
```


```
-- the GHCi REPL
EXP>> (def map 
  (fn (f lst) 
    (if (null? lst)
      () 
      (cons (f (car lst)) 
            (map f (cdr lst))))))
READ ERROR
EXP>> READ ERROR
EXP>> READ ERROR
EXP>> NIL
EXP>> READ ERROR
EXP>> *** Exception: Undefined function 'map'
*Main Data.IORef> 
```

See, my initial REPL was naive.

```haskell
module Main where

import Model
import Reader
import Evaluator

import System.IO

main_loop :: Environment -> IO ()
main_loop env = do putStr "EXP>> "
                   hFlush stdout
                   ln &lt;- getLine
                   case lisp_read ln of
                     Right res -> let evaled = eval res env
                                      env' = case evaled of
                                               (Res _) -> env
                                               (Mod _ e) -> e
                                  in do putStrLn . show $ res_of evaled
                                        main_loop env'
                     _ -> do putStrLn "READ ERROR"
                             main_loop env

main :: IO ()
main = do putStrLn $ "experimentaLISP v0.00001"
          main_loop global_env
```

The Racket one accidentally allows multi-line input because the function I'm using to get it, the built-in `read`, is expression based. GHCi is not. And the Haskell `getLine` has the predictable effect of getting a single line. And since it's unlikely that every piece of a particular expression split across multiple lines would parse on its own, that's a problem.

The first solution I thought of was non-blocking reads from `stdin`, with evaluation triggering when input ran out. After a quick read of `System.IO`, it's not immediately obvious how I'd go about doing that.

The second solution involved pipes, and gave me precisely the effect I wanted:

```
*Main Data.IORef> main
experimentaLISP v0.00001

EXP>> (+
2
3)
5

EXP>> (def map 
  (fn (f lst) 
    (if (null? lst)
      () 
      (cons (f (car lst)) 
            (map f (cdr lst))))))
NIL

EXP>> map
&lt;fn (f lst)>

EXP>> 
```

I can now spread valid expressions across as many lines as I like. Which includes

```
EXP>> "trivial
multi
line
strings"
"trivial\nmulti\nline\nstrings"

EXP>> 
```

So, here's how.

```haskell
module Main where

import Model
import Reader
import Evaluator

import Pipes
import Control.Monad (unless)
import System.IO
import Data.Char (isSpace)

main :: IO ()
main = do putStrLn $ "experimentaLISP v0.00001"
          runEffect $ stdinLn >-> reader >-> evaluator >-> prompt

stdinLn :: Producer String IO ()
stdinLn = do
  eof &lt;- lift isEOF
  unless eof $ do
            str &lt;- lift getLine
            yield str
            stdinLn

reader :: (Monad m) => Pipe String LispVal m ()
reader = loop []
    where loop acc = do 
            ln &lt;- await
            case strip ln of
              ":c" -> loop []
              _ -> case lisp_read . unlines $ reverse (ln:acc) of
                     Right res -> do yield res
                                     loop []
                     Left _ -> loop $ ln : acc

evaluator :: (Monad m) => Pipe LispVal LispVal m ()
evaluator = loop global_env
    where loop env = do 
            exp &lt;- await
            let evaled = eval exp env
                env' = case evaled of
                         (Res _) -> env
                         (Mod _ e) -> e
            do yield $ res_of evaled
               loop env'

prompt :: Show a => Consumer a IO ()
prompt = do lift $ putStr "\nEXP>> "
            lift $ hFlush stdout
            msg &lt;- await
            lift $ putStrLn $ show msg
            prompt

strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace
```

Aside from the usual program composition problem of requiring `import`s to preceed the program, you can pretty much read this one top to bottom.

```haskell
main :: IO ()
main = do putStrLn $ "experimentaLISP v0.00001"
          runEffect $ stdinLn >-> reader >-> evaluator >-> prompt
```

We're going to write a little intro message on which our version number will be proudly displayed, then we're going to go run a `Pipe`. The pipe will consist of four stations, `stdinLn`, `reader`, `evaluator` and `prompt`. The `stdinLn` station is going to get input from the user a line at a time, which should surprise precisely no one.

```haskell
stdinLn :: Producer String IO ()
stdinLn = do
  eof &lt;- lift isEOF
  unless eof $ do
            str &lt;- lift getLine
            yield str
            stdinLn
```

If we `EOF`, we're done. Otherwise, get a line, `yield` it, and loop. The station collecting those `yield`ed strings is the `reader`

```haskell
reader :: (Monad m) => Pipe String LispVal m ()
reader = loop []
    where loop acc = do 
            ln &lt;- await
            case strip ln of
              ":c" -> loop []
              _ -> case lisp_read . unlines $ reverse (ln:acc) of
                     Right res -> do yield res
                                     loop []
                     Left _ -> loop $ ln : acc
```

It gets a line, checks whether it's a special interpreter directive. If not, it tries to read the entirety of its accumulator, including this new line. If that worked, it `yield`s the result and clears its accumulator. Otherwise, it accumulates this new line. If the incoming line *is* an interpreter directive, it handles that directive. Though, at the moment, the only such directive is `:c`, which clears the current accumulator. Lines successfully read by the `reader` are passed to the `evaluator`

```haskell
evaluator :: (Monad m) => Pipe LispVal LispVal m ()
evaluator = loop global_env
    where loop env = do 
            exp &lt;- await
            let evaled = eval exp env
                env' = case evaled of
                         (Res _) -> env
                         (Mod _ e) -> e
            do yield $ res_of evaled
               loop env'
```

Which evaluates each incoming expression and yields the result. If evaluating that expression changes the environment, it also keeps those changes going forward. Finally, those evaluated expressions feed forward into `prompt`

```haskell
prompt :: Show a => Consumer a IO ()
prompt = do lift $ putStr "\nEXP>> "
            lift $ hFlush stdout
            msg &lt;- await
            lift $ putStrLn $ show msg
            prompt
```

Which prints a prompt of "EXP>> ", flushes `stdout` so that the prompt is actually displayed, awaits an expression, and prints it when it arrives.

This is a pretty useful use for `Pipe`s, but I get the feeling that the interesting stuff really happens over in [`Pipes.Concurrent`](https://hackage.haskell.org/package/pipes-concurrency-2.0.1/docs/Pipes-Concurrent-Tutorial.html). Where you can fork pipelines off into multiple directions, then merge disparate outputs up again later on. I can think of one or two uses to put that to.

## The Toronto Computer Science Reading Group

Also known as the [Comp-Sci Cabal](https://github.com/CompSciCabal) just finished up reading through [SICP](https://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs). And we made ourselves these

![](/static/img/cabal-badge.png)

Then we had a ceremony wherein we handed them out to the four of us who finished the book and a large percentage of the exercises.

![](/static/img/cabal-ceremony.jpg)

As a side-note, only 10 of the pins remain unclaimed, so if you want one, read SICP, do one of the last five exercises in the book, then find me in meatspace before they run out. Post-pinning, we sat down and reflected on what we had learned late into the night. Ok, it was only 'till around 10 or so, but we're old, and some of us had to travel. I loved every minute of it. It was, hands down, the highlight of the last half-year or so for me.

## Journal

So I'm about to have another son. The second of two.  We've picked out a name, but aren't telling anyone 'till he's here. *You* won't get to know in any case, unless you know me in real life. I'm mildly worried, which is only natural, I guess. It was more or less the same thing last time. I'm hoping both my wife and my new offspring make it through ok. And I'm hoping I do well raising him. Which is going to be hard, given that I don't have a clear metric for what it means to "do well" in this context. I think it has something to do with giving him the tools to interact well with human societies, but again "well" is ill defined. I think *that* has something to do with making sure he learns to contribute in a positive-sum way. It's something I'll have cause to think about carefully in the next little while, in between helping my wife with the recovery, looking after my existing heir, and maybe getting some day-job work done in the meantime.

There's a large number of branching futures ahead of me is what I'm trying to say here. And most of them have me thinking, no offense, about more important matters than the construction of functionally pure interpreters, or exploring the possibility space of LISP dialects. So, keep an eye out for updates, or have your [rss reader do it for you](/feed/atom), but don't hold your breath.

This is probably me signing off for a relatively long while.
