I am discussing CLSQL this week. If you don't want to hear about it, here's [a picture of two llamas instead](http://s521.photobucket.com/albums/w334/smackentosh/?action=view&current=1275822438839.jpg&mediafilter=images).

So I've decided to switch over to [CLSQL](http://clsql.b9.com/) from [cl-mysql](http://www.hackinghat.com/index.php/cl-mysql) for my Common Lisp databasing needs. Partly because CLSQL provides a database-agnostic, s-expression based syntax for SQL queries (as opposed to string-representations) , partly because it seems to be closer to a "standard" CL database library, but mainly because it's installable thorough quicklisp, whereas cl-mysql is only installable by downloading the tarball from [its github](https://github.com/hackinghat/cl-mysql) and asdf-install:installing that. Then crossing your fingers that you only get 37 compilation errors.

As usual, here's the experience from the perspective of a not-particularly-bright, young lisper.

Before I even get into using it, though, I have to admit that installation wasn't free of speed bumps. Using (ql:quickload "clsql") seemed to install and include the thing correctly, but as soon as I tried to use connect, it barfed at me, saying that it couldn't compile the C ffi libraries clsql was expecting. This particular machine is running on Debian 6 (the Intel 32 version) and SBCL 1.0.40.0 (this was also before the recent Quicklisp beta update, so it may not even be an issue anymore). Anyway, it turns out that I had to apt-get install cl-sql. libmysqlclient-dev was installed already (zach told me to check in [our brief SO correspondence](http://stackoverflow.com/questions/5032566/clsql-trouble-in-sbcl)), but that didn't seem to make a difference. On my desktop at home, I've got pretty much the same setup, except it's an AMD 64 machine instead of an Intel, and that seemed to trigger a couple of warnings (though following the [Accept] restarts got it into a workable state). Finally, I had one last problem on my Linode (where I hadn't thought to install gcc for some odd reason, so the C ffi libraries had no hope of compiling). That's the installation headaches over with.

TL;DR;so far: if you have any problems, make sure to install libmysqlclient-dev, gcc and cl-sql from the Debian repos. Expect two warnings on AMD machines (which you can [Accept] through).

The actual usage is fairly simple, assuming you're already familiar with SQL. There are two interfaces; a functional one and an OO one that binds tables to CLOS objects. I don't know much about that second one, so this is going to deal with my use of the functional interface.

If you're going to be doing this through the repl, you'll need to evaluate

```lisp
(connect '("localhost" "database-name" "database-user-name" "password") 
         :database-type :mysql) 
(start-sql-recording)
(enable-sql-reader-syntax)
;; I'm using :mysql. You could use something else, it shouldn't matter for the purposes of this article

```

The use of connect is fairly self-explanatory. start-sql-recording returns the SQL equivalent of any cl-sql query you evaluate (so don't use it in files, it's just for repl purposes). Finally, the call to enable-sql-reader-syntax lets you use CLSQLs bracket-delimited SQL macros in the REPL. If you've got a file you want to use CLSQL in (as opposed to at the repl), put (file-enable-sql-reader-syntax) at the top, right (after the in-package statement if you have one). The syntax works in two relevant ways. 

First, it converts lisp-case expressions to SQL_CASE expressions. For example

```lisp
(create-table [users]
              '(([user-id] integer :not-null :unique 
                           :primary-key :auto-increment)
                ([first-name] (string 50)) ([last-name] (string 50)) 
                ([num-logins] integer) ([password] string) ([salt] string)))

>;; 2011-03-03T09:41:44 localhost/database/user => CREATE TABLE USERS (USER_ID INT NOT NULL UNIQUE PRIMARY KEY AUTO_INCREMENT, FIRST_NAME CHAR(50), LAST_NAME CHAR(50), NUM_LOGINS INT(11), PASSWORD VARCHAR(255), SALT VARCHAR(255)) Type=InnoDB

```

If you're using a database other than MySQL, the CREATE TABLE statement will look different (if you want to play around creating stuff, you can find the CLSQL column-type reference about half-way down [this page](http://clsql.b9.com/manual/def-view-class.html)).

Second, it'll give you access to a subset of lisp for the purposes of creating SQL expressions, as in the :where clause here

```lisp
(select [*] 
        :from [user] 
        :where [and [= [first-name] "Inai"] 
                    [= [last-name] "mathi"]])

>;; 2011-03-03T12:41:40 localhost/database/user => SELECT * FROM USER WHERE ((FIRST_NAME = 'Inai') AND (LAST_NAME = 'mathi'))
```

Like I said, it takes a subset of lisp, not the whole thing, so while you can construct pretty elaborate where clauses using (and|or|[=><], you can't do something like 

```lisp
(update-records [user] 
                :attributes '([num-logins]) 
                :values '([+ 1 [num-logins]]))
```

Incidentally, that's one of the two ways you can organize column name and values in a query. The other (which I prefer whenever I'm changing more than one attribute at a time) is to pass up attribute-value pairs like so

```lisp
(insert-records :into [user] 
                :av-pairs `(([first-name] "Inai") ([last-name] "mathi") 
                            ([password] ,(salt-password pw salt)) 
                            ([salt] ,salt)))
```

As in regular SQL (or, at least, MySQL sql), if you're inserting a value for each column, in order, you can leave out the :attributes specification altogether. Simple, right? As long as you know SQL and Lisp, I mean.

The pitfalls i've hit in the coding bit really have more to do with some MySQL-specific (I guess?) things that I still didn't expect to be handling myself. For example, the first time I saw their [ ] notation, I thought "Oh, this is a way to translate some stuff to SQL notation". It seemed like a safe assumption that this would include things like [now] or [(now)], one of which I thought would call the sql NOW(); function to get the current datetime in the appropriate format. And that's a no. I honestly didn't think I'd have to write

```lisp
(defun mysql-now ()
  (multiple-value-bind 
        (second minute hour date month year day-of-week dst-p tz) 
      (get-decoded-time)
    (declare (ignore day-of-week dst-p tz))
    ;; ~2,'0d is the designator for a two-digit, zero-padded number
    (format nil "~a-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" 
                 year month date hour minute second)))

```

myself, but there you have it. Also, TIL that multiple-value-bind has an odd indeting pattern.

The next thing is that the timestamp column type doesn't seem to be supported by CLSQLs functional interface. The [reference page](http://clsql.b9.com/manual/def-view-class.html) I linked earlier states that you should be able to specify a timestamp column using wall-time, but that creates a vanilla datetime in MySQL (timestamps are separate, and will store the current time whenever that row is INSERTed or UPDATEd). The solution seems to be 


1.   Don't use timestamp columns, and instead manually call mysql-now when I need to update timestamps
1.   Sidestep the CLSQL reader syntax by using the query function anywhere you want a timestamp.


The second is unacceptable because it's basically what cl-mysql does by default, except without the automatic sql escaping. The default CLSQL syntax handles this, by the way, so feel perfectly free to call your admin account "'); DROP TABLE USERS; SELECT '", it shouldn't cause any trouble other than being annoying to type each time. To be fair, I'd probably only have to use SQL literals at table creation, so it wouldn't be the end of the world, but it's also not ideal. Not using timestamps where they're appropriate just because my tools don't like it is even worse. Hopefully, a solution presents itself, but judging from the [response over at SO](http://stackoverflow.com/questions/5147296/clsql-timestamp-column-type), I'm not holding my breath. Maybe I'm using wall-time incorrectly, or there's another column specifier that gets the correct behavior in MySQL, I dunno.

The only other problem I'm having is understanding how exactly you're supposed to use the with-connection and with-default-connection functions. Using with-default-connection doesn't seem to close the connection or (return it to the pool if you're using one). with-connection does, but it gives you style-warnings if you don't explicitly pass that connection to any queries you wrap in it. 

This last one is probably a broken understanding on my part though. Intuitively, I'd expect to either


- Wrap each handler function in a with-connection (so that any database hits happening as a result of that handler share a connection)
- Wrap each database-manipulating function in a with-connection (so that each database hit has its own connection. Sounds bad, but it's actually manageable on my current project)
- Start a connection with the server, and use that one to handle all traffic (which sounds scary in many ways, so I'm not seriously considering)


The second honestly sounds like the right choice (though I could be wrong depending on how much overhead is associated with starting a connection to the database server; I should run that through the profiler this weekend), but the first one is also acceptable. The trouble is that I can't reconcile either with the fact that with-connection really seems to want me passing explicit database references around. Like I said, more research is necessary.

Sorry for starting this month out on the boring side, but I've been poking at this for a week or so, and I needed to clear my head of it.
