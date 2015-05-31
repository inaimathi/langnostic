Another Approach to Formlets, or, The Argument Against Nesting

I haven't been working on [slash](http://github.com/Inaimathi/Slash) much lately. At all, actually. 

I've been working on another project for myself, which I'll unveil in a month or so if I keep up my current rate of progress. I don't want to say more 'till then, except that it's written in Scheme, and involves PLT's webserver. One piece I've had to work with quite a bit is the formlet system, and I've found it flat-out inadequate.

The formlet system has a couple of bugs in it, and a single design choice (in my opinion a massive shortcoming) that I want to discuss and post alternative code for.

First the bugs, just because those seem to be the easy to discuss (in the sense that they're obviously mistakes as opposed to potentially defensible design decisions). It doesn't support password inputs, textboxes or select-boxes. 

Taking a look at the source, there is actually a function that generates inputs based on type, but its dispatch is bugged such that it does the same thing for text and password types (ie. it generates an input tag with type="text"). 

Text-areas and select-boxes aren't supported at all, because the naming procedure seems to misstep with them. Instead of putting the name in as a parameter to the tag, it's put in as the content (so, if you tried doing "Label: " (text-box . -> . content) it would actually spit out &lt;textarea>name="input_2"&lt;/textarea>. The way name assignment works also seems to inherently break option buttons, but I haven't teste that myself. The only way around this seems to be coding up your own generators for textareas and selects, as well as fixing the password bug.

I jury-rigged some stuff that worked half-way acceptably, then I noticed that actually, formlets don't really support automatic validation. That's the "massive shortcoming", by the way. Validation is something you have to do to the vast majority of all web forms, and it is always the same three-step process of 


1.   Get the output back, 
1.   check each piece of output against a certain predicate (different for each input, but always the same for a given instance)
1.   If all of the inputs pass, send the extracted info off to the target page, otherwise send it back to the caller with some helpful notes attached.


That makes it a prime candidate for automation, and I find it curious that the PLT docs suggest that the formlets' "nesting" property was in effect chosen over "validation".

Nesting is pretty thorny, not just because it directly competes with automated validation, but because it also gets in the way of auto-generating the associated HTML boilerplate. Each complete form has its own &lt;form> tag and &lt;input type="submit" /> elements, but if your goal is to let formlets nest, then you don't want each one generating that; you only want it produced by the outer one.

First and foremost, nesting takes away a lot of convenience. As I said above, if your goal is to have nestable formlets, then it gets problematic to generate the form tags. It also kind of prevents you from doing validation, because (as I found out thinking about the problem), doing validation requires that you pass the form into whatever page is going to be displaying it rather than generating it on the fly (this is the only real way to display errors effectively, and if you have to pass an arbitrary tree of data recursively to itself... well, it's possible, but much more complicated than necessary most of the time).

Second, it forces output complication. As soon as you have nested forms, you can't just return a flat list of outputs in order of the forms; to represent it correctly you really need to be passing a tree back at that point. The PLT Guys actually do this, according to their examples. This requires that you assign a variable name to the output of each input, and then specify what format/order you want them output. If you drop nesting, you can shortcut this. You get the ability to return the input results as a flat list in order of the inputs, thereby skipping the naming and return-order declaration.

Third, and probably most damning, you really wouldn't use nesting much. Where you'd want it is if you have tons of different forms in which you ask for a username and password, or if you're a complete masochist who wants to roll your own date-entry system instead of just using a javascript datepicker on your view layer. I'm going to ignore the second because it seems ridiculous. The first one is really the place where non-nesting hurts; if you have lots of different forms that share many (but not all) pieces, then it's important to break them apart for modularity. The thing is, I'd argue that this is a rarer situation than you might think, and you can compensate for it by putting a set of formlet-component macros together. Validation, by contrast, is something you want in the vast majority of all forms, so if I were designing a formlet system and had to pick between making either intra-form modularity or automated validation very easy, I'll take that second one any day of the week.

The above led me to write the below; an implementation of PLT formlets that emphasizes validation and boilerplate-elimination over nestability. The reason I haven't emailed this to the PLT guys (or just checked into github) is there were a few issues I wanted to iron out, and i wanted to double-check the request-reading code. As soon as I'm done I will do one or both, in the meanwhile, my working code is below for your perusal. 

You declare formlets (associating each input with a validating predicate and failure message), then run it through the duo of generate-formlet and display-formlet (the second of which automatically calls validate-formlet). It validates based on your declared predicates. If the whole form passes, it gets sent off to the next page. If not, it gets sent to the current page, where inline errors are displayed next to each input (and any user input other than password fields is restored so the user doesn't need to retype/repick anything). The formlet also automatically handles generating the form and submit tags.

Here's what a declaration looks like:

```lisp
(define register-formlet
  (formlet register-page profile-page "Register"
           ("User Name: " input-text (not-blank?) "Username can't be left blank")
           ("Password: " input-password (longer-than? 7) "Passwords need to be 8 characters or longer")
           ("Confirm: " input-password (matches-previous?) "You need to enter the same password in 'Password' and 'Confirm'")
           ("E-mail: " input-text)
           ("Comment: " text-area)))

```

And here's the code (with some inline docs):

```scheme
#lang scheme
;; "formlets.ss"
;;;DOCS
;A function that uses these formlets needs to have the generated form passed to it in a parameter named "form". The display-form macro depends on this. If you want to use a different parameter name, you need to use the call
;        ([form-param-name] ,(embed/url (validate-form [form-prototype])))
;;ex:
;; (define (login-page req form)
;;   (local [(define (resp-gen embed/url)
;;             `(html (body
;;                     ,(display-form test-form))))]
;;          (send/suspend/dispatch resp-gen)))
   ;;&lt;later>
;; (login-page req (generate-form test-form))

;The target function needs to accept one list argument (not a request, just a list). The list contains extracted bindings from the form, in the order they were declared.

;the definition of a formlet is as follows:
;; (define test-form
;;   (formlet login-page logged-in "Login"
;;         ("Username: " input-text (not-blank?) "Username can't be empty")
;;         ("Password: " input-password (longer-than? 6) "Passwords are longer than 6 characters")
;;         ("User Level: " (select-tag '("Anonymous" "User" "Admin" "Super")) (mismatches? #rx"Pick one") "You actually need to pick one, genius")))

;An unvalidated input is declared as
;    ("Label: " input-text)

(require mzlib/defmacro
         web-server/servlet)
;;;CORE FUNCTIONS
(define-macro (formlet src-fn dest-fn submit-button-value . inputs) ;;massages simple user input into something useable by validate-form and generate-form
  `(list (list ,src-fn ,dest-fn ,submit-button-value)
         ,@(map (lambda (input-spec)
                  (apply (lambda (label input-fn (validate-fn #f) (fail-message ""))
                           `(list ,label ,input-fn ,validate-fn ,fail-message))
                         input-spec))
                inputs)))

(define (validate-form a-form) ;;Checks each input against the validation functions. If they all pass, it sends the extracted bindings over to the destination (where they are presumeably acted upon). If any check fails, it returns to the source and displays error messages.
  (lambda (req)
    (let* ((fields (build-list (length (cdr a-form)) id->name))
           (field-vals (map (lambda (input-id) (binding->string input-id req)) fields))
           (errors (map (lambda (input-spec id)
                         (if (or (not (third input-spec))
                                 ((third input-spec) field-vals id))
                             #f
                             (last input-spec)))
                       (cdr a-form)
                       fields)))
      (if (errors? errors)
          ((first (car a-form)) req (generate-form a-form #:i-error errors #:val field-vals))
          ((second (car a-form)) field-vals)))))

(define (generate-form a-form #:i-error (inline-errors #f) #:g-error (general-errors #f) #:val (input-values #f))
  (lambda (a-url)
    `(form ((method "POST") (action ,a-url))
           ,(if general-errors
                (map (lambda (gen-error) `(div ((class "general-error")) ,gen-error)) general-errors)
                "")           
           (ul ((class "form-fields"))
               ,@(let ((id-nums (build-list (length (cdr a-form)) values)))
                   (cond ((and inline-errors input-values) (gen-inputs (cdr a-form) id-nums inline-errors input-values))
                         (inline-errors (gen-inputs (cdr a-form) id-nums inline-errors))
                         (else (gen-inputs (cdr a-form) id-nums))))
               (input ((type "submit") (value ,(last (car a-form)))))))))


(define (gen-inputs input-list . rest)
  (apply map
         (cons (lambda (input-spec name (an-error #f) (a-val ""))
                 `(li (span ((class "label")) ,(first input-spec))
                      ,((second input-spec) name a-val)
                      ,(display-error an-error)))
               (cons input-list rest))))

(define-macro (display-form form-prototype)
  `(form (embed/url (validate-form ,form-prototype))))
  
;;;Core helpers
(define (display-error an-error)
  (if an-error
      `(span ((class "inline-error")) ,an-error)
      ""))

(define (errors? e-list)
  (cond ((null? e-list) #f)
        ((not (boolean? (car e-list))) #t)
        (else (errors? (cdr e-list)))))

(define (binding->string id req)
  (bytes->string/utf-8 (binding:form-value (bindings-assq (string->bytes/utf-8 id) (request-bindings/raw req)))))

;;;Input types
(define (input-tag (type "text"))
  (lambda (n (value ""))
    `(input ((name ,(id->name n)) (value ,(if (equal? type "password") "" value)) (type ,type)))))

(define (text-area-tag)
  (lambda (n (value ""))
    `(textarea ((name ,(id->name n))) ,value)))

(define (select-tag options (default-option "Pick one"))
  (lambda (n (value #f))
    `(select ((name ,(id->name n)))
             ,@(map (lambda (single-opt)
                      `(option ,(if (equal? value single-opt) `((selected "selected")) "") ,single-opt))
                    (cons default-option options)))))

(define (id->name id)
  (string-append "input_" (number->string id)))

(define (name->id name)
  (string->number (substring name 6)))

(define (lookup-val a-list a-name)
  (list-ref a-list (name->id a-name)))

;;there is no "select" function because it needs the list of options no matter what. If it occurs that I'm repeating myself, I'll put the common ones in
(define input-text (input-tag))
(define input-password (input-tag "password"))
(define text-area (text-area-tag))

;;;Predicate generators
(define (not-blank?)
  (lambda (vals-list cur-input)
    (not (equal? "" (lookup-val vals-list cur-input)))))

(define (longer-than? n)
  (lambda (vals-list cur-input)
    (> (string-length (lookup-val vals-list cur-input)) n)))

(define (matches? regex)
  (lambda (vals-list cur-input)
    (regexp-match regex (lookup-val vals-list cur-input))))

(define (mismatches? regex)
  (lambda (vals-list cur-input)
    (not (regexp-match regex (lookup-val vals-list cur-input)))))

(define (matches-previous?)
  (lambda (vals-list cur-input)
    (equal? (lookup-val vals-list cur-input)
            (list-ref vals-list (- (name->id cur-input) 1)))))

(provide (all-defined-out))
```
