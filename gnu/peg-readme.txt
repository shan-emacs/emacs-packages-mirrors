This package implements Parsing Expression Grammars for Emacs Lisp.

Parsing Expression Grammars (PEG) are a formalism in the spirit of
Context Free Grammars (CFG) with some simplifications which makes
the implementation of PEGs as recursive descent parsers particularly
simple and easy to understand [Ford, Baker].
PEGs are more expressive than regexps and potentially easier to use.

This file implements the macros `define-peg-rule', `with-peg-rules', and
`peg-parse' which parses the current buffer according to a PEG.
E.g. we can match integers with:

    (with-peg-rules
        ((number sign digit (* digit))
         (sign   (or "+" "-" ""))
         (digit  [0-9]))
      (peg-run (peg number)))
or
    (define-peg-rule digit ()
      [0-9])
    (peg-parse (number sign digit (* digit))
               (sign   (or "+" "-" "")))

In contrast to regexps, PEGs allow us to define recursive "rules".
A "grammar" is a set of rules.  A rule is written as (NAME PEX...)
E.g. (sign (or "+" "-" "")) is a rule with the name "sign".
The syntax for PEX (Parsing Expression) is a follows:

    Description		Lisp		Traditional, as in Ford's paper
    ===========		====		===========
    Sequence			(and E1 E2)	e1 e2
    Prioritized Choice	(or E1 E2)	e1 / e2
    Not-predicate		(not E)		!e
    And-predicate		(if E)		&e
    Any character		(any)		.
    Literal string		"abc"		"abc"
    Character C		(char C)	'c'
    Zero-or-more		(* E)		e*
    One-or-more		(+ E)		e+
    Optional			(opt E)		e?
    Non-terminal             SYMBOL		A
    Character range		(range A B)	[a-b]
    Character set		[a-b "+*" ?x]	[a-b+*x]   ;Note: it's a vector
    Character classes	[ascii cntrl]
    Boolean-guard		(guard EXP)
    Syntax-Class		(syntax-class NAME)
and
    Empty-string		(null)		ε
    Beginning-of-Buffer	(bob)
    End-of-Buffer		(eob)
    Beginning-of-Line	(bol)
    End-of-Line		(eol)
    Beginning-of-Word	(bow)
    End-of-Word		(eow)
    Beginning-of-Symbol	(bos)
    End-of-Symbol		(eos)

PEXs also support parsing actions, i.e. Lisp snippets which
are executed when a pex matches.  This can be used to construct
syntax trees or for similar tasks.  Actions are written as

    (action FORM)          ; evaluate FORM for its side-effects
    `(VAR... -- FORM...)   ; stack action

Actions don't consume input, but are executed at the point of
match.  A "stack action" takes VARs from the "value stack" and
pushes the result of evaluating FORMs to that stack.
See `peg-ex-parse-int' in `peg-tests.el' for an example.

Derived Operators:

The following operators are implemented as combinations of
primitive expressions:

    (substring E)  ; Match E and push the substring for the matched region.
    (region E)     ; Match E and push the start and end positions.
    (replace E RPL); Match E and replace the matched region with RPL.
    (list E)       ; Match E and push a list of the items that E produced.

Regexp equivalents:

Here a some examples for regexps and how those could be written as pex.
[Most are taken from rx.el]

    "^[a-z]*"
    (and (bol) (* [a-z]))

    "\n[^ \t]"
    (and "\n" (not [" \t"]) (any))

    "\\*\\*\\* EOOH \\*\\*\\*\n"
    "*** EOOH ***\n"

    "\\<\\(catch\\|finally\\)\\>[^_]"
    (and (bow) (or "catch" "finally") (eow) (not "_") (any))

    "[ \t\n]*:\\([^:]+\\|$\\)"
    (and (* [" \t\n"]) ":" (or (+ (not ":") (any)) (eol)))

    "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
    (and (bol)
         "content-transfer-encoding:"
         (* (opt "\n") ["\t "])
         "quoted-printable"
         (* (opt "\n") ["\t "]))

    "\\$[I]d: [^ ]+ \\([^ ]+\\) "
    (and "$Id: " (+ (not " ") (any)) " " (+ (not " ") (any)) " ")

    "^;;\\s-*\n\\|^\n"
    (or (and (bol) ";;" (* (syntax-class whitespace)) "\n")
        (and (bol) "\n"))

    "\\\\\\\\\\[\\w+"
    (and "\\\\[" (+ (syntax-class word)))

See ";;; Examples" in `peg-tests.el' for other examples.

References:

[Ford] Bryan Ford. Parsing Expression Grammars: a Recognition-Based
Syntactic Foundation. In POPL'04: Proceedings of the 31st ACM
SIGPLAN-SIGACT symposium on Principles of Programming Languages,
pages 111-122, New York, NY, USA, 2004. ACM Press.
http://pdos.csail.mit.edu/~baford/packrat/

[Baker] Baker, Henry G. "Pragmatic Parsing in Common Lisp".  ACM Lisp
Pointers 4(2), April--June 1991, pp. 3--15.
http://home.pipeline.com/~hbaker1/Prag-Parse.html

Roman Redziejowski does good PEG related research
http://www.romanredz.se/pubs.htm