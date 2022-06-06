% Notes on Lisp in Small Pieces
% Eric Normand
% Thu 05 Dec 2013 10:04:15 PM CST
% keywords=lisp, book

## Chapter 1

### Summary

A playful introduction to writing a Lisp interpreter. It is notable
for its exploration of the space of recursive evaluators and its
eventual return to a small core. As examples, the use of symbols as
variables is discussed, and several different scoping rules (no scope,
global scope, dynamic scope, and static scope) are implemented. In so
doing, the book eplores the history of Lisps and why the world has
settled on symbols as variables (terseness) and static scope (easier
to reason about).

### Quotes

> What makes Lisp unique--and thus what makes an explication of `eval`
  non-trivial--is its reasonable size, normally from one to twenty
  pages, depending on the level of detail. This property is the result
  of a significant effort in design to make the language more regular,
  to suppress special cases, and above all to establish syntax that's
  both simple and abstract.

(pp. 2-3)

> But the natural laziness of Lisp-users inclines them to use the
  symbol of the same name as the key associated with a variable.

(p. 5)

### Code in Clojure

#### Preamble

There were several decisions that I had to make when translating the
code to Clojure. I happened to find [this gist][fogus] in my search
for others translating LiSP to Clojure. I read it, and while it is
correct, I felt like I had to do it myself (differently) to honor the
intent of the book.

[fogus]: https://gist.github.com/fogus/3354936

One of the issues the book deals with is the minimal design of
Lisp. It deliberately uses a very small subset of Scheme in order to
show how little machinery was required to build an interpreter. As I
write this, I have not read past the beginning of Chapter 2, but it is
clear that the implementation language will be more and more
restricted as the book progresses. So I purposefully chose not to do
some shortcuts available in Clojure, such as destructuring let
bindings. The same goes for using fancier data structures than lists
to represent the environment. I chose to use lists (though `cons`es in
Clojure require a seq in the cdr position).

Another issue is mutation. The evaluator clearly defines `set!` as a
mutation operator. There are two ways to do mutation in Clojure: the
internal concurrency primitives or to bypass Clojure data structures
altogether (and use, for instance, Java arrays). Since I have not read
the rest of the book, I don't know where we're going, so I chose to
use a two-element Java array for cons cells. This also solves the
"dotted pair" problem. I hope this pays off as we go deeper.

Some odds and ends: I used a sentinel value for the empty list.




