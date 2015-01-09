# Page 1


* varying definitions of oop
 functions and data
 lua's version
 lisps version
 clojure/swift/rust/go version
 polymorphism
 my version: a set of behaviors (memory access is just a set of behaviors)
 enables one to say, I don't know exactly how this behaves at compile time

its a struct
plus polymorphism
plus namespaces for class based oop
- this isn't abstraction, this is just organization
plus it comes at a cost of not being able to extend types

plus various degree of permission/hiding depending on language
- this isn't abstraction, its safety which has questionable value


holywood pattern and given abstractions
lots of us live in some sort of framework where all the pieces are
given to us by the language or the framework and we basically just
assemble them like legos; we are given the pieces and the places
but we aren't using tools of abstraction to simplify our problem, we
are just using the pieces we've been given

duality of syntax by Doug Hoyte ( show book)
(also called compression by Richard Gabriel)
that's at the heart of all these tools from variables to function
to polymorphism, structs etc

^^^^^^ that is the key point, thats what we are after
how can we present a feature with less code
and how can we add new features or change old ones with less changes
if we have duality of syntax we can make changes without having to change
lots of other parts
patterns provide duality of syntax on a larger abstraction level; it permits
change


type based ploymorphism is an abstracted if statement
it can't change but in lua and javascript it can but
those you can reassign the path





* over engineered example of hello world
http://developers.slashdot.org/comments.pl?sid=33602&cid=3634763

* some slides that turn the gradual easy one into a mess while the first one
 looks basically the same

* are you abstracting or building with abstractions
* abstracting solves a problem

* change is number one priority of software (robert martin)

* good abstractions cant be guessed at, they are implemented to solve real problems
 those problems come about over the course of development
 so one has to refactor to those abstractions
 without tests refactoring is error prone and difficult
 ergo without tests, creating good abstractions is error prone and difficult
 testing code that wasn't written to test is difficult, and tedious
 tdd is writing code along with tests

* its fundamentally about dealing with change

* duplication is the root of all evil

* can there be objectively better code?
* better code has less duplication
* better code has surrounding code at the same level of abstraction

* encapsulation is not a goal; its a natural result of removing duplication
 and putting code at the same level of abstraction

* physical location / conceptual location of code

* new york slice architecture


* tdd book and kent beck

* uncle bob and solid

* refactoring book and code smells


* cargo cult -> don't mistake artifacts for the process

* patterns are abstractions that can't be implemented directly in language
* patterns are something to refactor to, not something to build with
