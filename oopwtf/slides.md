footer: @ Sean Levin, 2015
slidenumbers: true

# OOPWTF?

---

What is OOP?

_hackneyed question, I know_

---

classic shape example
functions plus data

---

lisp example of oop

---

java example of oop with patterns

---

rails example with huge model

---

objective-c example with huge controller

---

scheme version

---

javascript example with callbacks going off the side

---

given those are all subclasses of our abstract oop class
and how different those look it would seem theres not
much left that abstract class, so maybe its just an interface?

---

interesting things about interfaces is that there is no
data and there is no behavior
its just a set of guidelines for playing nicely with others

---

and that is what oop is really about
interfaces which enable run time variability

maybe in a later slide I want to say functions and behavior
as two interfaces
get/save data (structs) -- memory access is just a behavior
runtime behavior variability (polymorphism)
structs aren't oop
all thats left is variability, which can be implemented any way
but all require interfaces; thats it

---

we can get back to that
maybe a better question is why oop?

---

if oop is useful at all, then there is some set of features
F in some world W that satisfies

my program + F >> (more awesome operator) my program
(maybe some emojii version)

for some definition of more awesome otherwise what's the point?

cute diagram

---

I'd really like to believe that the more awesome operator
exists in a objective sense.

and I do believe it exists (minus some controllable human factors)

---

software nothing more than a machine executable implementation of
a set of constraints that maps inputs over time to a set of outputs over time

those constraints are called features (and possibly some misfeatures)

keep in mind performance can be a constraint, as can the machine running it,
binary size, not just the obvious stuff

---

so given two different programs that meet these constraints for the same
set of inputs and outputs

can a be more aesome than b? (diagram)

can anyone think of an example?

---

take program a and rot13 on all the variables/functions/classes to get b
or run minified javascript (although that does solve a binary size constraint)
how many people would like to inherit a program that only existed in its minified version

a is more awesome than b

---

given that all these things are identical why is a more awesome than b?

same performance
they both get it done in exactly the same way
they both work, who cares?
why would someone have to work on it?

fish for the fact that things will change.

---

#Change

the unwritten constraint, software always changes
they both don't meet that one constraint the same

---

Uncle Bob slide

quotation, photo
the number one feature of software is change
the second is to do what people want
site, books, great guy
maybe two slides?

---

int main() {} is an example of new code

---

add hello world, just changed old code

---

add more, changed old code

---

add more, changed old code

---

get it?

---

forget about the future features you can't predict
99.99999999% of the time you are changing old code

---

but what's wrong with old code? if you are just adding
features aren't you just adding code?

why is change a problem?

if change is guaranteed, change + old code
has to come at some cost otherwise it wouldn't be a big
deal, but we know from experience it is
the more old code the more change comes at a cost

fish for why is that?

(this slide needs help)

---

answer: internal constraints

as you satisfy external constraints
you set up implicit internal constraints

often/almost always those internal constraints
are in conflict with the new feature so they must
be changed; that has to be done in a way that
still satisfies existing constraints

that's hard

---

so given two otherwise similar programs, the one that
is more adaptable to change is the one that is more awesome

---

of course change isn't an absolute; change to what?
a may be more amenable to change to y
while b may be more amenable to change to x

its an imperfect world, tough

---

amenable to change means total amount of effort
required to introduce a change:
clarity, internal constraints, reduction of bugs
introduced in the process

---

so in order for oop to be helpful, oop must provide
features that help manage change

---

duality of syntax

hoyte, picture of book

some quotation

lisp macros

---

the interface is the primary mechanism for enabling this
duality of syntax

I can have a block of code that works one way, and when it needs to
behave differently I can enable that behavior change without having to
change other parts. its variability of behavior its polymorphism

---

probably need some examples of duality of syntax
in different languages (static and dynamic interfaces)

---

encapsulation is something that people bring up with oop, but again
this is just an interface; we set up rules where code can play nicely
with others and behind that wall we can change functionality

when we talk about abstractions, we are essentially putting a set of behaviors
behind an interface

---

design patterns and architecture are the same thing
if they make code better then they do it because they enable
change. in most cases, they provide flexibility in ways that
aren't directly supported by the language

---

my irrifutable logic by now has convinced you that lessening the costs
of change is an act of awesomeness, and that oop theoretically provides
this awesomeness by making change less of a burden, so how do you do
oop?

---

easy right? new file, type in class {} add some properties and functions
sweet, oop; what's the big deal? We all do it every day.

---

wrong

---

occasionaly my mind is brought back to the concept of
cargo cults and cargo cult programming

image, story behind the term

---

mistaking the artifacts of a process for the process itself

(profound theory about life actually)

---

the artifacts are the classes objects interfaces

but the process is seeing where the same code can be leveraged to work
in different places and inserting interfaces that can isolate those changes
from other working parts of the code

---

key concept here is that is a process that goes on every step of the way as
we code. and being part of this process of finding seams where we can isolate
changes while leveraging other code we've written to accommodate internal constraints
is the magic that produces useful objects

---

The hard part is knowing where to put these seams. If we don't know where the next part
of code that will have to change will be, how can we justify abstractions.

That's a reasonable complaint. And its exactly how we end up with 300 line methods and 1000line
classes. We don't know so we get something working, perhaps we could have more flexibility
but it works, so why change it. I've got Artem banging on my desk asking me when I'm finished!

---

I don't think we can get it right the first time. Experience gives us some intuition but
its not enough. Refactoring is the only way.

---

Refactoring Book

definition: refactoring vs programming

how to find these seams, where to put code, and how to do it

code smells

---

in my experience, people only refactor when they have to in order to
enable a feature that they can't figure any way around

I just don't think people do it enough. I'm only just getting better
from rarely doing it myself. Its a shame because its fun when you know
what you are after.

its not just move code around willy nilly, its to find ways to enable
duality of syntax which becomes a seam we can use forever after

---

another problem with refactoring is that its hard without tests. rearranging
working code is just not worth it unless you have to or feel comfortable

refactoring tools are one way

but unit tests are the other.

---

Kent Beck, TDD Explained

big fan. my code has tests
because it has tests I move it around and have confidence that I can
move it to something that is more awesome, and by more awesome I mean
reduces the burden of future changes

---

so without refactoring, the kind of oop that leverages duality of syntax is too hard
and without tests refactoring is too hard

---

I know. You are all thinking you subclass views and view controllers, and
delegate objects. You are doing this way more than I am giving you credit for.

---

There's a difference between abstracting and building with abstractions.

somone else already has done the abstracting. because they want to use
duality of syntax so you can leverage their work

---

from the first moment you start a new project in xcode, they hand you a template with
an empty view controller. They wrote the app already, you are just filling in the blanks
in the empty method calls they already gave you.

The app is loaded into memory and starts running their code, then their code says, "anything else you want?"

this is the inversion of control pattern, or sometimes called the Hollywood pattern
"dont call us we'll call you"

the abstraction has already been built

so when it comes to finding where to put the code we just put it where we can find an empty
space. repeat until we have a 500 line funtion or 1000line class.

its not weird is normal, but I propose its suboptimal

---




do I want to talk about different types of oop and how they support this?

clojure/swift/rust/go are similar

memory access is just set of behaviors too (structs)

class based oop is just namespaces
  and a way to provide subtypes
  plus it comes at a cost of not being able to extend types

plus various degree of permission/hiding depending on language
- this isn't abstraction, its safety which has its ups and downs




not just breaking down that makes things easier to code
but breaking things down to easier to understand
doing just the same as what other people understand is lame
why program to the lowest common denominator overly tricky is
not a good end in itself but if it solves a real problem its worth it


the question is one class with two sets of methods and two sets of state
completely separate; is that a problem?
all that is some sort of physical locality classes are kind of artificial
they are just a file and an open and close brace and implicit namespacing


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


* type based ploymorphism is an abstracted if statement
it can't change but in lua and javascript it can but
those you can reassign the path

* would love to talk about javascript and lua style of objects
in lua even namespaces are part of the language in tables
 its brilliant

* oop is not the only way to provide benefits

* over engineered example of hello world
http://developers.slashdot.org/comments.pl?sid=33602&cid=3634763

* some slides that turn the gradual easy one into a mess while the first one
 looks basically the same

* abstracting solves a problem

* good abstractions cant be guessed at, they are implemented to solve real problems
 those problems come about over the course of development
 so one has to refactor to those abstractions
 without tests refactoring is error prone and difficult
 ergo without tests, creating good abstractions is error prone and difficult
 testing code that wasn't written to test is difficult, and tedious
 tdd is writing code along with tests

* duplication is the root of all evil

* can there be objectively better code?

* better code has less duplication

* better code has surrounding code at the same level of abstraction

* physical location / conceptual location of code

* patterns are something to refactor to, not something to build with
