slidenumbers: true

# OOPWTF?

---

#Disclaimer

![right 90%](processdiff.png)

* No silver bullets
* In search of a better software process

^attempt to roll up a number of thoughts I've had the past year

^ comments are welcome

---

# What is OOP?

_hackneyed question, I know_

^functions + data, classes, polymorphism, encapsulation, protection

---

Lisp

```lisp
(defclass srt-time ()
  ((hr :initarg :hr :initform 0 :accessor hr)
   (mi :initarg :mi :initform 0 :accessor mi)
   (se :initarg :se :initform 0 :accessor se)
   (ms :initarg :ms :initform 0 :accessor ms))
  (:documentation "Time format for srt"))

(defgeneric display (what)
  (:documentation "Returns string that represents the object"))

(defgeneric normalise (time)
  (:documentation "Fix overflow of fields"))

(defmethod normalise ((time srt-time))
  (with-slots (hr mi se ms) time
    (loop until (< ms 1000) do (decf ms 1000) (incf se))
    (loop until (< se 60) do (decf se 60) (incf mi))
    (loop until (< mi 60) do (decf mi 60) (incf hr)))
  time)

(defmethod display ((time srt-time))
  (normalise time)
  (with-slots (hr mi se ms) time
    (format nil "~2,'0d:~2,'0d:~2,'0d,~3,'0d" hr mi se ms)))

(defun make-srt-time (arglist)
  (destructuring-bind (hr mi se ms) arglist
  (make-instance 'srt-time :hr hr :mi mi :se se :ms ms)))
```

^30 years old but is the model for clojure, go, rust
where implicit namespacing of methods doesn't restrict
adding new methods

---

Lua

```lua
Account = {balance = 0}

function Account:new (o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Account:deposit (v)
    self.balance = self.balance + v
end

function Account:withdraw (v)
    if v > self.balance then error"insufficient funds" end
    self.balance = self.balance - v
end

SpecialAccount = Account:new()

function SpecialAccount:withdraw (v)
    if v - self.balance >= self:getLimit() then
        error"insufficient funds"
    end

    self.balance = self.balance - v
end

function SpecialAccount:getLimit ()
    return self.limit or 0
end
```

^like Javascript construction is same language as rest of code


---

Java

```java
public interface MessageStrategy {
    public void sendMessage();
}

public abstract class AbstractStrategyFactory {
    public abstract MessageStrategy createStrategy(MessageBody mb);
}

public class MessageBody {
    Object payload;

    public Object getPayload() {
        return payload;
    }

    public void configure(Object obj) {
        payload = obj;
    }

    public void send(MessageStrategy ms) {
        ms.sendMessage();
    }
}

public class DefaultFactory extends AbstractStrategyFactory {
    private DefaultFactory() {;}
    static DefaultFactory instance;

    public static AbstractStrategyFactory getInstance() {
        if (instance==null) instance = new DefaultFactory();
        return instance;
    }

    public MessageStrategy createStrategy(final MessageBody mb) {
        return new MessageStrategy() {
            MessageBody body = mb;
            public void sendMessage() {
                Object obj = body.getPayload();
                System.out.println((String)obj);
            }
        };
    }
}

public class HelloWorld {
    public static void main(String[] args) {
        MessageBody mb = new MessageBody();
        mb.configure("Hello World!");
        AbstractStrategyFactory asf = DefaultFactory.getInstance();
        MessageStrategy strategy = asf.createStrategy(mb);
        mb.send(strategy);
    }
}
```

^patterns make for easy teasing but they are cooler than the jokes
would let on

---


Objective-c

![inline](objc.png)

---

![left fit](languages.png)

What do they all have in common?

^with so many wildly different languages all being object oriented; what do they all have in common?

^the interface

---

> _*The Interface*_
a mechanism for enabling dynamic behavior with static code

^it enables polymorphism

---
#Interface Example

```java
public class Person {
    public String name;

    public void printName() {
        System.out.println(name);
    }
}

public class VipPerson extends Person {
    public void printName() {
        System.out.println("Mr. " + name);
    }
}

public static void displayPerson(Person p) {
    p.printName();
}
```

---

# Why Object Oriented Programming?

^what is academic, why has implications

---
If OOP is useful at all...

![inline](moreawesome.png)

^ more awesome operator

^ first lets define more awesome

---

#What is a program?

A program is a machine executable implementation of
a set of constraints that maps a set of inputs over time to a set of outputs over time.


![inline](aprogram.pdf)


^other constriants: performance, portability, binary size

---

#What is more awesome?

![left fit](twoprograms.pdf)

* two programs
* meet same constraints
* is there an A more awesome than B?

^try to get audience to guess

---

#What is more awesome?

What if we did one of these to B?

* Rot13 all variable and function names
* Minify

---

#Why is A more awesome than B?

^fish for answer (Change)
same performance
done the same way
both work who cares?
why would someone have to work on it?

---

> Change

^programs change

---

![left fit](uncle.jpg)

#Uncle Bob

Robert C. Martin

The secondary value of software is to meet the user's needs.
The primary value is to change.

^very sharp guy
silly videos
brushed off at first, but now convinced

---
#New Code

```java
public static void main(String[] args) {

}
```

^not buying it? here's an example

---

#Changing Old Code

```java
public static void main(String[] args) {
    System.out.println("Nice to meet you, Mr. Bowie");
}
```

---

#Changing Old Code

```java
public static void main(String[] args) {
    if (args.length > 0) {
        System.out.println("Nice to meet you, " + args[0]);
    } else {
        System.out.println("Nice to meet you, Mr. Bowie");
    }
}
```

---

#Changing Old Code

```java
public static void main(String[] args) {
    String greeting = "Nice to meet you, ";
    if (args.length > 0) {
        if (args[1].equals("Iman")) {
            System.out.println(greeting + "Mrs. Bowie");
        } else {
            System.out.println(greeting + args[0]);
        }
    } else {
        System.out.println(greeting + "Mr. Bowie");
    }
}
```

---

> Get it?

^ forget about future features you can't predict
approx 100% of time you are changing your own old code

---

> What's wrong with old code?

> Why is change a problem?

^there must be some answer otherwise people wouldn't
prefer starting fresh over fixing old stuff

---

# Answer: Internal Constraints

^ every line of code you add to meet an external constraint / feature
also contributes internal constraints to the pool

and those existing internal constraints may conflict directly
with the code needed to implement the next external constraint

---

> Internal Constraints are a burden on Change

^ makes sense that if we can reduce the internal constraints
we can make code easier to change

---

![right fit](letoverlambda.jpg)

#Duality of Syntax

## Let Over Lambda
## Doug Hoyte

Duality of Syntax

Same syntax multiple behaviors

Richard Gabriel: Compression

^really talking about lisp and macros, but the
concept still holds

its abstraction

---

# The Interface
The OOP mechanism for duality of syntax

```java
public class Person {
    public String name;

    public void printName() {
        System.out.println(name);
    }
}

public class VipPerson extends Person {
    public void printName() {
        System.out.println("Mr. " + name);
    }
}

public static void displayPerson(Person p) {
    p.printName();
}
```

^this has less internal constraints than inlined code
displayPerson can stay the same but still do different
things at different times

^interface doesn't just mean java interface but the
concept behind it

---

# The Interface

```java
public class Person {
    private String name;

    public String getName() {
        return name;
    }

    public setName(String newName) {
        name = newName;
    }
}
```

^getters and setters are such a common practice
for solving this vary problem (change)
and are exactly this, a new interface

^by using the getter/setter interface I can change code
in one place, instead of everywhere

---

#The Interface

* Enables the same functionality with _fewer internal constraints_
* Fewer internal constraints, means _easier to change_
* Easier to change means _more awesome!_ :dancers:

^the idea of managing change is a key takeaway

^as you code and implement features, think about
what you also could do to make things easier to change
in the future

---

> "I'm already using an object oriented language so I'm already doing this."

---

> Maybe...

^oop as a tool for dependency management and modeling, sure

^as a tool for unentangling code, probably less so

---

# Building with Abstractions vs Abstracting

* UIViewController
* UIView
* UITableViewDelegate
* Hollywood Pattern and UIApplicationDelegate


^these are abstractions other people have come up with
so we can benefit from duality of syntax of their code

^fish for why its called hollywood pattern
"don't call us we'll call you"

---

# Origin of the 5000 line class

### Step 1: Take an existing abstraction
### Step 2: Give it a name that matches our problem domain
### Step 3: Add code to do stuff

^its not unreasonable to get in this situation
its perfectly natural

---

> Step 4: Refactor

^a step that receives too little attention in our industry

---

# Writing is Rewriting

“By the time I am nearing the end of a story, the first part will have been reread and altered and corrected at least one hundred and fifty times. I am suspicious of both facility and speed. Good writing is essentially rewriting.” _-Roald Dahl_

“I have rewritten — often several times — every word I have ever published. My pencils outlast their erasers.” _-Vladimir Nabokov_

^preparing for this talk is an example

^helping Grace become a better writer is an example

---

# It's too hard to get it right the first time.

^trying to solve how to implement a feature
and whats the best place to put this code
I'm not smart enough

---

![left fit](refactoring.jpg)

#Refactoring

##Martin Fowler

_Our "Manual of Style"_

_Code Smells_

* Duplicated Code
* Shotgun Surgery
* Long Method
* Speculative Generality
* _etc._

^tools for possible places and possible ways to make things better

^although smell kinda sounds like saying your code stinks
the metaphor they were after is that smells are possible
indicators that something needs your attention at some point

^feel free to check out my book at my desk

---

duplicated code as an example

shotgun surgery as an example


---

#Continuous Refactoring

* A perspective change: messes become problems with solutions
* Object oriented programming becomes a tool for taking things apart as well as putting things together
* Code starts to feel like a library of parts collaborating to produce the product
* Its fun and satisfying

^oop allows you to isolate independent parts of the code

^I guartantee after a few tries using that book, you'll do something
you love that you wouldn't have done before

---

![right fill](rich.jpg)

#Rich Hickey

##Creator of Cojure

##Simple Made Easy

##Easy: nearby, subjective

##Simple: not intertwined, objective (not easy)

^best talk ever

^german is not easy if you don't know it

^simple often means many smaller separate pieces

---

> Simplicity comes after complexity, not before.

^you've got to work for it

^nobody gets it right the first time

^oop is just one tool among many to help with the process

---

> Some Comments...

---

![left fit](gof.jpg)

#Design Patterns

Abstractions for managing dependencies not
directly supported by the language.

Build with: *bad*

Refactor to: *super sweet*

^feel snobby and overblown as something to build with

^see my books

---

> Indirecton without Abstraction

^design patterns without a problem

---

> Make it work first, then make it better

---

# "Where is anything getting done?!"

Abstractions mean more smaller pieces

Trading in physical locality for conceptual locality

Bad abstractions and bad names is still bad code

^just because we create an abstraction doesn't mean
we are making things better

^over estimate how easy giant methods and classes
are to deal with

---

## Abstractions aren't the problem

### Average adult has a 15,000 word vocabulary

### We don't understand every line of external libraries

### We certainly don't know what's behind Apple's abstractions

### Even worse...

---

# We don't even write our own programs

Contribution to your app breakdown:

![inline](loc.pdf)

^everything is an abstraction
the fact we don't think about it shows how
good we are at dealing with abstractions

^story of bacteria cells in our body outnumbers our own

---

> Its hard to refactor without any tests

---

![right fill](tdd.jpg)

Kent Beck, TDD Explained

big fan. my code has tests
because it has tests I move it around and have confidence that I can
move it to something that is more awesome, and by more awesome I mean
reduces the burden of future changes

maybe last as my version

every line of code is born in the furnaces of Mt. Change?

---

testing is about using the same code in two different places once in the context
of a test another in the context of the app

it encourages finding seams that enable change

---

I think we have an awkward relationship with the code we write.

We feel uncomfortable with some choices, worried we didn't do it as well as others

its important to get a better relationship and see it as part of a process; a process
of continual improvement and adjustment; the lessons in refactoring can help us take
a more mature position

---

most importantly I think is recognizing that the most important thing we can do is
pay attention to our development process

we have strange relationship with the code we write and can get emotionally invested
in it when we are proud of it; or feel terrible when we mess up; or worry that other
people are writing better code that we aren't capable of

---

the truth is good code is the result of good process applied over and over; we will never
get things perfect, but if we distance ourselves from the code and invest emotionally in
our process then the code will be goo

* be emotionally invested in your process not the code that results from that process

---

breakup longer slides? to shorter quotes

add backgrounds representing change, complexity
dynamism






style, mostly a matter of taste; taste is hard to target
we have to reach for things that we believe are more objective

eg, I could write all my function names in russian some people
would prefer that I'm sure


do I want to talk about different types of oop and how they support this?

clojure/swift/rust/go are similar

class based oop is just namespaces
  and a way to provide subtypes
  plus it comes at a cost of not being able to extend types

plus various degree of permission/hiding depending on language
- this isn't abstraction, its safety which has its ups and downs

* type based ploymorphism is an abstracted if statement
it can't change but in lua and javascript it can but
those you can reassign the path

* would love to talk about javascript and lua style of objects
in lua even namespaces are part of the language in tables
 its brilliant

* over engineered example of hello world
http://developers.slashdot.org/comments.pl?sid=33602&cid=3634763

* encapsulation is just behavior behind an interface

* cargo cult story

* confusing artifacts for process

* oop is not the only way to provide benefits
functional, static analyzers, better language idioms where the default choice
 of the language (the lazy choice) is the choice that makes future change easier
