# Yagni

Only implement the exact things that you need right now to solve the problem you need to solve right now.

Built on the dubious belief that if "If I do more work now, I will have less work later"

goes against the Kanban principle of least number of open tasks at once actually maximises throughput.

Let me do things now, it will be faster than dong one thing and then one thing.



People like abstractions, its what we do


Speculative Generality (cousin?)


cosco jumbo size stuff

more than you need

some guy with too much stuff


key points in Fowler's talk

key points crom c2 origin

examples of wrong sillhuette
could be a lot of work
but search around

abstraction is not an external requirement

does not mean abandon flexibility
getters and setters aren't an example of yagni

doesn't mean write shit code

changeable code is key
code with tests is much easier to change


abstractions are a tool for solving complexity that comes from duplication and scattered logic

yagni is not about thinking about the future, its just about coding it.


incremental development?



quotation from moore

The Basic Principle has a corollary:

Do Not Speculate!
Do not put code in your program that might be used. Do not leave hooks on which you can hang extensions. The things you might want to do are infinite; that means that each one has 0 probability of realization. If you need an extension later, you can code it later - and probably do a better job than if you did it now. And if someone else adds the extension, will they notice the hooks you left? Will you document that aspect of your program?


This is really going to be a clean framework. I'll make an abstract class out of this part so that folks can subclass it later, and I'll put in a bunch of well-commented overridable hooks in the concrete subclasses so that folks can use them as templates, and just in case somebody ever needs to build special debug subclasses, I'll put in extra stubs over there (somebody will thank me for 'em one of these days). Yeah, this is really going to be neat.
Thus is bloated software produced when our artistic sense gets the better of us. -- DaveSmith


Viper as an example of peremature generalization

I'm going to write "better" code now so I don't write bad code later

seems to offend some people (what's their argument?)

don't generalize from one

its harder to remove things that you aren't really using than not put them in in the first place

No Hooks (Charles Moore)
One is No Hooks. Don't leave openings in which you are going to insert code at some future date when the problem changes because inevitably the problem will change in a way that you didn't anticipate. Whatever the cost it's wasted. Don't anticipate, solve the problem you've got.


Martin Fowler
costs to not listening to yagni
cost of build
cost of delay (because you do future thing you aren't doing other present things)
unnecceessary feature odds are at least 2/3
cost of carry: the cost of adding anything is includes some multiple of all the features added before. adding something maybe useful now
cost of repair: even if you do guess the right future feature there is a risk of doing it wrong as well, and working around those mistakes

a lot of the costs can come from lots of small decisions; not so much implementing a month long feature at the wrong time but an hour here and there add up to lots of little features that aren't needed which complicate everything
