# Duplication is the Root of All Evil

![](olsen1.jpg)

---

# DRY
## (Don't Repeat Yourself)

---

# Test Driven Development: By Example
## Kent Beck

![right fill](tdd.jpg)

^remove duplication over and over

---

> Number one in the stink parade is duplicated code. If you see the same code structure in more than one place, you can be sure that your program will be better if you find a way to unify them.
- Martin Fowler

---

> Duplication is perhaps the biggest cost in maintaing a codebase over time.
- Jeff Langr

---

> Duplicated code is the most pervasive and pungent smell in software.
- Joshua Kerievsky

---

> Behind every duplicated block of code is a concept dying to be treated with respect and given a name.
- Sean Levin

---

# Design Patterns:
* Null Object
* Composite
* Adaptor
* Iterator
* Template Method
* Visitor
* State

---

# Polymorphism

A tool for removing duplicate if/switch statements.

```java
if (!thing) {
  return "";
} else {
  return thing.name;
}

...

if (!thing) {
  return 0;
} else {
  return thing.age;
}
```
---

# Polymorphism

A tool for removing duplicate if/switch statements.

```java
NullThing
int age() { return 0; }
string name() { return ""; }

...

thing.name();
thing.aga();

```

---
![left fit](twins.jpg)

# Similar but not the Same

-

# Forcing Duplication Up/Down the Stack

---

# Its Not About Compression

```java
parsedData = parseJSONData(data);
valid = removeInvalidEntries(parsedData);
displayResults(valid)

...

parsedData = parseJSONData(data);
valid = removeInvalidEntries(parsedData);
displayResults(valid)

```

---

# Its Not About Compression

```java
handleResults(data);

...

handleResults(data);

...

handleResults(data);
```

---

# Tests - Compression

```java
void testBeginningIsValid() {
  runTest(5, 6, 6, "param2", 756);
}

void testEndingIsValid() {
  runTest(5, 6, 6, "param9", 759);
}

```
---

# Tests - Compression

```java
void testBeginningIsValid() {
  Buffer b = Buffer.newWithText("param2", 6);
  Char c = b.charBetween(5, 6);
  assert(c.code == 756);
}

void testEndingIsValid() {
  Buffer b = Buffer.newWithText("param9", 6);
  Char c = b.charBetween(5, 6);
  assert(c.code == 759);
}

```
---

# Tests - Mocking Libraries

* Tools for easy inline class creation.
* Tend to attract duplicate construction code.

stub, stub, stub... change... shit
