# Inventorium: A RESTful Inventory Management API in Haskell

## Description and Motivation

During the Spring 2015 semester, Dr. Hayes introduced our software engineering class
to XXXX and XXXX, who are involved with managing technology for Powell County Schools.
In an experiment in software team management, the class organized into subgroups to design
and implement a single software package to aid in tracking the significant inventory of
desktop computers, laptops, tablets, and other pieces of technology that XXXX and XXXX
were tasked with managing.

For various reasons, the project was unsuccessful. However, being from Eastern Kentucky myself,
I was interested in contributing to the project and seeing something really useful delivered
to the end users in Powell County.

During our software engineering course, my subgroup was primarily responsible for the design of
the application and, later, the management of the rest of the development subteams. Our design
was a typical three-tiered architecture:

- a browser-based client,
- a web API,
- a relational database.

We made specific technical decisions based on the realities of the large team. Our primary
criterion was finding technologies with which our team members had prior experience, but
secondarily, we needed technologies that were accessible in both paradigm, documentation,
and availability of learning resources.

For these reasons, we chose to implement the web API in PHP, a widely-available dynamic
programming language well-suited for web development. We knew a basic RDBMS would be 
the best choice for the database layer and chose MySQL for its ease of setup. Finally,
we chose a combination of Twitter Bootstrap and Backbone.js to implement the browser-based
client. Twitter Bootstrap is styling and behavioral library, which let the UI team focus
on the UI despite relatively shallow web design skills. Among the many JavaScript frameworks,
Backbone.js was the one the other front-end subteam felt most comfortable with.

Perhaps the most successful technical choice in the initial design was the use of two software
systems, Vagrant and Ansible, to provide consistent, reproducible virtual development environments
for the team. Vagrant offers a way to create, manage, and boot virtual machine images while Ansible
is used to describe and execute provisioning strategies for remote systems, virtual or otherwise.
Prior to all the subteams beginning their work, our team set up a virtual environment that could
be reproduced consistently by individual team members with just a few commands.

These technologies were, for the most part, ones that I was very comfortable with. In my
day-to-day work doing web application development, the so-called LAMP stack (Linux, the
Apache web server, MySQL, and PHP) is very common. This was useful, since I would be able
to answer any questions that came up in the process.

However, that same work experience has taught me that many aspects of this technology stack
are woefully inadequate. For this reason, I chose to explore re-implementing this basic
architecture while making technology choices based on technical factors rather than the
expediencies required by a time-constrained, large-group project.

## Architecture

As I said, the notion of using virtual machines for development was really the most successful
part of the original project. With surprisingly few issues, around 20 students were able to
start contributing code to the project despite different operating system configurations and
and experience with the technology stack.

However, the environment we used has a potentially serious drawback. It consisted of a single
VM that contained both the web server, responsible for serving both the web API and static
files (HTML, JavaScript, CSS, and images) for the client, and the database server.
In a production scenario, this would clearly be problematic for scalability.

Ideally, the API server, the static web server, and the database server should all be separate. This
offers more flexibility: as usage increases, multiple API servers could be run behind a load balancing
mechanism or multiple database servers could be spun up to handle queries, via replication.
Of course, if managing a single reproducible development environment is a challenge, managing several
independent environments certainly requires an automated, reproducible solution.

I chose to solve this problem using a technology called Docker. Docker is a so-called
"containerization" platform. Essentially, individual services (an API server, a database, etc.)
can be packaged as containers: lightweight, bare-bones VMs that can (but are not required to)
run on the same host machine. Containerized services are completely isolated, eliminating
issues where, for example, an installed dependency for your database server is incompatible with
your the API server. The containers can still communicate over the network by explicitly mapping
network ports within the containers.

Inventorium, then, requires a container for the static web server, which was configured to
forward API requests to the container that runs the API server. Similarly, the API server's container
communicates with the database server. Although this was not implemented, there is very good
support for deploying containers to cloud platforms like Amazon Web Services. This is an advantage,
because it would allow Powell County Schools to make use of Inventorium cheaply, without needing
a dedicated server, and avoiding potential issues with deploying to some shared server in the
school district's IT office.

The web server container runs the Nginx web server. The original implementation used the Apache
web server for simplicity. Apache uses OS-level threads to handle each web request, which
introduces quite a bit of overhead for each request. This is less important when each request
requires a great deal of CPU time (e.g., when responding to a complex API request), but when
handling many relatively simple requests (for static files or simply proxying requests to an
API server), Nginx's single-threaded, event loop-based architecture is offers higher throughput and
better responsiveness in this use case.

I chose to carry on using a RDBMS for the project, rather than opting for a NoSQL solution
such as a document-based database like MongoDB. Ultimately, I chose to use PostgreSQL rather
than MySQL. While MySQL is an extremely common choice for small to medium sized web applications, but
although it has made improvements in the past few years in terms of standards compliance, transactional
safety, and data integrity, PostgreSQL is still regarded as better in these areas. One potential area

The final choice, and certainly the focus of both my development effort and the technical
discussion here, regards the implementation of the API server. I chose to use Haskell, a strong-
and statically-typed pure functional language that compiles to native code. Haskell's
pedigree is decidedly academic and it is often derided as being overly theoretical,
but as I will discuss below, it has a number of features that make it appealing for what
is often called "line of business" software and not just writing compilers or experimenting
with type systems and programming language theory.

## The Web API

Generally, the purpose a web API is to expose the operations of the business domain over HTTP.
We use an architecture called Representational State Transfer (REST) to accomplish this. Described by
Roy T. Fielding in his 2000 PhD dissertation, REST is an architectural style that emphasizes building
around the fundamentals of the web: HTTP and hypermedia (i.e., navigable links between entities).

In a RESTful architecture, domain objects (and collections or other aggregates of those objects) are
*entities* (in the HTTP sense), i.e., resources addressed by URLs. They may be available in many
representations: an image resource might be available in a JPEG representation or PNG. HTTP's methods
(verbs like `GET` and `POST`) describe the operations available. A `GET` request is a request for a
representation of a resource, i.e., a read operation.  A `POST` request takes a representation of an
entity and requests that the application store it, i.e., a create request.

Other aspects of the API are all handled through the well-defined channels of HTTP. Options and
parameters can be specified using URL query string parameters and/or HTTP headers. Similarly,
errors are reported via HTTP status codes. If a user requests an entity that does not exist, 
the result should be the well-known 404 response. If the entity exists but the user requests
an representation the server does not know how to provide, the API should instead respond with
HTTP's "406 Not Acceptable" error code.

REST can be a restrictive architecture, but its principle of offering a "uniform interface" to APIs
is powerful. For example, HTTP defines `GET` requests to be *idempotent* operation, meaning that
a `GET` request should not change the state of an entity on the server. Thus, if an application
properly implements the REST architecture, you get caching of `GET` requests for free via standard
HTTP caching mechanisms.

The inventory management domain fits naturally within the REST scheme. The system tracks facility
information (buildings and the rooms within them), inventory items and their properties (e.g., a laptop's
CPU, memory, and hard drive specs), and "check-in" records that indicate a given inventory
item was logged as being present in a given room at a given date and time.

The API organizes this domain with a system of hierarchical URLs. For example, facility 
information is organized into four entities addressed by different URLs:

a. `/api/buildings`: the collection of building entities
b. `/api/building/:buildingID`: an individual building entity with the given ID
c. `/api/building/:buildingID/rooms`: the collection of rooms in a given building
d. `/api/building/:buildingID/rooms/:roomID`: an individual room entity

URLs (b) and (d) address individual entities. HTTP `GET` requests retrieve a representation
of that single entity, while HTTP `PUT` updates it (with the new details included in the
body of the HTTP request), and HTTP `DELETE` deletes the record (if allowed by business logic
rules).

URLs (a) and (c) address *collections* of entities. HTTP `GET` retrieves a representation
of the entire collection and `POST` creates a new entity within the collection (with the details
of the new entity in the request body). HTTP `DELETE` is not supported.

## Implementation

Before we discuss the specific details of the web API's implementation, I will present 
a short example that illustrates a few of Haskell's features. The following code sample
implements uses parametric polymorphism to implement a homogeneous linked list and a
function over that data type.

```
data List a = Cons a (List a)
            | Nil
            deriving (Eq)

length Nil              = 0
length (Cons head tail) = 1 + length tail
```

We define a data type `List` that takes a single type parameter `a`.  This is a homogeneous
list, and every element will be of that type `a`. `List` is a recursive, algebraic data type,
since it is the *sum* of two cases: a `List` is either a `Cons` cell (to use the Lisp terminology)
that contains a value of type `a` plus a pointer to another `List`; or it is `Nil`, the empty list.
Note that `Cons` is itself a *product* of two types, `a`, the type of items in the list
and, recursively, `List a`.

The `deriving` keyword instructs the Haskell compiler to generate an
implementation for the `Eq` typeclass. Typeclasses are analogous to Java interfaces and
a datatype must implement `Eq`'s interface to be usable with the `==` equality operator.
With this automatic derivation, lists containing any data type that implements `Eq`
can be compared for equality.

Finally we define a function `length` on `List`. We use a feature called *pattern matching*
to define the two cases for the function. If `length` is applied to `Nil`, the result
is zero (an empty list has length zero). Otherwise, we have a `Cons` cell with a `head`
element and a `tail` list and the length is one plus the length of `tail`.

This small example demonstrates a number of Haskell language features that have
important practical implications. Algebraic data types are more expressive than
enumeration types and they make the pattern matching in our function definition possible.
They also allow the compiler inform us when our pattern matches are not exhaustive, which
often indicates a bug and to systematically generate code for the typeclass implementations
as we saw with `Eq` above.

Parametric data types (known as *generics* in some languages) are also powerful. With
our parametric list type, we can work with `List`s of `String`s, `Integer`s, or any
custom data type and use all the existing `List` code while still enjoy the security 
of knowing that the compiler can tell the difference between a `List` of `Strings` and
a list of `Integers`.

Parametricity in combination with Haskell's default purity can be very powerful 
for reasoning about your code. A function of type `a -> a`, i.e., a function
that takes a value of some type `a` and returns a value of that same type,
can only have one implementation in a pure language: the identity function. This
is a very trivial example, but the Haskell ecosystem is filled with libraries
where parametricity and purity allow their maintainers to reason about the correctness
of their code over and above simple type safety.

Consider our definition of the `length` function. Notice that despite all of the
discussion about the language's type system, We did not actually have to add a type
signature! Because Haskell makes use of *type inference*, it is actually able infer
that `length` has a type of `List a -> Integer` just based on the types of the patterns
and return values used in the definitions. Anyone who has written Java code that
looked like `HashMap<Integer, String> = new HashMap<Integer, String>()` will
appreciate the ability to use an expression and let the compiler infer the type
and make sure it is consistent. It is standard practice to annotate top-level
functions with a type signature, both for documentation purposes and as an early
sanity check, but in most cases they are unnecessary.

Finally, we should take a closer look at what are perhaps Haskell's most radical
features: *laziness* and *purity*. Laziness, or non-strict evaluation, means that
when evaluating an expression, subexpressions are only evaluated as needed. The C
language uses lazy evaluation to short circuit operators like `&&` and `||`, but
Haskell uses this evaluation strategy everywhere by default. In practical terms,
evaluation is driven by demand. For instance, we could cleanly define an enormous
(or even infinite) data structure like a game tree, and it will only be evaluated
as deeply as its consumer requires.

Implementing lazy evaluation in a standard imperative program would be quite 
tricky, if not impossible. As a result, Haskell functions are pure, meaning
they have no side effects (direct manipulation of memory, I/O, etc.) and
always return the same result given the same parameters. This property is
nice for reasoning about code and it actually enables the Haskell compiler
to make optimizations that would be impossible in an impure language.

Of course, real software (like web API servers) needs to do things like read
user input or talk to the network. Haskell exposes these in a rather interesting
way. Values of polymorphic type `IO a` represent instructions for the Haskell
runtime to perform some operation that may have side effects that will yield
a value of type `a`. 

For example, `getChar` is a function of type `IO Char`. Calling `getChar` does
not return a `Char`. Instead, `getChar` is a pure function (as all Haskell functions
are) that returns an instruction that, when given to the runtime, will
read a character from standard input and yield it to subsequent `IO` actions.
Primitive operations like reading a character can be sequenced together to
create more complex operations like reading a line from an input stream. 

`IO` is an example of a *monad*, a common abstraction in Haskell. A monad
can be thought of as a computational context that supports sequenced operations.
Other examples include sequenced computation with mutable or read-only state,
non-deterministic computations, or computations that may or may not yield a result.
Haskell also supports the notion of *monad transformers*, which compose multiple
monads together into a *monad stack*. In essence, sequencing and custom monad 
stacks allow us to define our own custom imperative sub-languages: I/O with
read-only configuration, non-deterministic computation with short-circuit failure,
or whatever the application needs.

### Libraries

Three Haskell libraries were pivotal to the implementation of the web API: Servant, for
defining and implementing RESTful APIs, plus Persistent and Esqueleto for database access.

**Servant**

The core concept in Servant is to express a REST API as a data type. That data type forms
a specification of the API that can be used by the compiler to guarantee that the functions
that implement the API conform to the specification. Additionally, Servant's code for
dispatching HTTP requests uses this specification for routing those requests to the appropriate
handlers, decoding request details into Haskell values, performing content type negotiation,
validating HTTP headers, encoding resulting values into appropriate content types, and packaging
the response, including using appropriate HTTP response codes.

Servant uses some of the more advanced extensions to the Haskell langauge that have been
implemented in the de facto standard compiler, GHC. Importantly, it uses type-level literals
allowing strings, natural numbers, and lists to be used in type definitions. I will not
go into the details here, but the concepts are discussed further in [1].

Here is a fragment adapted from the project source code:

```
type FacilitiesApi = "api" :> "buildings"
                     :> Header "Authorization" AuthToken
                     :> ReqBody '[JSON] Building 
                     :> Post '[JSON] BuildingDetail
```

This defines a data type that specifies a single API endpoint available at the URL
`/api/buildings`. It specifies that the HTTP header `Authorization` should be captured
for use by the endpoint's handler, interpreted as a value of type `AuthToken`. It requires a
the request body contain JSON that can be decoded into a `Building` value. Our type also specifies
that this endpoint responds to HTTP `POST` requests and will return a JSON encodeding of a
`BuildingDetail` value if successful.

The value of the `Authorization` header and the decoded `Building` value
will be captured and passed to the handler function, again adapted from the project source:

```
postBuilding :: Maybe AuthToken -> Building -> Handler BuildingDetail
postBuilding auth building = do
    checkAuthToken auth
    validateBuilding Nothing building
    buildingId <- runDb $ insert building
    return $ BuildingDetail { building = Entity buildingId building
                            , rooms = Nothing }
```

The endpoint's handler is simply a function. Its first parameter is a value of type
`Maybe AuthToken`. `Maybe` is a sum type that represents a value that may or may not be present.
So, if the request supplied an authorization token `"abc123"`, we would receive the value 
`Just "abc123"`. If none was supplied, we get `Nothing`.

The second parameter is the decoded `Building` value we got from the request's body.

The return value is of type `Handler BuildingDetail`. `Handler` is our application's custom monad
stack which supports access to read-only configuration information, exceptions, and general I/O,
primarily for database access. In other words, our API endpoint handlers are pure functions
that have configuration implicitly wired through them, can throw exceptions, and return instructions
for how the Haskell runtime should perform I/O in order to build up a value of type `BuildingDetail`.

We will cover more specific details about the implementation below, but it is worth reflecting
on how Servant worked in practice. There are many web frameworks available, like Ruby on Rails, Django
for Python, and Laravel for PHP. One important criteria for judging this frameworks is their ability
to abstract the common details of the HTTP request/response lifecycle. Servant does this as well as,
and perhaps better than, any framework I have worked with. Consider this workflow:

- The request URL is parsed and mapped to an appropriate handler based on the request method (`GET`, 
  `POST`, etc.)
- HTTP headers are parsed and, where applicable, marshalled into native data types:
    - The request `Content-Type` header is checked to ensure that the body of the request is in
      a format that our application is willing to accept
    - The `Accept` header is checked to ensure that the client will accept a response in a format
      our application is willing to deliver
- URL path fragments (such as the ID of a building) and query string parameters are parsed and
  marshalled into native data types
- The relevant path fragments, query string parameters, and headers are passed to the handler.
- If successful, the return value of the handler is marshalled into an appropriate encoding 
  based on the request's `Accept` header.

There are two important themes here. The first is that in all cases, our application logic ought deals
with semantically meaningful domain values rather than simply strings or associative arrays (probably
full of strings themselves!) and our framework should abstract the details of marshalling raw strings
from the request into our domain data types and from our domain data types into raw strings in the
response.

The second is that the framework should abstract the details of handling as many common HTTP error
cases as possible. If we can declaratively define the content types our application is willing
to accept, the framework should use that information to return `415 Unsupported Media Type` without
our application code's direct intervention if the client gives us a content type we do not support
of return `400 Bad Request` if the request body cannot be parsed into a suitable value.  Of course,
there will always be error cases that only our application can know about. If a request is made for
a record that is not in the database, we should not expect the framework to know to return `404 Not Found`,
but we should expect it to return that error response if the request URL does not map to any known handler.

Servant is quite successful in these two regards, and I would argue that the Haskell language
contributes to that succes. It does not surprise me to see a pervasive focus
on using meaningful domain data types in a strongly-typed language like Haskell, but Servant leverages
Haskell's features to good effect, using typeclasses to automate marshalling to and from
domain data types. For example, if you have a data type that implements the `ToJSON` and `FromJSON`
type classes, that data type can be accepted in the body for requests with the
`Content-Type: application/json` header and returned in the response for requests with the
`Accept: application/json` header.

Similarly, any language could support a framework that offers a declarative way to define acceptable
content types, required headers, and so on, but Servant's radical approach of using Haskell's type
system to make that declarative specification not just a source of runtime information but a
binding, compile-time contract for handler functions is extraordinarily powerful. Servant also
supports using that same type-level declarative specification to drive tools that generate 
API documentation and client code for other languages like JavaScript.

While working with Servant, I did come across a few odd corner cases, mostly involving unexpected response
codes in certain error cases. It is possible Servant understands the HTTP specification better than I do,
but Servant is a relatively immature library. However, despite some shortcomings, what Servant offers is
a library that takes the classic software engineering mantra, "Don't Repeat Yourself", stretches it
to its limits, and lets the compiler enforce it all. I was very impressed.

**Persistent and Esqueleto**

- Persistent, Esqueleto
    - High-level database access primitives
    - Type-checked DB primitives (though some Esqueleto queries can fail at runtime)
    - DB safety: No SQL injection

### Code Organization and Walkthrough

- Types
- Handlers
- Main.hs 
- Other files, functions

## Testing

Due to time constraints, an exhaustive test suite was not implemented. However, two popular Haskell
testing frameworks were researched.

QuickCheck is a property-based testing library. Relevant properties are expressed as predicates
and QuickCheck randomly generates test cases and asserts that each of those properties holds.
In the event a property fails to hold, QuickCheck is capable of searching through the space of
test cases to find a minimal instance that still fails.

Property-based testing is a natural fit for Haskell. Because programs tend to focus on pure
transformation of values by function application, the algebraic properties of those functions
are often more relevant than test cases that set up some pre-conditions, execute a procedure, and
then make an assertion about the post-conditions. Moreover, Haskell's purity by default means
that repeatedly running tests is safe and we can be confident that the same code will not
mysteriously stop working in a production environment.

Perhaps due to my inexperience with Haskell, it seemed like the bulk of my code was still
fundamentally impure (i.e., it was ultimately executed in the `IO` monad), which to my
knowledge makes QuickCheck unsuitable.

Another option is the HSpec library. In the vein of Ruby's RSpec, HSpec supports a testing
and development paradigm known as *behavior-driven development* (BDD). In BDD, requirements
are expressed in natural language and paired with an automated test that should prove the
requirement is properly implemented. HSpec defines a DSL for describing requirements and
related tests, including typical assertion-based tests and property-based tests. HSpec is
suitable for testing impure code and can run set-up procedures before executing each test,
e.g., resetting a database to some known state. Additionally, there is an experimental
library called HSpec-WAI that aids in describing the behavior of web application endpoints.

## Challenges

Haskell is, in many ways, a strange language. Going from writing PHP at work to
writing Haskell for this project, it seemed as though Haskell made a conscious effort to
do *everything* differently: the functional paradigm rather than imperative or object-oriented
code, a strict type system, lazy evaluation. Working with such a radically different
technology stack created a high barrier to productivity.

Frequently during the course of this project, I came across a task that I did not know
exactly how to accomplish. Often this was something that might have been easy to do
in PHP, although probably without Haskell's safety. However, with a bit of research or
experimentation, I found a solution and learned a lot in the process. In a school project,
this is only a positive. In an industrial setting, these delays would certainly be more
costly.

Another frustration that comes from my relative inexperience with functional programming
is the issue of code organization. In object-oriented programs, there exists a very basic
mechanism for organizing your code: the class. A data type and the operations on it are
kept together in a class definition. Haskell has a module system that can provide a 
similar organizational framework, but I found that I had a tendency to freely define functions
with much less attention to how they were organized. I suspect this would improve with
more experience and Haskell at least makes refactoring a safer proposition.

## Lessons Learned

Despite the frustrations described above, I found that working with Haskell was a great
development experience. I want to share one particular experience that really impressed
me.

Most of the way through the process of developing the API, it became clear that there 
were some pretty serious shortcomings with the codebase, particularly how it was
organized. There was too much code in the main file that was not relevant to bootstrapping
the application server. A file describing the domain model and auxiliary datatypes was
strateching into the hundreds of lines (which is a lot in a language as expressive as
Haskell). 

I decided to do a major refactor to imporve the situation. I moved a few definitions around,
essentially "sketching out" how the code would be organized. Of course, the project no longer
compiled. From there, I fixed compiler errors (there were a lot!) until the program compiled.
The really impressive part was that one it did compile, all of the API endpoints worked just
as they did before the refactor. 

In my experience with PHP, a refactor of this magnitude would have a similar project a
complete disaster. Coincidentally, we implemented a similar set of refactorings on a PHP
web API at my job at the same time I was working on this project and the contrast between
the experiences with those two code changes was profound. Seeing how pleasant refactoring
could be made struggling with PHP very frustrating!

I think that the biggest factor that makes refactoring in Haskell so pleasant is the
strong, static type system. PHP code frequently makes use of untyped associative arrays
and uses strings or integers to pass around options or flags. Even when using a feature
called *type hints* to be explicit about what data types a function parameter should
be, type mismatches are not reported until runtime. 

With Haskell, untyped associative arrays are replaced with strongly-typed record data
types and a set of flags can be encoded as a sum type. Moreover, all of this is checked
for consistency before the program even starts to run. With the Servant library and its
DSL for specifying the type-level definitions of your API, the compiler is able to make
even higher level guarantees about your code. I do not mean to be hyperbolic, but coming
from PHP, this is a revelation and makes me a bit envious!

## Future Work

- Front-end
- Expanded sorting, filtering, and related ("expand") functionality.

## References

1. "Type-level Web APIs with Servant" (http://www.andres-loeh.de/Servant/servant-wgp.pdf)
