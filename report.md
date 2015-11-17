# Inventorium: A RESTful Inventory Management API in Haskell

## Description and Motivation

During the Spring 2015 semester, Dr. Hayes introduced our software engineering class
to XXXX and XXXX, who are involved with managing technology for Powell County Schools.
In an experiment in software team management, the class organized into subgroups to design
and implement a single software package to aid in tracking the significant inventory of
desktop computers, laptops, tablets, and other pieces of technology that XXXX and XXXX
were tasked with managing.

For various reasons, the project was unsuccessful. However, being from Eastern Kentucky myself,
I was interesting in contributing to the project and seeing something really useful delivered
to the end users in Powell County.

During our software engineering course, my subgroup was primarily responsible for the design of
the application and, later, the management of the rest of the development subteams. Our design
was a typical three-tiered architecture:

- a browser-based client;
- a web API;
- a relational database.

We made specific technical decisions based on the realities of the large team. Technology
choices were based on experience and language accessibility. PHP was chosen to implement the
web API in PHP, MySQL for the database, and a combination of Twitter Bootstrap and Backbone.js
were used to implement the browser-based client.

Perhaps the most successful technical choice in the initial design was the use of virtual
machines to provide a consistent, reproducible development environment for the team.

These technologies were, for the most part, ones that I was very comfortable with. In my
day-to-day work doing web application development, the so-called LAMP stack (Linux, the
Apache web server, MySQL, and PHP) is very common. This was useful, since I would be able
to answer any questions that came up in the process.

However, that same work experience has taught me that many aspects of this technology stack
are woefully inadequate. For this reason, I chose to reimplement my team's same design using
a rather different technology stack, which we will discuss shortly.

Simply due to time constraints, the Inventorium project remains unfinished. XXXX MORE

## Architecture

As I said, the notion of using virtual machines for development was really the most successful
part of the original project. With surprisingly few issues, around 20 students were able to
start contributing code to the project despite different operating system configurations and
and experience with the technology stack.

However, the environment we used has a potentially serious drawback. It consisted of a single
VM that contained both the web server, which would be responsible for serving both the web 
API and static files (HTML, JavaScript, and CSS) for the client, and the database server.
Looking forward, this would clearly be problematic for scalability.

Ideally, the API server, the web server, and the database server should all be separate to
add flexibility if the architecture required say multiple API servers with load balancing or
database replication. However, managing several VMs independently would be troublesome. 

I chose to solve this problem using a technology called Docker. Docker is a so-called
"containerization" platform. Essentially, individual services (an API server, a database, etc.)
can be packaged as containers: lightweight, bare-bones VMs that can (but are not required to)
run on the same host machine. Containerized services are completely isolated, eliminating
issues where an installed dependency for your database server is incompatible with your web
server. The containers can still communicate over the network by explicitly mapping network
ports in the containers.

Inventorium, then, requires a container for the static web server, which was configured to
forward API requests to a container for the API server, which itself communicates with the
database server. Although this was not implemented, there is very good support for deploying
containers to platforms like Amazon Web Services. This is an advantage, because it would allow
Powell County Schools to make use of Inventorium cheaply, without needing a dedicate server,
and avoiding potential issues with deploying to some shared server in the IT office.

The web server container runs the Nginx web server. The Apache web server uses a OS-level
threads to handle each web request, which introduces quite a bit of overhead for each request.
This is less important when each request requires a great deal of CPU time (e.g., when 
responding to a complex API request), but when handling many relatively simple requests
(for static files or simply proxying requests to an API server), Nginx's single-threaded
architecture is more efficient: high throughput and responsiveness are achieved through
an asynchronous event-based architecture. Since the heavy lifting occurs on the API
server container, 

The database container runs the PostgreSQL relational database management system.  MySQL
is an extremely common choice in the web application world and has made improvements in
terms of standards compliance, transactional safety, and data integrity, points where
PostgreSQL was traditionally better.  One potential area where PostgreSQL might have been
useful is its ability to store and index document-based data in the form of JSON data,
however I did not choose to use that in this project. In other words, the choice to use
PostgreSQL was a toss-up and made little difference.

Finally, and perhaps most interestingly, the web API server was written in Haskell, a strong-
and statically-typed pure functional language that compiles to native code. Haskell's
pedigree is decidedly academic and it is often derided as being overly theoretical,
but as I will discuss below, it has a number of features that make it great for what
is often called "line of business" software and not just writing compilers or experimenting
with type systems and programming language theory.

## The Web API

Our goal when designing a web API is to expose the operations of the inventory management domain over HTTP.
We use an architecture called REST, Representational State Transfer, to accomplish this. Described by
Roy T. Fielding in his 2000 PhD dissertation, REST is an architectural style that emphasizes building
around the fundamentals of the web: HTTP and hypermedia (i.e., navigable links between entities).

In a RESTful architecture, domain objects are *entities* (in the HTTP sense), that is, resources
addressed by URLs. They may be available in many representations: an image resource might be available
in a JPEG representation or PNG. HTTPs methods (verbs like `GET` and `POST`) describe the operations
available. A `GET` request is a request for a representation of a resource, i.e., a read operation.
A `POST` request takes a representation of an entity and requests that the application store it,
i.e., a create (or update) request.

Other aspects of the API are all handled through the well-defined channels of HTTP. Options and
parameters can be specified using URL query string parameters and/or HTTP headers. Similarly,
errors are reported via HTTP status codes. If a user requests an entity that does not exist, 
the result should be the (well-known) 404 response. If the entity exists but the user requests
an representation the server does not know how to provide, you instead get HTTP's "406 Not Acceptable"
error.

REST can be a restrictive architecture, but its principle of offering a "uniform interface" to APIs
is powerful. For example, HTTP defines `GET` requests to be idempotent or safe operation, meaning that
a `GET` request should not change the state of an entity on the server. Thus, if an application
properly implements the REST architecture, you get caching of `GET` requests for free via the web's
standard cachine mechanisms.


- REST
- Description of domain and rough outline of endpoints/manipulations.
- Pointer to full documentation

## Implementation

- Why Haskell
    - Type safety!
    - Expressiveness

### Libraries

- Servant
    - Expresses core REST concepts directly as types
    - Types drive compile-time safety in implementation
    - Same types also drive adjunts: documentation generation, client generation
    - Marshalling between native datatypes and JSON is easy
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

- ???
- (Testing harness client built via Servant's client generation)

## Challenges

- High barrier to entry to be productive
- Doing things right opens up can of worms 
- OOP has a built-in mechanism for code organization: the class. Code exists near the data it operates on.
  This is not necessarily true in a functional language, so code can become disorganized quickly.

## Lessons Learned

- The power (and strictness) of Haskell's type system is immensely powerful. Massive refactor, involving
  every file in the project: took a while to make all changes satisfy the compiler, but when it finally
  compiled again, everything just worked.

## Future Work

- Front-end
- Expanded sorting, filtering, and related ("expand") functionality.
