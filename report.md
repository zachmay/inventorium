# Inventorium: A RESTful Inventory Management API in Haskell

## Description and Motivation

## Architecture

Full description of web/API/DB stack, noting that the front-end is not implemented, but how it would be
served in this architecture.

- Architecture description
    - Application/API server
    - Web server
    - DB server
- Docker
    - Development story
    - Deployment story

## The Web API

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
