# Posterchild

Type-safe PostgreSQL queries in quasi-quotes

## Introduction

This library operates in a similar problem space as `beam`, `esqueleto` and
`rel8`. The problem, in a nutshell: **type-safe SQL queries**. (See below for
an in-depth explanation of what we mean by that). We think that even though
types cannot catch *all* SQL errors, and even though the errors they can catch
depend on knowing the database schema at compile time, type-checking SQL
queries can still be massively useful.

However, we also believe that a programmer should not have to learn yet another
DSL in order to query a database - SQL is ubiquitous, and most developers will
already know it, so the ability to **write SQL as SQL** is an important design
goal of this library.

Another important goal is **useful error messages**. Type checking is not just
about detecting bugs or asserting correctness; it is also an important
productivity tool, and a good type checker should produce error messages that
guide the programmer towards understanding and solving the problem that
triggered it.

## Development Status

This library is currently still highly experimental, and will not be ready for
production use anytime soon.

Feel free to send suggestions and ideas, but please bear in mind the
experimental status of things.

## Type-Safe SQL Queries

Obviously we cannot have type-safe SQL queries in the sense that the types can
statically guarantee that our query will never be rejected or error - this
defies the laws of logic, because we simply cannot know at compile time the
runtime state of the database we will be talking to. If someone changes the
schema between us compiling our code and the code actually running, then there
is nothing we can do about that. If the database server goes down, there is
nothing we can do about that. If our application is configured to talk to a
different, incompatible, database, then there is nothing we can do about that.

But we *can* provide conditional type safety; colloquially: "*If* the database
we run against matches the schema declared at compile time, *then* this query
will be accepted by the server". So while we still cannot prevent all runtime
errors, we do signal logical programming errors such as type mismatches, typos,
incorrect assumptions about the schema, bugs due to incorrect usage of SQL,
etc.

We can, in principle, also guard against schema changes; however, this is
somewhat expensive and requires features that the library currently does not
have. The idea is simple: when connecting to the database server, query it for
the current schema, compare it to the schema we expect, and bail if it's not
compatible.

And finally, while in the ideal situation, we want to run our SQL type checks
statically, we may have to deal with dynamically constructed queries, so we
expose the parsing and type-checking machinery both as a compile-time mechanism
and as plain Haskell API that can be used at runtime.

## Writing SQL As SQL

Most existing libraries in this problem space expose an EDSL, designed to
mirror the structure of an SQL query as closely as possible, while at the same
time fitting Haskell's type system to the constraints of the represented
query. However, SQL cannot be made a subset of Haskell syntax, so any EDSL will
have to deviate from SQL syntax, which defeats our goal of writing SQL as SQL.

However, we cannot simply represent SQL queries as strings, because that would
mean that we can only scrutinize them at runtime, and we cannot get any static
guarantees for our queries - an incorrect query will fail at runtime.

GHC Haskell has two mechanisms that we could use to solve this dilemma:
**Type-level strings** (a.k.a. "Symbols"), and **Template Haskell**.

The former seems like the more elegant solution, but unfortunately, using
type-level strings means we have to do all our SQL parsing and type checking in
*type-level Haskell*; this is possible in theory, but in practice, even with
amazing libraries such as `singletons-th`, the ergonomics of this are pretty
bad, and there are a few caveats with type-level programming in Haskell that
make it difficult to produce good error messages. Type-level code also tends to
produce very long compilation times, and if it is complex enough, it can be
difficult to debug.

So instead, we use Template Haskell, providing a quasi-quoter that accepts an
SQL query string, and emits a function that represents the query, with a type
that (hopefully) accurately reflects the semantics of the query.

## Useful Error Messages

There is a pretty tough decision in this area. On the one hand, we might want
completely customized, human-friendly error messages, that state in plain
English what the problem is, e.g.:

> Schema does not have a column "toppings" in table "pizza".

OTOH, users of the library are most likely Haskell developers, and if our error
messages seamlessly integrate with the rest of GHC's errors, then that is
certainly also going to be valuable. Because of this, we will instead rely on
Haskell's typeclass machinery to state the constraints of a query, and leave
the actual error messages to the compiler, so instead of the above custom error
message, we would generate a constraint like this (simplified for explanatory
value):

```haskell
... :: SchemaHasTable schema "pizza"
    => TableHasColumn "pizza" "toppings"
    => DatabaseDriver
    -> Proxy schema
    -> {- input type -}
    -> IO {- output type -}
```
