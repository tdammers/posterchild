# Ideas & Wishes

## Better Errors

Inject custom errors to improve error messages further.

## Testing I

Various possible solutions for testing "will the compiler accept / reject
this":

- Use GHC API
- Run a persistent GHCi process in the background
- Use deferred type checks
- Something something HLS?

## Testing II

Using an actual PostgreSQL database as an "oracle" to verify the type checker:

- Generate a query
- Ask the type checker whether it's OK or not
- Run it against a database to verify the type checker's verdict

Caveat: we will allow some queries that may error out, and we will reject some
queries that would run just fine, so the generator needs to be a bit
opinionated to avoid those cases.

## Composable Query Fragments

Use case scenario: compose a well-typed query from well-typed fragments, using
typesafe combinators. Query fragments should be type checked at compile time,
even though the combining may be dynamic.

This kind of thing comes naturally with an EDSL approach like Beam's, but may
prove to be too difficult to fit into this TH-based design.

A typed EDSL on top of the current (largely untyped) AST might be a viable
approach here, though it would still require replicating some of the type
checking logic in type-level Haskell.

Food for thought.

## Example Of Reusing Query Across Schemas

Compiled queries are type-checked without looking at the schema; the demands on
a schema that a query is valid for are encoded as constraints. This means that
the same query function can be run against multiple schemas; add an example of
this to the documentation.
