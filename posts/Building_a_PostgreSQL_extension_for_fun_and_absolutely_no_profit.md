A few months ago, [I built my first PostgreSQL extension, `pg_ddate`](https://github.com/hellerve/pg_ddate/).
I mostly wanted to learn how PostgreSQL extensions work, and make a simple
but feature-complete extensions with custom data types, operators, and
all the bells and whistles.

It worked quite well, and was extremely fun, so I decided to write up a
tutorial for all of you who want to do something like this yourselves!

For the purposes of this blog post, I assume that you’re at least somewhat
comfortable with both C and SQL. We’ll explore why extensions are fun and
necessary, and build a loadable extension together that defines a new data
type and registers it and its operators, as well as a custom helper function
to get a full picture. I also won’t get into what a Discordian date is, so
[you might need a quick primer](https://en.wikipedia.org/wiki/Discordian_calendar).
It is the obviously correct data system.

A quick note on development setup: I use Docker for development convenience,
and the one I came up with [can be found in the repo](https://github.com/hellerve-project-based-learning/pg_ddate/blob/main/Dockerfile).

## What are extensions for, exactly?

[The official PostgreSQL documentation on extensions](https://www.postgresql.org/docs/current/extend-extensions.html)
gives a little bit of a primer on why extensions are useful and sometimes
required, but it’s mostly focused on the technical bits, so I thought I’d
briefly explain what an extension is and why we’d use one..

Extensions are packaged C-and-SQL artifacts that extend PostgreSQL with
functions and data types. While SQL itself is geared toward defining data
structures that fit well into a tabular structure, some types require different
handling. A custom date type like a Discordian date is a prime example. We want
these dates to behave exactly like normal date types, including printing nicely,
and being orderable and castable to and from regular dates. This is probably
achievable in SQL, but packaging a new C data type is a bit easier, and it
allows us to ship all the algorithms around the data type in a language that
is more geared toward “regular” procedures than SQL.

Other extensions might not even ship custom types and instead be focused around
custom algorithms that are not easily expressible in SQL. In a nutshell,
extensions give us the raw algorithmic power of C underneath our relational
abstraction.

## Building an extension

Now that we’ve talked a bit about what an extension is and why we might want
one, let’s talk about how to build it. We will start with the project structure
that PostgreSQL imposes on us, and then gradually fill it all out with code.

### Making a scaffold

Conceptually, we need three files<sup><a href="#1">1</a></sup>: a C file, a
SQL file, and a control file.

The control file just defines a bunch of metadata and can be largely
copy-pasted and forgotten about:

```
comment = 'Discordian date type'
default_version = '1.0'
module_pathname = '$libdir/pg_ddate'
relocatable = true
```

The C and SQL files are where the magic happens. The SQL layer is largely
responsible for wiring up the types, functions, and operators that we define.
For now, we will only define an empty type inside it:

```
\echo Use "CREATE EXTENSION pg_ddate" to load this file. \quit

CREATE TYPE ddate;
```

The C side defines the meat of the extension. A scaffold for it could look
like this:

```
// these includes are all defined by postgres
#include "postgres.h"
#include "fmgr.h"
#include "utils/builtins.h"
#include "utils/date.h"
#include "utils/datetime.h"
#include "utils/timestamp.h"
#include "libpq/pqformat.h"

#include <time.h>
#include <math.h>

PG_MODULE_MAGIC;

typedef struct {
    int32 julian_day;
} DDate;
```

We do not know how to pass things around yet, but we do have all the pieces in
place to get started.

### Defining a type

To define a full type in our extension, we will need to use [`CREATE TYPE`](https://www.postgresql.org/docs/current/sql-createtype.html).
`CREATE TYPE` takes a whole range of different inputs, but for our purposes
we mostly need to tell PostgreSQL how to convert the type to and from text
(`input` and `output`) and binary (`receive` and `send`), as well as the size
and alignment of our type.

The SQL to do so looks like this:

```
CREATE OR REPLACE FUNCTION ddate_in(cstring)
RETURNS ddate
AS 'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

-- ... do the same for ddate_out, ddate_recv, and ddate_send ...

CREATE TYPE ddate (
    internallength = 4,
    input = ddate_in,
    output = ddate_out,
    receive = ddate_recv,
    send = ddate_send,
    alignment = int4,
    storage = plain
);
```

This tells PostgreSQL that the length of the type is four bytes, its alignment
and storage type, and wires up the conversion functions.

So, how do these functions look like in C? Let’s start with the string functions:

```
PG_FUNCTION_INFO_V1(ddate_in);
Datum ddate_in(PG_FUNCTION_ARGS) {
    char *str = PG_GETARG_CSTRING(0);
    DDate *result;

    result = (DDate *) palloc(sizeof(DDate));

    result->julian_day = ddate_parse_from_str(str);

    PG_RETURN_POINTER(result);
}

PG_FUNCTION_INFO_V1(ddate_out);
Datum ddate_out(PG_FUNCTION_ARGS) {
    DDate *ddate = (DDate *) PG_GETARG_POINTER(0);
    char *result;

    result = (char *) palloc(128);
    ddate_write_to_str(ddate, result);

    PG_RETURN_CSTRING(result);
}
```

The main bits that are interesting here is that we register the function using
`PG_FUNCTION_INFO_V1`, and then adhere to the signature PostgreSQL expects: we
return a `Datum` and take in `PG_FUNCTION_ARGS`. We get to the actual arguments
by using the accessors provided by PostgreSQL (`PG_GETARG_CSTRING` and
`PG_GETARG_POINTER` in the above). When we return values, we wrap those in
similar macros. This might look scary at first, but it’s actually very little
ceremony for a foreign function interface, and it reads pretty clearly!

The binary converters look quite similar:

```
PG_FUNCTION_INFO_V1(ddate_recv);
Datum ddate_recv(PG_FUNCTION_ARGS) {
    StringInfo buf = (StringInfo) PG_GETARG_POINTER(0);
    DDate *result;

    result = (DDate *) palloc(sizeof(DDate));
    result->julian_day = pq_getmsgint(buf, sizeof(int32));

    PG_RETURN_POINTER(result);
}

PG_FUNCTION_INFO_V1(ddate_send);
Datum ddate_send(PG_FUNCTION_ARGS) {
    DDate *ddate = (DDate *) PG_GETARG_POINTER(0);
    StringInfoData buf;

    pq_begintypsend(&buf);
    pq_sendint(&buf, ddate->julian_day, sizeof(int32));

    PG_RETURN_BYTEA_P(pq_endtypsend(&buf));
}
```

The main difference here is that we need custom functions to actually send and
receive things, but if we squint it’s not too bad. As a note: these two
functions aren’t strictly necessary, but are required if we want our data type
to support the binary protocol.

And just like that we have a fully functional data type!

### Adding functions

Now we’re ready to add functions. We’ll start with a normal one, then we’ll
add custom operators and casts to complete the picture.

Let’s start with `ddate_now()`, a function we can call in SQL to get the
current discordian date. In SQL, this is a trivial addition:

```
CREATE OR REPLACE FUNCTION ddate_now()
RETURNS ddate
AS 'MODULE_PATHNAME'
LANGUAGE C STABLE;
```

We just tell SQL that a function with that name exists, and we are done!

The C side is not much scarier:

```
PG_FUNCTION_INFO_V1(ddate_now);
Datum ddate_now(PG_FUNCTION_ARGS) {
    DateADT current_date;
    DDate *result;

    current_date = GetSQLCurrentDate();

    result = (DDate *) palloc(sizeof(DDate));
    result->julian_day = current_date + POSTGRES_EPOCH_JDATE;

    PG_RETURN_POINTER(result);
}
```

We utilize the fact that we can already get the current date in PostgreSQL’s
C API, and just get the date from there. A relatively easy addition, all in
all.

Now that we’re practiced extension writes, let’s do some more interesting
things.

#### Customizing our operators

Let’s add an equality operator and go from there. In SQL:

```
CREATE OR REPLACE FUNCTION ddate_eq(ddate, ddate)
RETURNS boolean
AS 'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

CREATE OPERATOR = (
    leftarg = ddate,
    rightarg = ddate,
    procedure = ddate_eq,
    commutator = =,
    negator = <>,
    restrict = eqsel,
    join = eqjoinsel,
    hashes,
    merges
);
```

We only need to register an equality function and we are able to register our
operator. Quite nice!

C is even easier, facilitated by the fact that we’re dealing with comparable
types already:

```
PG_FUNCTION_INFO_V1(ddate_eq);
Datum ddate_eq(PG_FUNCTION_ARGS) {
    DDate *date1 = (DDate *) PG_GETARG_POINTER(0);
    DDate *date2 = (DDate *) PG_GETARG_POINTER(1);

    PG_RETURN_BOOL(date1->julian_day == date2->julian_day);
}
```

All we need to do is get the date arguments and compare them! This means that
defining operators, while sounding scary, becomes mostly busy work. We need to
do this somewhat boring work for all operators separately. Once we have that,
however, we can even register our type as an operator class for b-tree:

```
CREATE OPERATOR CLASS ddate_ops
DEFAULT FOR TYPE ddate USING btree AS
    OPERATOR 1 <,
    OPERATOR 2 <=,
    OPERATOR 3 =,
    OPERATOR 4 >=,
    OPERATOR 5 >,
    FUNCTION 1 ddate_cmp(ddate, ddate);
```

The most interesting operator is probably `~`, which performs regex match.
Check it out if this blog post has whet your appetite ([C](https://github.com/hellerve-project-based-learning/pg_ddate/blob/4338b04a3a6fd23ecb0902ab5fbc001c75221122/pg_ddate.c#L310-L327),
[SQL](https://github.com/hellerve-project-based-learning/pg_ddate/blob/4338b04a3a6fd23ecb0902ab5fbc001c75221122/pg_ddate--1.0.sql#L159-L170)).

#### Adding casts

Finally, we define some casts from dates to discordian dates. Let’s start with
C this time, since it contains all things you know already:

```
PG_FUNCTION_INFO_V1(date_to_ddate);
Datum date_to_ddate(PG_FUNCTION_ARGS) {
    DateADT date = PG_GETARG_DATEADT(0);
    DDate *result;

    result = (DDate *) palloc(sizeof(DDate));
    result->julian_day = date + POSTGRES_EPOCH_JDATE;

    PG_RETURN_POINTER(result);
}

PG_FUNCTION_INFO_V1(ddate_to_date);
Datum ddate_to_date(PG_FUNCTION_ARGS) {
    DDate *ddate = (DDate *) PG_GETARG_POINTER(0);
    DateADT result;

    result = ddate->julian_day - POSTGRES_EPOCH_JDATE;

    PG_RETURN_DATEADT(result);
}
```

At this point, all of this should be familiar to you. The functions follow the
usual structure, the only new additions are `PG_*_DATEADT`, which get and
return date types, respectively.

On the SQL side, the addition is also quite simple:

```
CREATE CAST (date AS ddate) WITH FUNCTION date_to_ddate(date) AS IMPLICIT;
CREATE CAST (ddate AS date) WITH FUNCTION ddate_to_date(ddate) AS IMPLICIT;
```

We tell PostgreSQL how to cast dates to ddates and vice versa, and we allow
for implicit casts, type safety be damned.

And that’s it! You’ve just built a full data type for PostgreSQL, including all
manner of quite advanced functionality such as b-tree indexing, casts, and
operators!

## Fin

I hope this post made it clear to you just how delightful I find the PostgreSQL
extension API. It is one of the least intrusive APIs that doesn’t get in the
way while being powerful and straddling the gap between the higher and lower
levels of the database effortlessly. Honestly, I was quite in awe.

The truth of the matter, however, is that most of us will never or very rarely
need to write PostgreSQL extensions, so we won’t get to play with the API.
Truth be told: most of the time it’s probably a bad idea. I hope this blog
post comes in handy for you in the rare cases where it isn’t.

#### Footnotes

<span id="1">1.</span> In reality, we also need a Makefile that uses
[PGXS](https://www.postgresql.org/docs/current/extend-pgxs.html), but I will
omit that for the purposes of this post, since building the extension is a minor
concern. If you are truly interested, check [the Makefile in the repository](https://github.com/hellerve-project-based-learning/pg_ddate/blob/main/Makefile).
