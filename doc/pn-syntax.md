# Proposal for the syntax of petri net files


Line comments start with `(*)` and block comments are surrounded by `(* ... *)`

Then a file contains entries for tokens, places, transitions, and arcs.


```
(** Tokens **)

(*) Just a list of names

token token1 token2 token3

(** Places **)

(*) Just a name followed by its initial marking,
(*) which may be empty but it has to appear.

place name [token1 token3]

(*) many places can be declared in one line
(*) the initial marking is required
place name1[] name2[] name3[token1 token2^3]

(*) note: the notation "^n" where n is a natural number,
(*) means that we have n copies of a token.

(** Transitions **)

transition name1 name2 name3

(*) Transitions simply have a label for now

(** Arc **)

(*) Arcs relate places and transitions and have associated a multiset of tokens.

name>->name[token1 token2^3]

(*) finally we can specify a complete arch in the following way:
name>->name[token1]>->name[token]...
```

Note that arcs never relate places to places or transitions to transitions. When an arc relates a place to a transition the token multiset is the required tokens to activate the transition, and in the converse situation the resulting tokens.

These entity declarations can appear many times in a file and can appear in any interleaving, as long as they only refer to entities that appear before them (e.g.: an arc only connects an already defined place to an already defined transition).

A name is any sequence of characters separated by a space, they can include spaces if they are surrounded by "". They cannot contain quotes.

In the lexical structure of names symbols [] "" cannot take part in it.
