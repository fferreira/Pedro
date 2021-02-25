# Proposal for the syntax of Pedro files


Line comments start with `(*)` and block comments are surrounded by `(* ... *)`

Then a file contains entries for tokens, places, transitions, and arcs. Each entry is terminated by a full stop (.).


```
(** Tokens **)

(*) Tokens declare a name in a sort.

token token1:sort1 token2. (* Here token1 is of sort1, while if we omit the sort then the token has the same sort as its name *)
(*) the previous line declares token1 of sort sort1 and token2 of sort token2.

token:msg m1 m2 m3 m4. (* if we specify the sort at the beginning all the tokens get that sort *)

(** Places **)

(*) Just a name followed by its initial marking,
(*) which may be empty but it has to appear.


place name1 []. (*) a place with no initial marking
place name2 [token1 token3]. (*) a place with two tokens
place name3 [token1 token3^2]. (*) a place with three tokes where 'token2' appears twice

(*) many places can be declared in one line
(*) the initial marking is required
place name1[] name2[] name3[token1 token2^3].

(*) note: the notation "^n" where n is a natural number,
(*) means that we have n copies of a token.

(** Transitions **)

transition name1 name2 name3.

(*) Transitions simply have a label for now

(** Arc **)

(*) Arcs relate places and transitions and have associated a multiset of tokens.

name>->name[token1 token2^3]

(*) Also we can specify a complete arch in the following way:
name>->name[token1]>->name[token]...
```

Note that arcs never relate places to places or transitions to
transitions. When an arc relates a place to a transition the token
multiset is the required tokens to activate the transition, and in the
converse situation the resulting tokens.

These entity declarations can appear many times in a file and can
appear in any interleaving, as long as they only refer to entities
that appear before them (e.g.: an arc only connects an already defined
place to an already defined transition).

A name is any sequence of characters separated by a space, they can
include spaces if they are surrounded by "". They cannot contain
quotes.

In the lexical structure of names symbols [] "" cannot take part in
it.
