# VIT
## Blackboard summary
### Goal
Allow a team to develop synchronously a software, without having merge conflict every `git pull`.

### How to do it ?
####  Use our own programming language
A functional one, to allow us to think about types simply. Certainly a kind of MiniMl, see #3.

#### Develop an abstract patch language
We don't want to share *non-useful-information* (like indentations...), we can't use traditional patch format.

A VIT patch is an abstract structure, like:
```Ocaml
type ('a, 'b) either = Left of 'a | Right of 'b
module
  type t (* The patch itself *)
  type helper (* To recover from a patch application failure *)

  (* From two states of the same program, infer a list of patches of type t *)
  val infer : prog -> prog -> t list
  (* We can also imagine a dual to helper for infer *)

  (* Given a proram and a patch, try to apply it. The result is either a program or a helper, aiming to help to recover from this failure *)
  val apply : prog -> t -> (prog,helper) either

  (* Given a helper, try to recover from the failure *)
  val use_helper : prog -> helper -> (option context -> option prog) -> helper

  (* Serialize *)
  val serialize : t -> string

  (* Deserialize *)
  val deserialize : string -> t
end
```

For example, we can imagine the following patch types:

* Addition
* Deletion
* alpha-conversion
* commutativity of arguments
* ...

#### The workflow
Then the workflow is similar to:

* Code
* Commit:
  * Pull from the server
  * Verify that we are up to date
  * Infer patches
* Send patches

Dually, we:

* Receive a patch
* Try to apply it:
  * If we fail, ask the user for help!

### Server
There is a server centralizing a _canonical_ list of VIT patches, for now stored in git repository.

The client can download this list to produce the source code. Then users can have different source codes, but all have the same list of VIT patches. We indeed just want to ensure the property that all codes have the same semantic.

#### Validate
This will certainly be dependent of who sent the patch, and evaluate it the code is well typed, if it compile, or maybe if it pass through user-defined tests.

### A layer over git ?
We want to share code, and `git` allow us to do it simply. At least, we need to import/export to a proper git repository, but can we make everything to work over it?
-> See the Server part.

### Basic case
Imagine I am working over a simple project, and I am committing **only** a new top level declaration (like a new function). How VIT will handle it?
