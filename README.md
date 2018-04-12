# VIT
## Blackboard summary
### Goal
Allow a team to develop synchronously a software, without having merge conflict every `git pull`

### How to do it ?
####  Use our own programming language
A functional one, to allow us to think about types simply. Certainly a kind of MiniMl, see #3 

#### Develop an abstract patch language
We don't want to share *non-useful-information* (like indentations...), we can't use traditional patch format.

Also, a patch is often a *living thing*, others developpers will want to change a line or two before including it.
So we have to develop a strategy about a patch life: Someone write a change, send it over the network, and others rate it, or apply minor changes, and then approve it. And of course including this strategy in a VIT's patch life.

##### The merge problem
How do we solve the merge problem:
* If two patchs are *othogonal*, it means that they don't change the same part of the code, then they commute, and we can add it in any order
* Otherwise, it is more complicated. Do we ask help to the user ? Or try something like `darcs` ?

##### Describe change's implications
Imagine I am renaming a function. I can "annotate" the concerned line with a tactic to solve a merge issue (like `rename f() in g()`) and let VIT handle this for us.

#### A layer over git ?
We want to share code, and `git` allow us to do it simply. At least, we need to import/export to a proper git repository, but can we make everything to work over it ?
-> See the Server part.

#### A text editor ?
Because do not want VIT to stop a programmer during its work for a conflict if he don't need the concerned code right now, we have to interract with him. A common place to do it is within an IDE.

### Random Ideas
To use a descendant way-to-think, we started to think about something like
```
AST ast = init()
startThread:
  while notEnded:
    Patch patch = receive()
     if(validate(patch))
       ast = apply(ast, patch)
  endSession(ast)

startThread:
  while notEnded:
    Patch patch = commit()
    if(validate(patch))
      send(patch)
```

### Server
A centralized server, but everyone has a local clone of the depo (à la git)

First approximation: One server per project, one file per project

A server can receive different patches:

* A low-level patch, like a simple addition, for which VIT can infer the nature of the patch
* A high-level patch otherwise

The server will certainly delegate the user gestion, and low-level things, to gitolite.

#### Validate
This will certainly be dependent of who sent the patch, and evaluate it the code is well typed, if it compile, or maybe if it pass through user-defined tests.

### Basic case
Imagine I am working over a simple project, and I am committing **only** a new top level declaration (like a new function). How VIT will handle it ?
