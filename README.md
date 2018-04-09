# VIT
## Blackboard summary
### Goal
Allow a team to develop synchronously a software, without having merge conflict every `git pull`

### How to do it ?
####  Use our own programming language
A functional one, to allow us to think about types simply. Certainly a kind of MiniMl, see #3 

#### Develop an abstract patch language
We don't want to share *non-useful-information* (like indentations...), we can't use traditional patch format.

#### A layer over git ?
We want to share code, and `git` allow us to do it simply. At least, we need to import/export to a proper git repository, but can we make everything to work over it ?

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

#### Validate
This will certainly be dependent of who sent the patch, and evaluate it the code is well typed, if it compile, or maybe if it pass through user-defined tests.

### Basic case
Imagine I am working over a simple project, and I am committing **only** a new top level declaration (like a new function). How VIT will handle it ?
