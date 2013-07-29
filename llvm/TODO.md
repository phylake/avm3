Proof of Concept
================
- prop up PoC with boxed ints
- handcraft needed bootstrapping

Bootstrap
=========
- parse multiname to map multiname index to function address
 - use a naming convention: {namespace}_{class}_{function}
  - namespace is important because i can define a public class int {} in a different package
- bootstrap class globals
 - hook up multiname/function pointer arrays
- need to setup class definition globals to support new_int()
- pass-by-value classes need malloc and sizeof

Runtime
=======
- consider writing out all variables as accessor pairs so multiname-function map can continue to be leveraged
- add/subtract int   may be polymorphic if every object has toInt()
- add/subtract float may be polymorphic if every object has toNumber()
- UNBOXED_INTS, UNBOXED_FLOATS
- Class Sizeof, instance Sizeof D
- GC? memory manager?

Misc
====
- DSL for LLVM bitcode authoring (data D by itself isn't cutting it)
