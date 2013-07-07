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
- UNBOXED_INTS, UNBOXED_FLOATS
