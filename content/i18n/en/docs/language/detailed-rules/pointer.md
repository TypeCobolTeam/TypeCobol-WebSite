---
title: Pointer
---

## The need

In Cobol 85, pointers cannot be incremented or decremented by a numeric variable (like in C for example).

## The solution

TypeCobol allows you to use the syntax :

```cobol
SET MyPointer UP BY n
SET MyPointer UP BY 1
SET MyPointer DOWN BY n
SET MyPointer DOWN BY 1
```

Where:

- `MyPointer` is a `pointer`
- `n` is a numeric variable

### Code generation

If a pointer is used that way, the code generation must add a redefines to the pointer like this:

```cobol
01 MyPointer pointer.
01 MyPointer-HASH redefines MyPointer pic 9(05) comp-5.
```

## Detailled rules

- [ ] **POINTER_SET_UP_BY** standard pointer of Cobol 85 declared in level 01 to 49 can be used in instruction `SET UP BY` and `SET DOWN BY` like index.
  - Forbid this syntax for pointer declared in level 77
- [ ] **POINTER_CGEN_DECLARATION** if a pointer is used in a `SET UP/DOWN BY`, then its declaration is redefined.
  - The name of the redefines, is the 22 first characters of the pointer followed by a hash of 8 characters of the unique name of the pointer. This name is named `redefinedPtrName`.
  - The picture clause is `pic 9(05) comp-5`
- [ ] **POINTER_CGEN_USAGE** the instruction `SET ptr UP BY n` is translated to `ADD n to redefinedPtrName`
