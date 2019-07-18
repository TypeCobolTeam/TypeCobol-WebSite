---
title: UNSAFE clause
---

## TypeCobol / Cobol 2002 rules

In Cobol 2002 a `Move` with a strongly/strictly typed receiver implies that the sending identifier is declared with the same type.

In some case it is however necessary to initialize a group item, to do this a new option of the 'MOVE' statement' will be introduced in TypeCobol to authorize the MOVE. This should be used in limited statements (otherwise it is better to declare identifier-2 without the Strong/Strict property).

## TypeCobol rules

```cobol
move unsafe identifier-1               to identifier-2
move unsafe literal-1                  to identifier-2
```

The unsafe keyword inhibit the strict/strong type check.

- If receiver (identifier-2) of a move references a strongly-typed group item, sender (identifier-1) can be described without a type or with a different type
- If sender (identifier-1) is described as a group-item of the same type or if receiver (identifier-2) is not described as a strongly-typed group item, then a warning should be issued to specify that in this case the **unsafe** keyword is useless
