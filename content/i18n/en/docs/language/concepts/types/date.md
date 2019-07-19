---
title: Date
---

## Type Date

`Date` is an intrinsic type of TypeCobol. A Date use the format `YYYYMMDD`.

You can use the type Date just like any other TypeCobol type.

```cobol
01 Identifier Type Date.
```

The Date type is defined like this:

```cobol
01 Date Typedef strong.
  05 YYYY  pic X(04).
  05 MM    pic X(02).
  05 DD    pic X(02).
```

## Technical note

`Date` is a Cobol 85 keyword. However we wanted to reuse this as a type name because the word `Date` just fit perfectly for our need.
We didn't want to name our type `TCDate` or something like that.
