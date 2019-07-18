---
title: Namespaces
---

TypeCobol doesn't support namespace for now. However we support to add `Program` as a qualifier.

Example:
With the following program

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.
       DATA DIVISION .
       working-STORAGE SECTION.
       01 Person TYPEDEF STRICT.
         05 Id         pic 9(05).
         05 lastName   pic X(30).
         05 FirstName  pic X(30).
         05 BirthDate  pic X(08).
       PROCEDURE DIVISION.
       declare procedure isDateValid private
            input    date1  type Date
            output   result type Bool.
       procedure division.
      *     code of the procedure here...
            set result to true
            .
       end-declare.
       END PROGRAM PGM1.
```

You can write:

```cobol
01 myData type PGM1::Person.

call PGM1::isDateValid input myDate
```
