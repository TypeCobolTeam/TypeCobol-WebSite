---
title: Functions and procedure in a Nutshell
---

## Simple example

```cobol
identification division.
program-id. MyProgram.
data division.
working-storage section.
01  myDate1 TYPE Date.
01  myDate2 TYPE Date.
01  numberOfDays pic 9(05) comp-5.

procedure division.

-Procedure to calculate the number of days between 2 dates
declare procedure daysBetween private
                       input   date1 type Date
                               date2 type Date
                       output  nbOfDays pic 9(05) comp-5.
procedure division.
-   The details of the calcul is not detailled here...
    move result to nbOfDays
    .
end-declare.


-Call your procedure
    call daysBetween input  myDate1
                            myDate2
                     output numberOfDays
    goback
    .
 end program MyProgram.
```

## Advantages of function/procedure

By comparison against a standard Cobol 85 program/nested program

- Syntax shorter than a nested program or a program
- Name your function with more than 8 characters
- Specify your input/output parameters
- Control your arguments (input/output) at compilation time
  -- control the number of arguments and their types
  -- control the size of parameters (2017)
- Propose visibility mechanism (private and public)
- Have the return code as an implicit parameter (2017)
- Check if caller test the return code (2017)
- Function chaining (2017/2018)
