---
title: TYPEDEF & TYPE clauses (from Cobol 2002)
---

#### Table of Contents

- [Introduction](#Introduction)
- [Syntax](#Syntax)
- [General Rules](#General-Rules)
- [Strongly-typed and Strictly-type rules](#Strongly-typed-and-Strictly-type-rules)
- [Strongly-typed rules](#Strongly-typed-rules)

<a name="Introduction"></a>

## Introduction

`TYPEDEF` can be used to create user-defined data types.
User-defined data types are elementary or group items defined in `WORKING-STORAGE`, `LOCAL-STORAGE`, `LINKAGE` or `FILE` sections.
These type definitions act like templates that can be used, using the `TYPE` clause, to define new data items.

<a name="Syntax"></a>

## Syntax

**Format : `TYPEDEF` clause**

```cobol
TypeName TYPEDEF (STRICT | STRONG) (PRIVATE | PUBLIC)
```

Use of the defined user types is done with TYPE clause.
This statement can be used at any level of data entries. If the TYPDEF is a group description, all level numbers of subordinate items are adjusted.

**Format : `TYPE` clause**

```cobol
TYPE TypeName
TYPE ProgramName::TypeName
```

Example of TYPEDEF and TYPE use :

```cobol
01  GROUPE-TYPE TYPEDEF STRICT.
    03  ITEM-1 PIC X.
    03  ITEM-2 PIC 9.


01  GROUPE-WS1.
    05  ITEM-WS1 PIC X(10)
    05  GROUPE-WS2 TYPE GROUP-TYPE.

* will be interpreted as
*
        01  GROUPE-WS1.
            05  ITEM-WS1 PIC X(10)
            05  GROUPE-WS2.
                06  ITEM-1 PIC X.
                06  ITEM-2 PIC 9.
*
```

## General Rules

General rules that applies to all kind of types: Weak, Strict and Strong

1\. **TCTYPE_TYPEDEF_AFTER_NAME** `TYPEDEF` clause must immediately follow type-name.
_(source page 290 of ISO Cobol 2014)_

i.e.

```cobol
   01 NEW-TYPE TYPEDEF PIC 9(2).  >> is correct
   01 NEW-TYPE PIC 9(2) TYPEDEF.  >> is incorrect
```

2\. **TCTYPE_TYPEDEF_ONLY_LVL01** `TYPEDEF` clause can only be used for level `01` entries
_(source page 290 of ISO Cobol 2014)_

```cobol
   01 NEW-TYPE TYPEDEF.                  >> is correct
       05 NEW-TYPE-PART1 PIC 9.
   01 NEW-TYPE.                          >> is incorrect because TYPDEF is at level 05
       05  NEW-TYPE-PART1 TYPEDEF PIC 9
```

3\. **TCTYPE_DECLARATION** Type-name can be an elementary or group item; for group-items all subordinate items of the group are part of the type declaration

```cobol
   01 NEW-TYPE TYPEDEF.                  >> is correct
       05 NEW-TYPE-PART1 PIC 9.
   01 NEW-TYPE TYPEDEF PIC 9.            >> is also correct
```

4\. `TYPEDEF` clause cannot be specified in the same data description entry as following entries

1.  **TCTYPE_NO_EXTERNAL** `EXTERNAL`
2.  **TCTYPE_NO_REDEFINES** `REDEFINES` (**TCTYPE_NO_REDEFINES_IF_STRONG**)

_(source page 289-290 of ISO Cobol 2014)_

```cobol
   01 NEW-TYPE TYPEDEF.                   >> is incorrect because EXTERNAL clause forbidden at other level GT 01
       05 NEW-TYPE-PART1 PIC 9 EXTERNAL.  >>

   01 NEW-TYPE TYPEDEF PIC 9 EXTERNAL     >> is incorrect as EXTERNAL should not be used with TYPEDEF

   01 NEW-TYPE TYPEDEF EXTERNAL           >> is incorrect as EXTERNAL should not be used with TYPEDEF
       05 NEW-TYPE-PART1 PIC 9.           >>

```

5\. **TCTYPE_TYPEDEF_LOCATION** `TYPEDEF` clause can only be specified in the `WORKING-STORAGE`, `LOCAL-STORAGE`, `LINKAGE`, or `FILE` sections of a program.

6\. **TCTYPE_GLOBAL_TYPEDEF** If the `TYPEDEF` clause is specified with the `GLOBAL` clause, the scope of the `GLOBAL` clause applies to the type-name, and to any data items subordinate to the type-name. The `GLOBAL` attribute is not acquired by a data item that is defined using a global type-name within a `TYPE` clause

```cobol
 ID Division.
 Program-ID. PR.
ENVIRONMENT      DIVISION.
CONFIGURATION    SECTION.
SOURCE-COMPUTER. IBM-3090.
OBJECT-COMPUTER. IBM-3090.
 Data Division.
 Working-Storage Section.
 01  NEW-TYPE TYPEDEF PIC X GLOBAL.
..
 ID Division.
 Program-ID. SEC1 Is Common.
 Data Division.
 Working-Storage Section.
 01  WS-NEW   TYPE NEW-TYPE.
 01  WS-NEW-1 TYPE NEW-TYPE.
..
  End Program SEC1.
 Program-ID. SEC2 Is Common.
 Data Division.
 Working-Storage Section.
 01  WS-NEW   TYPE NEW-TYPE.
 01  WS-NEW-2 TYPE NEW-TYPE.
..
  End Program SEC2.
  End Program PR.
```

7\. Group items can be weakly, strictly or strongly typed.

A typed group item is strictly/strongly typed in any of the following cases:

1. **TCTYPE_GROUP_TYPED** The item is described with a `TYPE` clause that references a type declaration specifying the `STRICT` or `STRONG` phrase.
2. **TCTYPE_ITEM_UNDER_STRONG_TYPE** The item is subordinate to a group item described with the `TYPE` clause that references a type declaration specifying the `STRICT` or `STRONG` phrase.
   _(source page 132 of ISO Cobol 2014)_

```cobol
 01 TYPE-1 TYPEDEF.
    05 TYPE-1-1 PIC X.
 01 TYPE-2 TYPEDEF STRONG.
    05  TYPE-2-1 PIC X.
    05  TYPE-1-1 TYPE TYPE-1.

 01 TYPE-USE TYPE TYPE-2.  --> Group item TYPE-USE-2 is Strong because TYPE-2 is Strong, TYPE-1 of TYPE-USE is
                               also strongly typed as subordinate of a strong group item.
```

8\. **TCTYPE_DEFAULT_ACCESS_MODIFIER** By default access modifier of a `TYPEDEF`is `PRIVATE`. However this access modifier could be also explicitly set to `PRIVATE`or `PUBLIC`.

- The `PUBLIC` phrases gives a public visibility to the TYPE. From an external program, a PUBLIC TYPE will be accessible by typing `MyPGMName::MyPublicType`. Otherwise, a `PUBLIC` `TYPEDEF` could be called only by it's name in the enclosing program.
- The `PRIVATE`phrases gives a private visibility to the Type. Only the enclosing program and procedures/functions inside the same program can call it.

```cobol
     **Default TYPEDEF is PRIVATE
       01 DateJulian TYPEDEF STRICT.
         10 YYYY PIC 9(04).
         10 DDD  PIC 9(03).

     **Explicitly set TYPEDEF as PRIVATE
       01 DateJulian TYPEDEF STRICT PRIVATE.
         10 YYYY PIC 9(04).
         10 DDD  PIC 9(03).

     **Explicitly set TYPEDEF as PUBLIC
       01 DateJulian TYPEDEF STRICT PUBLIC.
         10 YYYY PIC 9(04).
         10 DDD  PIC 9(03).
```

## Strongly-typed and Strictly-type rules

9\. Strongly-typed and Strictly-typed group items and elementary items subordinate to strongly-typed or strictly-typed group items cannot be any of
the following:

1. implicitly or explicitly redefined
2. renamed in whole or in part (**TCTYPE_NO_RENAMES_IF_STRONG**)
3. reference modified, except for elementary items of category alphabetic, alphanumeric, boolean and
   national.

_(source page 807 of ISO Cobol 2014)_

```cobol
* Unauthorized use of redefines , renames for strong typed groups
01  TYPE-GRP-STRONG TYPEDEF STRONG.
    10  TYPE-GRP-STRONG-PART1 PIC X(10).
    10  TYPE-GRP-STRONG-PART2 PIC X(05).
    10  TYPE-GRP-STRONG-PART3 PIC X(05).
01  USE-TYPE-GRP.
    10  USE-TYPE-GRP-STRONG TYPE TYPE-GRP-STRONG.
    10  USE-TYPE-GRP-STRONG-RED REDEFINES USE-TYPE-GRP-STRONG.
        15 USE-TYPE-GRP-STRONG-RED-PART1 PIC 9 Comp.
66  TYPE-GRP-STRONG-REN RENAMES TYPE-GRP-STRONG-PART1 OF USE-TYPE-GRP-STRONG
                           THRU TYPE-GRP-STRONG-PART2 OF USE-TYPE-GRP-STRONG
 * Unauthorized use of reference modification for strong typed groups
01  TYPE-GRP-STRONG TYPEDEF STRONG.
    10  TYPE-GRP-STRONG-PART1 PIC X(10).
    10  TYPE-GRP-STRONG-PART2 PIC 9(04).
01 USE-TYPE-GRP-STRONG TYPE TYPE-GRP-STRONG.
...
PROCEDURE DIVISION
    MOVE 'xxx'       TO USE-TYPE-GRP-STRONG (2:5)


```

10\. **TCTYPE_LIMITED_STRONG_RECEIVER** A strongly-typed or strictly-typed group item may be referenced as a receiving operand only in one of the following:

1. a program, function or method activation as a formal parameter or returning item
2. an INITIALIZE statement
3. A MOVE statement
4. a READ statement
5. a RELEASE statement with the FROM phrase
6. a RETURN statement
7. a REWRITE statement with the FROM phrase
8. the data item referenced in the DESTINATION clause of an element of the operand of a VALIDATE
   statement
9. the subject of a data description entry that contains a VALIDATE-STATUS clause that references the
   element of the operand of a VALIDATE statement.
10. a WRITE statement with the FROM phrase.

_(source page 807 of ISO Cobol 2014)_

11\. **TCTYPE_STRONG_COMPARE** A strongly-typed or strictly group item can be compared only with another strongly-typed group item of the same type.
_(source page 807 of ISO Cobol 2014)_

1.  A strongly typed or a strictly-typed receiving variable of a move statement can only receive value from a variable of the same type.

    ```cobol
       01 DateJulian    TYPEDEF STRONG.
         10 YYYY PIC 9(04).
         10 DDD  PIC 9(03).
      01 MyDate1             TYPE DateJulian.
      01 MyDate2             TYPE DateJulian.
      01 NonTypedDateJulian  pic 9(07).
    ```

    `move MyDate1 to MyDate2` It's allowed because MyDate1 is of the same type as MyDate2

    ~~`move NonTypedDateJulian to MyDate2`~~ It's Not allowed because NonTypedDateJulian is not of type DateJulian

## Strongly-typed rules

12\. **TCTYPE_NO_ELEMENTARY_STRONG** Elementary items cannot be strongly typed.

_(source page 132 of ISO Cobol 2014)_

```cobol
   01 NEW-TYPE IS TYPEDEF STRONG PIC X.    >> unauthorized as it is an elementary item
   01 NEW-TYPE TYPEDEF STRONG.             >> correct
      05 NEW-TYPE-PART1 PIC 9.
```

13\. **TCTYPE_STRONG_NO_VALUE** The data description entry of a strongly-typed group item cannot contain a VALUE clause, nor can the item be
a conditional variable.

_(source page 807 of ISO Cobol 2014)_

```cobol
* Authorized use of Values with Elementary items
01  TYPE_ELT TYPEDEF PIC X Value 'A'.
    88 TYPE-ELT-VALB VALUE 'B'.
    88 TYPE-ELT-VALC VALUE 'C'.
* Unauthorized use of Value as we have here a strongly typed group
01  TYPE-GRP TYPEDEF.
    05  TYPE-GRP-1 PIC X VALUE 'A'.
    05  TYPE-GRP-2 PIC X VALUE 1.
```
