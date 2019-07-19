---
title: ":: Operator"
---

## Format

```
<GROUP> :: <ELEMENTARY ITEM>
```

> ‘A name can be made unique if it exists within a hierarchy of names, and the name
> can be identified by specifying one or more higher-level names in the hierarchy.
> The higher-level names are called qualifiers, and the process by which such names
> are made unique is called qualification.’ COBOL Language Reference

In COBOL syntax, this qualification is done by using one or more phrases with `IN` or `OF` followed by a qualifier.

It is only necessary to specify enough qualification to make the name unique.

Difficulties with `IN` or `OF` operators :

- Data items must always be qualified from the _lower level_ item to the _higher level_ group item. This is not consistent with more modern programming languages, where variables are qualified from the _higher level_ data structure to the variable of interest. This can thus be considered a backwards practice by younger COBOL programmers.
- This can make name completion (ie. content assist) less efficient.

TypeCobol proposes an alternative method to fully qualify data items when they are used.
A new operator will be added for qualification of an element in a group.

Instead of saying
`‘A of B’` or `‘A in B’`
we will use a new syntax based on `‘::’` operator
`‘B::A’`

## Cobol examples

### WORKING-STORAGE SECTION.

```cobol
01 GR1.
   02 GR11.
      05 ELT11   PIC X.
   02 GR12.
      05 ELT11   PIC X.
   02 GR13.
      05 ELT12   PIC X.
01 GR2.
   02 GR21.
      05 ELT11   PIC X.
   02 GR22.
      05 ELT21   PIC X.
01  GRX          PIC X(100).
```

### PROCEDURE DIVISION.

**(1)**

```cobol
MOVE ELT11                  TO GRX
```

###### COBOL Z/os Error message:

_IGYPS0037-S "ELT11" was not a uniquely defined name.
The definition to be used could not be determined from the context.
The reference to the name was discarded._

**(2)**

```cobol
MOVE ELT11 OF GR11          TO GRX
```

**(3)**

```cobol
MOVE ELT11 OF GR1           TO GRX
```

###### COBOL Z/os Error message:

_IGYPS0037-S "ELT11 OF GR1" was not a uniquely defined name.
The definition to be used could not be determined from the context.
The reference to the name was discarded._

**(4)**

```cobol
MOVE ELT11 OF GR11 OF GR1   TO GRX
```

**(5)**

```cobol
MOVE ELT11 OF GR2           TO GRX
```

**(6)**

```cobol
MOVE ELT11 OF GR21          TO GRX
```

**(7)**

```cobol
MOVE ELT21 OF GR22 OF GR2   TO GRX
```

**(8)**

```cobol
MOVE ELT21 OF GR22          TO GRX
```

**(9)**

```cobol
MOVE ELT21 OF GR2           TO GRX
```

**(10)**

```cobol
MOVE ELT21                  TO GRX
```

#### Comments

**(1)**: `ELT11` is an elementary item of groups `GR11`, `GR12` and `GR21`. Then indicate `ELT11` doesn’t allow to distinguish which `ELT11` is to be moved.

**(2)**: With the detail `GR11` we have identified `ELT11` among all the descriptions.

**(3)**: With the detail of the group `GR1` we still have two possibilities : `GR11` and `GR12`.

**(4)**: In this form we specify all hierarchy details; it is not mandatory (as we see in **(2)**) but valid.

**(5)**, **(6)**: With indication of sub-group or group, our item `ELT11` is uniquely defined.

**(7)**, **(8)**, **(9)**, **(10)**: As `ELT21` is only declared once, we can use it with all the hierarchy, part of it, or only the elementary item.

## TYPECOBOL to COBOL translation

The following sentence:

```
A :: B :: C
```

will be translated as:

```cobol
C OF B OF A
```

## TYPECOBOL examples

### WORKING-STORAGE SECTION.

```cobol
01 GR1.
   02 GR11.
      05 ELT11   PIC X.
   02 GR12.
      05 ELT11   PIC X.
   02 GR13.
      05 ELT12   PIC X.

01 GR2.
   02 GR21.
      05 ELT11   PIC X.
   02 GR22.
      05 ELT21   PIC X.
01  GRX          PIC X(100).
```

### PROCEDURE DIVISION.

```
MOVE GR1::GR11::ELT11   TO GRX
MOVE GR2::GR21::ELT11   TO GRX
```

In COBOL providing the complete hierarchical path is not mandatory: it is only needed to give minimal group names to avoid any ambiguity on elementary items.
In TypeCobol however, as a rule of thumb, the complete path to the elementary item should always be provided. Even if some parts of the path could be made implicit, in the general case, being explicit improves code readability and sustainibility.

## Multi dimensional tables

In Cobol language for a multi-dimensional table, all subscripts are specified after the elementary item.
With TypeCobol and new `::` operator, each subscript will be mentioned at the exact level of the occurrence.
Objective is to improve understanding and readability by using subscript at the level of the occursed element or group.

Examples

```cobol
01 A.
    02 B OCCURS 10.
      03 C.
        04 D1 PIC X(10).
        04 D2 PIC X(10) OCCURS 10.

01 AA.
    02 B OCCURS 10.
      03 C.
        04 D1 PIC X(10).
        04 D2 PIC X(10) OCCURS 10.
```

In Cobol to address a `D2` item we could use one of the following syntax:

```cobol
D2 OF A (x y)
```

```cobol
D2 OF C OF A (x y)
```

```cobol
D2 OF B OF A (x y)
```

```cobol
D2 OF C OF B OF A (x y)
```

Suggested syntax in TypeCobol with `::` operator

```cobol
A :: B (x) :: C :: D2 (y)
```

## Specific case : unnamed groups

In Cobol, declaration of elements or groups not used without a name is possible.
For Cobol syntax there is no impact because if we want to qualify an element of a group it is not necessary to give the complete hierarchy.
The Cobol syntax for multi dimensional table is also not impacted by this, i.e. :

```cobol
01  GROUPE.
    05            OCCURS 5.
			10   		OCCURS 5.
				15 ELEMENT1 PIC X.
```

In Cobol syntax we have all subscripts at the end then we address with

```cobol
	ELEMENT-1 (i j)
```

Or

```cobol
	ELEMENT-1 OF GROUPE (i j)
```

This has an impact on TypeCobol new operator `‘::’` because we should use some subscript without any name :

```cobol
GROUPE::(i)::(j)::ELEMENT1
```

It should be considered as a best practice to avoid these groups.
