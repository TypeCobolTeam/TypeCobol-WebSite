---
title: Boolean Type
---

`Bool` is an intrinsic type of TypeCobol . A bool is limited to two values (TRUE and FALSE); this is not possible with current COBOL 85 syntax.
Format : 	
```cobol
01 Identifier TYPE Bool.
```

Main usages are:
```cobol
SET Identifier to TRUE
Set Identifier to false
if identifier
if not identifier
when identifier
```

As content of a boolean should be restricted to values ‘TRUE’ and ‘FALSE’ , TypeCobol parser must include specifics controls for Bool type :
-	TypeCobol parser should prevent usage of Move to the elementary item
-	(except MOVE from another Boolean or, for TypeCOBOL extension, MOVE TRUE or MOVE FALSE)
-	Typecobol should also warn when using a statement that might alter the boolean values 
 -	Move to a group containing a boolean 
 -	Initialize of a group with Booleans


## Default value
You can set a default value for bool just like you do in Cobol 85:
```cobol
01 var1 TYPE Bool value true.
01 var2 TYPE Bool value false.
```

By default, the value is `false`.

# Translation

## Translation in Cobol 85
In Cobol 85, bool is translated to:
```cobol
01 Identifier-value PIC X value low-value.
88 Identifier value ‘T’.
88 Identifier-false value 'F' X'00' thru 'S'
                              'U' thru X'FF'.
```
### Set
```cobol
SET Identifier to TRUE	remains the same in COBOL
SET Identifier to FALSE	will be translated as ‘SET Identifier-false’ to TRUE’
```

## Cobol 2002
Note that in Cobol 2002, you can use "set to false", so type bool is translated to:
```cobol
01               pic X value low-value.
  88 Identifier        value 'T' false 'F'.
```
 
