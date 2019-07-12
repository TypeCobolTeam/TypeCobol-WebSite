---
title: Types
---

What is a TYPE
==============
:toc:
:toc-placement: preamble
:toclevels: 1

// Need some preamble to get TOC:
{empty}

= What is a type?
A `TYPE` is both a structure of data and definition of a type.

== Structure of data

|===
| *TypeCobol syntax* | *Translation into Cobol 85*
| You can declare a TYPE `Person` using the TYPEDEF keyword:
```cobol
01 Person TYPEDEF STRICT. 
   05 Id         pic 9(05).     
   05 lastName   pic X(30).     
   05 FirstName  pic X(30).      
   05 BirthDate  pic X(08).
```
Levels will be renumbered when used as a type
|

| You can then use `Person` as a structure:
```cobol
01 person1 TYPE Person.
```
|Which is equivalent to:
```cobol
01 person1.
   02 Id         pic 9(05).     
   02 lastName   pic X(30).     
   02 FirstName  pic X(30).      
   02 BirthDate  pic X(08).
```

|A `TYPE` can be used at any level, but be aware that Cobol limits the number of level to 49.
```cobol
01 person1 TYPE Person.
01 MyData.
    05 Array occurs 5.
       10 person2 TYPE Person.
```
|Which is equivalent to:
```cobol
01 person1.
   02 Id         pic 9(05).     
   02 lastName   pic X(30).     
   02 FirstName  pic X(30).      
   02 BirthDate  pic X(08).
01 MyData.
    05 Array occurs 5.
       10 person2.
         11 Id         pic 9(05).     
         11 lastName   pic X(30).     
         11 FirstName  pic X(30).      
         11 BirthDate  pic X(08).
```
|===


== Type (only apply to STRICT and STRONG type)

A `TYPE` allow you to control assignment to a variable.
Only variable of the same `TYPE` can be moved into each other.

With the following declaration:
```cobol
01 Person TYPEDEF STRICT. 
   05 Id         pic 9(05).     
   05 lastName   pic X(30).     
   05 FirstName  pic X(30).      
   05 BirthDate  pic X(08).
01 person1 TYPE Person.
01 person2 TYPE Person.
01 person3 pic X(30).
```
Here, `person2` is of `TYPE Person` and can only be assigned from `Person1`.
```cobol
move person1 to person2  *> OK
move person3 to person2  *> KO - type not equals
```



= Weak, Strict and Strong type

* *Weak* (like Cobol 2002 ISO Specifications)
** The type is only used as a structure

* *Strict* (TypeCobol)
** Strict represent both a structure and a type

* *Strong* (like Cobol 2002 ISO Specifications)
** Strong represent both a structure and a type. 
** It's an equivalent of Strict, but you cannot have a `value` clause or a picture directly on the Typedef data element.


= How to share type between programs?

To reference a Type from another program, you must first declare it `PUBLIC`:
```cobol
ID DIVISION.
PROGRAM-ID. PgmA.
data division.
working-storage section.
01 Person TYPEDEF STRICT PUBLIC. 
   05 Id         pic 9(05).     
   05 lastName   pic X(30).     
   05 FirstName  pic X(30).      
   05 BirthDate  pic X(08).
END PROGRAM PgmA.
```

You can then prefix the type with the program name:
```cobol
ID DIVISION.
PROGRAM-ID. PgmB.
data division.
working-storage section.
01 MyPerson TYPE PgmA::Person.
END PROGRAM PgmB.
```

= Links
Use https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolNameQualification[operator `::`] instead of operator `of` or `in`.


= In a future release
* typed pointer