---
title: Types
---

# Types in a Nutshell

## The problem
With Cobol 85, there is no simple way to reuse structure of data across different programs.
I can use a COPY, but this doesn't work well if I need to use a structure with different data level.

Ex:
```cobol
01 Person
   05 BirthDate.
      10 BirthDate-Year  pic 9(04).
      10 BirthDate-Month pic 9(02).
      10 BirthDate-Day   pic 9(02).
   05 LastModificationDate.
      10 LastModificationDate-Year  pic 9(04).
      10 LastModificationDate-Month pic 9(02).
      10 LastModificationDate-Day   pic 9(02).
```
If my date format must evolve, I need to modify all my variables named something like 'Date' one by one.

### Prefix and length of variable name
If I use the variable parent's name, I'll end up with very long variable name. As Cobol 85 limit it to 30 characters (at least on Z/OS), I'll be limited very quickly.

It'd be great to avoid to reuse the variable parent's name and still distinguish clearly inside the code to what I refer.

## The need
 * Reuse structure of data across different programs.
 * Avoid to write the prefix
 * Be sure that a structure is modified as a whole

## The solution : Type


### TypeCobol syntax (almost like Cobol 2002)  Translation into Cobol 85
 
```cobol
01 Person TYPEDEF STRICT. 
   05 Id         pic 9(05).     
   05 lastName   pic X(30).     
   05 FirstName  pic X(30).      
   05 BirthDate  pic X(08).
```
 Define a type `Person`. +
Levels will be renumbered when used as a type

 
```cobol
01 myPerson TYPE Person.
```
 
```cobol
01 myPerson. 
   02 Id         pic 9(05).     
   02 lastName   pic X(30).     
   02 FirstName  pic X(30).      
   02 BirthDate  pic X(08).
```

 
```cobol
01 myPerson    type Person.
01 myPerson2   type Person.
move myPerson  to myPerson2
```
 
```cobol
*Ok because myPerson is of the same type as myPerson2
move myPerson to myPerson2
```

 
```cobol
01 txtZone     pic X.
01 myPerson    type Person.
move txtZone     to myPerson    
```
 
It doesn't compile because txtZone isn't of the same type as myPerson.   

 
```cobol
01 txtZone     pic X.
01 myPerson    type Person.
move UNSAFE txtZone     to myPerson    
```
 
It's allowed because the UNSAFE keyword is here to tell TypeCobol compiler to not control type. +
As a developer it's my responsibility to be sure that the content of txtZone match the description of myPerson. 

 
```cobol
01 CarInsurance.
  05 firstDriver   type Person.
  05 SecondDriver  type Person.
```
 
```cobol
01 CarInsurance.
  05 firstDriver.
   06 Id         pic 9(05).     
   06 lastName   pic X(30).     
   06 FirstName  pic X(30).      
   06 BirthDate  pic X(08).
  05 SecondDriver
   06 Id         pic 9(05).     
   06 lastName   pic X(30).     
   06 FirstName  pic X(30).      
   06 BirthDate  pic X(08).
```
###


To learn more:

 * https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolTypes[Detailed introduction on Type and Typedef]
 * https://github.com/TypeCobolTeam/TypeCobol/wiki/Cobol02TYPEDEF