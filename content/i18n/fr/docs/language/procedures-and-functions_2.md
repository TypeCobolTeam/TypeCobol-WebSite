---
title: Procedures & Functions
---

#### Table of Contents
* [Motivation](#introduction)
* [Terms & definitions](#terms)
* [Syntax](#syntax)
* [Examples](#examples)
* [Code generation](#codegen)

<a name="introduction"/>

## Introduction and motivation
As Cobol 85 only have intrinsic functions you must rely on custom strategy to encapsulate portion of reusable code into yours programs.  
TypeCobol provides a fast and reliable way to implement and call simple (stackless) cutom-designed functions and procedures.  

Advantages of function and procedure over Cobol 85 sub programs:
* Syntax is shorter than Cobol 85 sub programs
* You can name function and procedures with more than 8 letters.
* Propose visibiliy mechanism
* Control input/output arguments
* Have an implicit return code
* Check if caller test the return code


<a name="terms"/>

## Terms and definitions

Procedures and functions are defined by:

|Term   |Description| Function|Procedure|
|---	|---	|---	|---	|
|name  |A function must have a name to be callable. Two functions or procedures of the same namespace can have the same name, provided they have different input parameters (see [[disambiguation|TypeCobolFunctionsDisambiguation]]). Two functions or procedure can have the same name as long as the same input parameters only if they belong to different namespaces.|1|1|
|library|The program defining a given function or procedure. A library can define any number of functions or procedures.|1|1|
|access modifier|`PUBLIC` makes the function or procedure visible to every program which has also access to its namespace. `PRIVATE` makes the function or procedure visible only inside in the enclosing program.|1|1|
|input parameters|An `INPUT` parameter is read-only for the callee. It must be set by the caller to a value expected by the calee prior to a given function or procedure call.|0..N|0..N|
|in-out parameters|An `IN-OUT` parameter is defined as both `INPUT` and `OUTPUT`. It is readable and writable by the calee.|0|0..N|
|output parameters|An `OUTPUT` parameter should be written by the callee before the end of a given function or procedure call.|0|0..N|
|returning parameter|Same behavior as the COBOL85 `RETURNING` phrase|0..1|0|
|error-code parameter|An implicite error code parameter is always passed as an output parameter |1|1|
|error-code values|list of error-codes values that the caller should test after the procedure or function call  |0..N|0..N|

### Caller
The program containing a given **function/procedure** call.

### Callee 
The program containing a given **function/procedure** definition.

#### Namespaces
TypeCobol [[namespaces|Namespaces]] are structured as hierarchies to allow reuse of names in different contexts. 
They group **functions/procedures** around a particular functionality and avoid name collisions.
#### Standard library
TypeCobol provides out-of-the box, ready for production functions and procedures via it standard library.
These are called TypeCobol « Intrinsic » functions.
The TypeCobol standard libray namespace is [[http://www.teamtown.com/happinesslocator/images/qm_icon.gif]] *TCSL* (TypeCobol Standard Library).

### Parameters
A **function** or **procedure** can have any number of data parameters.
When you call a **function/procedure**, you can pass any data to it, provided each data is of the type defined in the **function/procedure** interface contract. You can pass identifiers or directly literals with no particular concern. 
Each parameter can be of any COBOL format or of any TypeCobol [[custom type|TypeCobolTypes]].
If a **function/procedure** needs more than one parameter, you need to use the proper parameter separator.  



#### Error-code parameter
The error-code parameter is used by **function/procedure** to specify if it has encountered an error during its treatment.
The error-code act as an output parameter and has a fixed name.

The error-code is an alphanumeric parameter of length 4 (`pic X(04)`).
It should be equal to `"0000"` if all the mandatory terms of the interface contract are filled. All others values should be treated as an error.
The meaning of each values depend on the interface contract of the **function/procedure**.

####Error-code values (*TODO: Need to clarify this*)
Expected error-code are values of *error-code parameter* that the callee should test after the **function/procedure** call. Each value represent a cause of common error and it's likely to happen.  
Note that not all causes of errors need to specified as expected return-code values, only the most common.  
A callee can ignore error-code values, but must at least test the if the error-code parameter is OK (`value = "0000"`).

#### Receiving mode
Input parameters can be passed from the **caller** to the **callee** either by reference (default) or by content.
Output parameters are always passed by reference from the **callee** to the **caller**.
You can't use **by value** with functions or procedures. 
Receiving mode are transitive across the parameters like standard Cobol 85. It means, that if you specify "by content" for one parameter, it applies to all following parameters that don't specify a receiving mode.

<a name="syntax"/>
## Syntax
A function or procedure declaration section looks a bit like a nested program:
it must indicate input, in-out, output, returning and return and return-code parameters,
and it has all it needs to fill its interface contract.

### Grammar
<a name="procfun-grammar"/>
```cobol
DECLARE FUNCTION function-name PRIVATE|PUBLIC
  input     **list of parameter-name**
  returning **parameter-name**
  error-code **values of the error-code that the caller should test after the call**.

DATA DIVISION.
    working-storage section.
        your data.
PROCEDURE DIVISION.
        (type)cobol statements*
END-DECLARE.
```

```cobol
DECLARE PROCEDURE procedure-name PRIVATE|PUBLIC
  input       **list of parameter-name**
  in-out      **list of parameter-name**
  output      **list of parameter-name**
  error-code  **values of the error-code that the caller should test after the call**.

DATA DIVISION.
    working-storage section.
        your data.
PROCEDURE DIVISION.
        (type)cobol statements*
END-DECLARE.
```

### Rules
See detailled syntax rules here: [TypeCobolFunctionsSyntax](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolFunctionsSyntax)


<a name="examples"/>
## examples
###Function currentDate 
```cobol
declare function currentDate private
returning result type Date.

data division.
procedure division.
    accept result from DATE YYYYMMDD.
end-declare.
```
You can then call this function like this:
```cobol
move function currentDate() to myDate
```

###Procedure readCustomer
```cobol
declare procedure readCustomer public
input     customedId pic 9(04)
output    customer   type Customer.

data division.
procedure division.
    ...
    move xxxx to customer::name
    move xxxx to customer::surname
    ...
end-declare.
```
You can then call this procedure like this:
```cobol
call readCustomer input  by content '0001'
                  output myCust
end-call
```
*TODO: check errors*

See complete examples here: [TypeCobolFunctionsExamples](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolFunctionsExamples)

<a name="codegen"/>
## Code generation
Each TypeCobol function or procedure translate in COBOL 85 into a program.
This program is nested in the same `PROCEDURE DIVISION` as the original function or procedure.

### Example
The [above declaration](#procfun-grammar) translates as the following:
```cobol
PROGRAM-ID. function-name.
DATA DIVISION.
    WORKING-STORAGE SECTION.
        your data.
PROCEDURE DIVISION
    USING **list of parameter-name**
    RETURNING parameter-name
    .
         (type)cobol statements*
END PROGRAM.
```
### Rules
See detailled codegen rules here: [TypeCobolFunctionsSyntax](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolFunctionsSyntax#codegen)
