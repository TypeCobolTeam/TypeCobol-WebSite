# Detailled rules of procedures and functions syntax 

#### Table of Contents
This page describes many rules regarding function or procedure declaration/call.
All these rules begin with **TCRFUN\_**.
* [Grammar reference](#grammar)
* [Caller / How to call a function or procedure](#call)
* [How to declare a function or procedure](#declaration)
* [Code generation](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolFunctionsCodegen)

<a name="grammar"/>

# Grammar reference

#### Procedure invocation syntax
```cobol
CALL <procedure-name>
    <INPUT clause>?
    <INOUT clause>?
    <OUTPUT clause>?
END-CALL?
```

#### Function invocation syntax
```cobol
<function-name> ( <arguments>? )
```

#### \_INPUT\_ clause syntax
```cobol
INPUT < <SENDING MODE clause>? <parameter-name> >+
```
#### \_SHARING MODE\_ clause syntax
```cobol
BY? <CONTENT | REFERENCE | VALUE>
```

#### \_INOUT\_ clause syntax
```cobol
INOUT <parameter-name>+
```

#### \_OUTPUT\_ clause syntax
```cobol
OUTPUT <parameter-name>+
```

<a name="call"/>

# Caller / How to call a function or procedure

## Procedure
```cobol
CALL procedure-name INPUT      inputParam1 ... inputParamN
                    IN-OUT     inoutParam1 ... inoutParamN
                    OUTPUT     outParam1   ... outParamN
end-call
if procedure-name::error-code = '0000'
   ... return OK
else 
   ... return KO, handle error
end-if
```

## Functions
```cobol
FUNCTION function-name (param1, param2, param3, ... paramN)
```
This syntax is the same as the syntax for COBOL intrinsic functions and can be used in the same contexts. In particular:
  * __TCRFUN\_TCFUNC\_WHERE\_COBOL85FUNC__\_ a custom function can be called as part of any statement where an COBOL 85 intrinsic function call would be acceptable. **TODO: see if we can allow intrinsic functions in more contexts than COBOL 85.**
  * __TCRFUN\_OPTIONAL\_FUNCTION__ the `FUNCTION` keyword is optional
However, if there is any ambiguity about *function-name*, the function call must be qualified as described in [[Namespaces & imports|Namespaces]].

### Alternate call for function
__TCRFUN\_FUNC\_ALLOW\_CALL__ Functions can also be called like a procedure. This syntax make it easier to test the error-code.
```cobol
CALL function-name INPUT      inputParam1, ... inputParamN
                   RETURNING  returningParameter 
end-call
if function-name::error-code = '0000'
   ... return OK
else 
   ... return KO, handle error
end-if
```

## Common rules for functions and procedures calls

* Rule that need to be validated \_TCRFUN\_FUNC\_COMMA\_SEPARATOR  You need to separate parameters with a comma (`,`).\_

* __TCRFUN\_CALL\_PARAMETER\_ORDER__
  During a procedure invocation, parameters are always used in the same order they are declared in the corresponding procedure:
  1. First, all `INPUT` parameters (if any)
  2. Second, all `INOUT` parameters (if any)
  3. Third, all `OUTPUT` parameters (if any)


* __TCRFUN\_INPUT\_BY__   `INPUT` parameters can be passed by content, by reference or by value.
  This distinction is made for a given parameter using the \_SHARING MODE\_ clause.
  If the \_SHARING MODE\_ clause is omitted for a given parameter, the \_SHARING MODE\_ of the parameter
  declared immediately before on the current `INPUT` clause is used. 

  If there is no previous parameter immediately before on the current `INPUT` clause, then `BY REFERENCE` is assumed. 

  For example, in the following declaration:
** `i1`, `i4`, `i5`, `o1`, `o2` are passed `BY REFERENCE`
** `i2`, `i3` are passed `BY CONTENT`

```cobol
CALL myprocedure INPUT i1 BY CONTENT i2 i3 BY REFERENCE i4 i5 
                 OUTPUT o1 o2 
END-CALL.
```
* __TCRFUN\_CALL\_INOUT\_AND\_OUTPUT\_BY\_REFERENCE__  `INOUT`,`OUTPUT` parameters are passed by reference.
  The \_SHARING MODE\_ clause cannot be specified.

  In other words, `BY REFERENCE` is always implied for `INOUT`,`OUTPUT` parameters.

## Parameters matching
The caller must provide parameters that match a function or procedure declaration signature.
Following rules applies to determiner if a match is successful:
* **TCRFUN\_MATCH\_PARAMETERS\_NUMBER**    The number of parameters provided on each procedure call must match the number of parameters of at least one declared procedure of the same name.
* **TCRFUN\_MATCH\_PARAMETERS\_TYPE** 
  The type of all `INPUT`,`INOUT`,`OUTPUT` parameters provided on a given procedure call must match the type of all parameters of at least one declared procedure of the same name.
* Rules that need to be implemented: 
 * [TCRFUN\_MATCH\_PARAMETERS\_SIZE](https://github.com/TypeCobolTeam/TypeCobol/wiki/FunctionsDeclaration)
 * [TCRFUN\_MATCH\_PARAMETERS\_COMPRESSION](https://github.com/TypeCobolTeam/TypeCobol/wiki/FunctionsDeclaration)


<a name="declaration"/>

# How to declare a function or procedure

## Procedure declaration
```cobol
DECLARE PROCEDURE procedure-name PRIVATE|PUBLIC
  INPUT       <parameter descriptions>
  IN-OUT      <parameter descriptions>
  OUTPUT      <parameter descriptions>
  ERROR-CODE  <error-code values description>.

DATA DIVISION.
    WORKING-STORAGE SECTION.
        <data declaration(s)>
PROCEDURE DIVISION.
        <COBOL and/or TypeCobol statement(s)>
END-DECLARE.
```

## Function declaration
```cobol
DECLARE FUNCTION function-name PRIVATE|PUBLIC
  INPUT       <parameter descriptions>
  RETURNING   <parameter-name>
  ERROR-CODE  <error-code values description>.

DATA DIVISION.
    WORKING-STORAGE SECTION.
        <data declaration(s)>
PROCEDURE DIVISION.
        <COBOL and/or TypeCobol statement(s)>
END-DECLARE.
```

## Common rules for procedure and function declaration
The declaration of a procedure or a function in TypeCobol follows the same rules as the declaration of a nested program in COBOL 85, with the following differences:

* **TCRFUN\_DECLARE\_FUNCTION\_OR\_PROCEDURE**  
A function is declared using the `DECLARE FUNCTION` statement.
A procedure is declared using the `DECLARE PROCEDURE` statement.
The `FUNCTION` or `PROCEDURE` keywords are not mandatory. If both of these keywords are omitted, the following rules apply:
  * The nature of what is declared is decided according to the rules **TCRFUN\_NO\_INOUT\_OR\_OUTPUT\_FOR\_FUNCTIONS** and **TCRFUN\_NO\_RETURNING\_FOR\_PROCEDURES**.
  * If the parameter list declaration uses both the `RETURNING` keyword and any of the `IN-OUT` or `OUTPUT` keywords, the `DECLARE` statement will be in error, as what is declared cannot be either a function or a procedure.
  * If there are only \_input\_ parameters (or no parameter at all) and no \_in-out\_, \_output\_ or \_returning\_ parameter, the `FUNCTION` keyword is assumed.

* __TCRFUN\_FIXED\_FORMAT__ In fixed column format, all keywords related to function or procedure, can start at any column between columns 8 and 72 included.

* __TCRFUN\_DEFAULT\_ACCESS\_MODIFIER__ Procedure or Function declaration will not need any of the PRIVATE or PUBLIC access modifier. The default Access modifier is PRIVATE if none is declared. 
  * The `PUBLIC` phrases gives a public visibility to the procedure or function.
  * The `PRIVATE` phrases gives a private visibility to the procedure or function. Only the enclosing program or others procedures and functions inside the same program can call it.

* __TCRFUN\_DOTS__ A `.` is mandatory only after the parameters and return-code declaration.

* __TCRFUN\_NO\_DOT\_AFTER\_VISIBILITY__ There is no dot after function or procedure visibility.

* __TCRFUN\_NO\_PERFORM\_OF\_ENCLOSING\_PROGRAM__  
A function or procedure cannot use any `PERFORM` statement that targets a paragraph or section of the enclosing program.

### Parameters description (input, in-out, output and returning):
* __TCRFUN\_PARAMETER\_DECLARATION\_ORDER__ Parameters are always declared in this order: input, in-out, output, returning.

* __TCRFUN\_PARAMETER\_DESCRIPTION__ A parameter description must contains the following:
  * a parameter name (ex: `dateToConvert`)
  * either one `PICTURE` clause or one `TYPE` clause
  * Restrictions:
    - No level descriptor, except for level `88`. Level `01` is implied in all cases when `88` is not present.
    - No `REDEFINES`
    - No `RENAMES`
    - Neither `FILLER` nor unnamed variables
    - Neither `GLOBAL` nor `EXTERNAL`
    - Neither `BY REFERENCE`, nor `BY VALUE`, nor `BY CONTENT` can be specified: `BY REFERENCE` is implied in all cases.

* __TCRFUN\_0\_TO\_N\_PARAMETERS__
Lists of \_input\_, \_in-out\_ and \_output\_ parameters can contain 0 to N parameters.
  * The list of \_input\_ parameters can be empty. In this case, the function/procedure takes no \_input\_ parameters and the `INPUT` keyword must not be used.
  * The list of \_in-out\_ parameters can be empty. In this case, the procedure takes no \_in-out\_ parameters and `IN-OUT` keyword must not be used.
  * The list of \_output\_ parameters can be empty. In this case, the procedure takes no \_output\_ parameters and `OUTPUT` keyword must not be used.

* __TCRFUN\_NO\_INOUT\_OR\_OUTPUT\_FOR\_FUNCTIONS__
  A function can only declare \_input\_ parameters (using the `INPUT` clause) and one \_returning\_ parameter (using the `RETURNING` clause). A function cannot declare \_in-out\_ or \_output\_ parameters.

* __TCRFUN\_NO\_RETURNING\_FOR\_PROCEDURES__ A procedure cannot have a \_returning\_ parameter.

* __TCRFUN\_0\_TO\_1\_RETURNING\_PARAMETER__ A function has exactly one returning parameter. The declaration of returning parameter is mandatory, but it can be omitted (`RETURNING OMITTED`).

* __TCRFUN\_NO\_COPY\_IN\_PARAMETERS__ A parameter cannot be described by a COPY.

#### Level 88 parameter description
__TCRFUN\_LEVEL\_88\_PARAMETERS__ Level 88 parameters are declared like a standard COBOL 85 parameters:
**TODO: see if we can provide enum grammar, instead of levels 88 **
```cobol
DECLARE FUNCTION function-name PRIVATE
  input     typeOfTransportpic X
              88 typeOfTransport-Car  value 'C'
              88 typeOfTransport-Bike value 'B'.
``` 

### Error-code values
* __TCRFUN\_ERROR\_CODE\_DECLARATION\_ORDER__ error-code values are always declared after input, in-out, output and returning parameters.
* __TCRFUN\_ERROR\_CODE\_OPTIONAL__ Error-code declaration is optional for procedure and function.

See page [Error](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolErrorSyntax) for more details about error management.

### Other Rules:
<a name="TCRFUN\_DECLARATION\_AS\_NESTED\_PROGRAM"/>

__TCRFUN\_DECLARATION\_AS\_NESTED\_PROGRAM__ A procedure or a function is declared like a nested program. The declaration is inside the procedure division and outside of any section or paragraph.

Compared to a nested program declaration, a procedure or function declaration has the following exceptions:
* __TCRFUN\_DECLARATION\_NO\_IDENTIFICATION\_DIVISION__ There is no `IDENTIFICATION DIVISION`
* __TCRFUN\_DECLARATION\_NO\_ENVIRONMENT\_DIVISION__ There is no `ENVIRONMENT DIVISION`
* Inside the `DATA DIVISION`:
  * __TCRFUN\_DECLARATION\_NO\_FILE\_SECTION__  There is no `FILE SECTION`
  * Restricted data declaration inside `WORKING-STORAGE`, `LOCAL-STORAGE` and `LINKAGE` 
    * __TCRFUN\_DECLARATION\_NO\_GLOBAL__ `GLOBAL` are not allowed
    * __TCRFUN\_DECLARATION\_NO\_EXTERNAL__ `EXTERNAL` are not allowed 
    * __TCRFUN\_DECLARATION\_NO\_DUPLICATE\_NAME__ No `LINKAGE SECTION` item can have the same name as a parameter declared as `INPUT`, `OUTPUT`, `IN-OUT` or `RETURNING`.
* `PROCEDURE DIVISION` is equivalent to COBOL 85 program, with the following exceptions:
    * __TCRFUN\_DECLARATION\_NO\_USING__ The `USING` phrase is not allowed.
* __TCRFUN\_MANDATORY\_END\_DECLARE__ The `END-DECLARE.` phrase is mandatory.

* __TCRFUN\_LIBRARY\_PROCEDURE\_NO\_USING__ 
A library `PROCEDURE DIVISION` must not declare any `USING` clause.

* __TCRFUN\_ONLY\_PARAGRAPH\_AND\_PUBLIC\_FUNC\_IN\_LIBRARY__
f you declare a PUBLIC procedure or function, then you can only write PARAGRAPH declaration inside the same PROCEDURE DIVISION.
You can only declare PUBLIC procedure or functions inside this PROCEDURE DIVISION.
A special paragraph named INIT-LIBRARY will be called when the library is called.

## Where to put this declaration?
It depends on the procedure visibility: *private* or *public*

#### Private procedure 
Private procedure declaration must be put outside paragraph and section declaration.

```Cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. TypeCobol.
data division.
working-storage section.
01 a pic 9(04).
01 b pic 9(04).
01 c pic 9(04).
PROCEDURE DIVISION.
   move 1 to a
   move 2 to b
   perform MyParagraph
.
MyParagraph.
   CALL MyProcedure input(a, b)
                    output(c)
.
declare procedure MyProcedure private
  input  param1 pic 9(04)
         param2 pic 9(04)
  output outParam1 pic 9(04).
procedure division.
   compute outParam1 = param1 + param2 
   .
end-declare.
.
 END PROGRAM.
```

#### Public procedure
If you want to declare at least one public procedure, then you can't write section or paragraph inside procedure division.
All code inside procedure division must be put inside functions or procedures declaration.
The only way to call your program is through a public function or procedure.

Technically all functions and procedure will be generated into a sub-programs and your procedure division will contains code to call theses sub-programs.

```Cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. TypeCobol.
PROCEDURE DIVISION.
declare procedure MyProcedure private
 input  param1    pic 9(04)
 output outParam1 pic 9(04).
procedure division.
   ...
   .
end-declare.
declare function MyPublic public
 input     param1 9(04)
 returning result type bool.
 procedure division.
   ...
   .
end-declare.
.
END PROGRAM.
```