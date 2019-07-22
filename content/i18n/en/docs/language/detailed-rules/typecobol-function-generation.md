---
title: "Function generation"
---

# Code generation for Functions and Procedure
Each TypeCobol function or procedure translate in COBOL 85 into a program.
This program is nested in the same `PROCEDURE DIVISION` as the original function or procedure.

## Function/Procedure declaration
### Example
```cobol
       DECLARE FUNCTION MyProcedure PRIVATE
         INPUT  param1    PIC 9(04)
         OUTPUT outParam1 PIC 9(04).
         PROCEDURE DIVISION.
          ...
          .
       END-DECLARE.
       DECLARE FUNCTION MyPublic PUBLIC
         INPUT param1 PIC 9(04)
         RETURNING result TYPE bool.
         PROCEDURE DIVISION.
          ...
          .
       end-declare.
```
is translated into this COBOL 85 code:
```cobol
       PROGRAM-ID. MyProcedure.
         DATA DIVISION.
         LINKAGE SECTION.
           01 param1    PIC 9(04).
           01 outParam1 PIC 9(04).
         PROCEDURE DIVISION
           USING BY REFERENCE param1 outParam1
         .
          ...
       END PROGRAM.
       PROGRAM-ID. function-name.
         DATA DIVISION.
         LINKAGE SECTION.
           01 param1 PIC 9(04).
           01 result-value PIC X VALUE LOW-VALUE.
             88 result       VALUE 'T'.
             88 result-false VALUE 'F'.
         PROCEDURE DIVISION
           USING param1
           RETURNING BY REFERENCE result-value
         .
           ...
       END PROGRAM.
```


* __TCRFUN_CODEGEN_AS_NESTED_PROGRAM__ The procedure or function declaration header becomes a program identification header with neither `INITIAL`, `RECURSIVE` or `COMMON` phrase, nor authoring properties. The information regarding the procedure or function access modifier is not translatable and is therefore lost.
* __TCRFUN_CODEGEN_NO_ADDITIONAL_DATA_SECTION__ Each section of the `DATA DIVISION` will only be present in the generated code if it was present in the original code.
* __TCRFUN_CODEGEN_PARAMETERS_IN_LINKAGE_SECTION__ Each `INPUT`, `OUTPUT`, `IN-OUT` or `RETURNING` parameter is generated as an entry of the `LINKAGE SECTION` of the generated nested program, if a data description entry with the same name is not already present.
* __TCRFUN_CODEGEN_DATA_SECTION_AS_IS__ Each entry in a section of the `DATA DIVISION` already present in the TypeCobol source code is translated with no additional consideration than what is described in [TypeCobol Types codegen](Cobol02TYPEDEF#codegen).
* __TCRFUN_CODEGEN_PARAMETERS_ORDER__ All `input`, `in-out`and `output` parameters are translated using the `USING` phrase, in the following order: `USING input-parameter* in-out-parameters* output-parameter* return-code`
* __TCRFUN_CODEGEN_RETURNING_PARAMETER__ The returning parameter is translated using the `RETURNING` phrase.


#Caller
__TCRFUN_CODEGEN_CALL_PRIV_WITH_LITERAL_HASH__
The generated call to a procedure must use its hasname as a literal.

Example:
```cobol
           call Procedure input xxx
           end-call
```
generated code:
```cobol
           call '12345678' using xxx
           end-call
```

__TCCODEGEN_FUNCALL_ALWAYS_ENDCALL__
An `end-call` must always be generated for a procedure call, even if it's missing in the original TypeCobol code.


Ex:
```cobol
           call ProcedureName
```

generated code:
```cobol
           call '12345678'
           end-call
```

__TCCODEGEN_FUNCALL_PARAMS__
Parameters of a generated procedure/function call must generate the following information:
 1. sharing mode 
    * if present in the TypeCobol code
    * if it's the first parameter of `in-out` and `output` a `by reference` must be generated
    * Otherwise it's an implied sharing mode and it doesn't generate anything
 2. Value of the parameter passed in the TypeCobol call

Ex:
```cobol
           call ProcedureName input param1 by content param2 param3 by reference param4
                                 in-out param5 param6
                                 output param7
```

generated code:
```cobol
           call 'hashName' using 
                                  param1
                     by content   param2
                                  param3
                     by reference param4
                     by reference param5
                                  param6
                     by reference param7
```

__TCCODEGEN_FIXFOR_ALIGN_FUNCALL__
The `call` and the `end-call` generated for a procedure/function call must start at column 12 to avoid line truncature.

__TCCODEGEN_FIXFOR_ALIGN_FUNCALL_PARAMS__
For a parameter of a generated call: 
 * if it's sharing mode is present according to rule `TCCODEGEN_FUNCALL_PARAMS` (#380), it's start at column 21
 * The value of the paramter start at column 34 

Ex:
```cobol
           call ProcedureName input param1 by content param2 param3 by reference param4
                                 in-out param5 param6
                                 output param7
```

generated code:
```cobol
           call '12345678' using 
                                 param1
                    by content   param2
                                 param3
                    by reference param4
                    by reference param5
                                 param6
                    by reference param7

```
