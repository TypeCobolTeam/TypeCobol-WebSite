---
title: Errors
---

**Rules detailled here are not implemented yet**

## How to represent an error

- **TCERR_INTRINSIC_TYPE** Error are represented with the intrinsic type _ErrorCode_. This type is defined like this:

```cobol
01 ErrorCode TYPEDEF STRONG.
   05 Code pic X(04).
```

## Error variable

- **TCERR_IMPLICIT_ERRORCODE_VAR** All TypeCobol programs can access an implicit variable always named "ErrorCode". This variable is declared with a private scope (not GLOBAL or EXTERNAL). This variable is for internal use and to call other functions/procedure. This variable can't be received as a parameter (not declared in linkage section).

- **TCERR_IMPLICIT_ERRORCODE_PARAM1** All functions and procedures have a special parameter named ErrorCode. If another parameter is also named "ErrorCode", there must be an error.
  All call to a function or procedure must pass this ErroCode variable.

- **TCERR_IMPLICIT_ERRORCODE_PARAM2** After the call to a function or procedure, you can then use the syntax:

```cobol
Call getCurrentDate returning xxx
If getCurrentDate::ErrorCode not = OK
    error, by default call the paragraph used to manage error.
Else
    Ok, continue
End-if
```

## Error management for functions and procedures

- **TCERR_IMPLICIT_PARAGRAPH** A paragraph used to manage error must be present in all programs, functions and procedures. By default, this paragraph is implicit and is always named `TC-STANDARD-ERROR-MANAGEMENT`. If another paragraph have the same name, there must be an error.

- **TCERR_OVERRIDE_IMPLICIT_PARAGRAPH** The implicit paragraph used to manage error can be changed. The sentence `error procedure is *MY-PARAGRAPH*` must be declared in identification division _TODO specify exactly where_

- When a function is called and there is an error, the paragraph used to manage error must be called. This is the job of the codegen, TODO see page xxxxx.

### Expected errors

- **TCERR_DECLARE_EXPECTED_ERRORS** Function and procedure can declare error codes that the caller should test.
  These are errors which callee know that they are likely to occur.
  Syntax to declare error codes is:

```cobol
declare function MyFunction PUBLIC
     input
     returning
     error ErrorCode::Code-NotFound ErrorCode::Code-DatabaseNotAvailable '7894'

end-declare
```

`Error` keyword must be followed by either:

- ErrorCode::Code 88 level values
- Literal of length 4

* **TCERR_ERROR_INSIDE_CALL** A call to a function or procedure can include code to manage errors.

```cobol
Call myFunction input xxxx
                returning xxxx
   on error
      when ErrorCode::Code-NotFound
         xxx
      when ErrorCode::Code-DatabaseNotAvailable
         xxxx
      when ErrorCode::Code-Fatal
      when 'U748'
         xxxx
      when other
       IMPLICIT_ERROR_PARAGRAPH
End-call
```

Note that `when` clause can only contains a ErrorCode::Code 88 level values or a literal.
There is no link between error declared by the function or procedure and `when` clause specified in the callee.
Error declared by function or procedure are just a guide for the callee.
An IDE can use this declarations to autocomplete the `on error` statement.
If not specified, the following `on error` statement is implied:

```cobol
   on error
      when other
       IMPLICIT_ERROR_PARAGRAPH
```

TODO: This is the job of the CodeGen, see page xxx
