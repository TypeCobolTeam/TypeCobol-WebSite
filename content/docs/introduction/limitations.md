---
title: Limitations
---

# Current limitations that we are looking to fix

## Current known bugs that will be fixed soon
* Continuation line doesn't  works if the last character of the previous line ends with a quote (`'`).

## Current known bugs
* `EXEC SQL INCLUDE` on multiple line.  `EXEC SQL INCLUDE` on the same line works fine.
* We do not check if a symbol is an UPSI Switch
* Symbolic characters, class name
* Renames
* COPY instruction without a terminal `.`
* Recursive programs
* Replace of the following forms doesn't works.
```cobol
000000 REPLACE ==UBININT-2== BY ==PIC 9==.              
000000 LOCAL-STORAGE SECTION.                                                                                 
000000 01 var UBININT-2.    
```

Following Replace is working:
```cobol
000000 REPLACE ==:MAX:== BY ==200==.              
000000 LOCAL-STORAGE SECTION.                                                                                 
000000 01 array.
000000    05 elt pic X occurs :MAX:.
```

## Controls not done
* Check if usage of variable with occurs respect occurs declaration
* Most of the semantic checks



# Things that will certainly remains forever
### Cobol parsing known limitations

* Some old syntax are not supported (ex: `COPY SUPPRESS copyname`) we are based on Cobol V5 specs.

**The following cases are allowed by Cobol reference but will not be accepted:**
* Paragraph and variables identifiers which contains only numeric characters. For example, `01.` is a valid data description entry but can also be a paragraph name.

* Partial Cobol word
* User defined words of the form 123E-4, 123-4X or 266-24HOURS are valid according to the spec but will not be supported by this compiler. These cases are considered highly improbable, but we will have to check on a large body of existing programs. Purely numeric paragraph and section names are not supported.  

**Additional limitations:** (as discussed in [#59](https://github.com/TypeCobolTeam/TypeCobol/issues/59))
* Because of the internal conversion of the program text to Unicode characters in .Net or Java, we do not support alphanumeric literals containing non printable EBCDIC characters
* Because of the feature allowing free text format and variable line length, we do not support alphanumeric literals containing line ending characters
* __TCLIMITATION_NO_CE_ACROSS_SOURCES__  A cobol statement (Cobol85, 2002, 2014 or TypeCobol) can't be written across more than 1 source file because it'll be too complicated to generate the code across theses source files.

### Potential problems with TypeCobol
* Do not declare the same literal replaced twice. See an example here: https://github.com/TypeCobolTeam/TypeCobol/issues/320