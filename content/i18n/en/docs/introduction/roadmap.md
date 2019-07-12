---
title: Roadmap
---

Here is a list of features we would like to be implemented, grouped by milestone.


# Version 1.0 - Types and procedures (June 2017)
## User features

### TypeCobol language
* Type (weaked and strong) - specs comes from Cobol 2002 + Type `Strict` close to Strong
* Intrinsic type based on Cobol 2002 type: Date, bool
* `::` operator which is used as the dot operator in modern language
* Procedures
    * Shorter syntax to declare a procedure than a sub-program
    * Control caller arguments during compilation time (partially)
         * Control only number of arguments and their type
    * Write clearly which arguments serves as input, output or both
    * Propose visibility mechanism: only public and private for now

### Code generation
* Generate Cobol 85 code from a TypeCobol source file

### IDE integration
* Preprocessor integration for RDz
* POC for integration with RTC (https://github.com/TypeCobolTeam/TypeCobolBuild)

#### Propose a Console Interface
* Parse a Cobol file to check for errors
* Convert a TypeCobol source file to a Cobol 85 file

## Technical Considerations
* Cobol 85 parser with lexical, syntactic and semantic analysis
* Grammer of Cobol 85 is almost complete
* Semantic error are very limited : basically we check if variable are defined
* Cobol 2002 grammar only for `Typedef`
* Incremental parsing is done till syntactic phase


# Version 1.1 - Implement Language Server (March 2018)
Implement LanguageServer protocol for : 
* parsing in real time
* completion
* error in real time
* go to definition


# Version 1.2 - Optimization (June 2018)
* Migration from Antlr to CUPS. Semantic phase is now 10x faster.

# Version 1.3 - Namespace support (end of 2018)