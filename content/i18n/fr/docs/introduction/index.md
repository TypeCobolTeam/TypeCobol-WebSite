---
title: Introduction
---

1. [What is TypeCobol?](#goals)
2. [What problems does TypeCobol solve?](#problems)
3. [What are TypeCobol design principles?](#design)
4. [In practice, how does it work ?](#inpractice)
5. [And ... who are you ?](#aboutus)

## <a name="goals">What is TypeCobol?</a>

The TypeCobol project goal is twofold:
* create an [[incremental parser|https://en.wikipedia.org/wiki/Incremental_compiler]] for COBOL
* create a new language, **TypeCobol**, as a superset of COBOL

## <a name="problems">What problems does TypeCobol solve?</a>

### Incremental parser

Our parser will be able to understand the semantics of COBOL, analyze code errors and refactor snippets of code. At first sight, that's nothing especially new. However, by effectively taking into acount __only the lines of code that have changed__, our parser avoids wasting time and computer resource. Processing small changes to the source code is __nearly instantaneous__.

The primary application is of course make this incremental parser available to COBOL developpers via an IDE. The incremental aspect of our tool makes feedback about code not only reliable, but __fast__ and __complete__: our parser can __understand wrong code__, which is vital when talking about code that is currently being written.

But, what is exactly our definition of « wrong » code ?

1. First, it's code that doesn't meet the specifications of [*IBM Enterprise COBOL 5.1 for zOS*](https://github.com/TypeCobolTeam/TypeCobol/raw/master/TypeCobol/Documentation/Reference/IBM%20Enterprise%20Cobol%205.1%20for%20zOS%20-%20Language%20Reference.pdf). We aim at detecting all errors one can write in this specific version of COBOL. Out of the box.

2. Second, it's code that doesn't meet *your* standards. We know that every organization out there has its own custom usage of COBOL, and its own custom QA rules. We'll thus provide extension points, so you can be notified about the detection by our parser of specific code elements. So, you can check these code elements the way you want and add your __custom diagnostics__ (errors, warning, tips and so on) to it.

Integration to any code editor is only the visible tip of the iceberg, however.

The parser builds a **free, complete and documented representation** of Cobol code. Once you know your code and have the hand on it, you can think of other usages : QA, analytics, custom export ... these are just some examples.

Take note however, that we really build a _parser_, not a _compiler_. We want a representation of the source code to be able to analyze it, modify it and generate stuff from it, but generating machine code from it is outside of the scope of this project.

### The TypeCobol language

Are you a Cobol developper ?
Do you wish you hadn't to write always the same snippets of code ?
Do you wish you had some features of modern languages, like strict typing, or functions ?

Well, look no further my friend: here's TypeCobol.
The well-known tast of COBOL, but with sweet and tasty bits of modernity inside! (^‿^v)

## <a name="design">What are TypeCobol design principles?</a>

_TODO_

## <a name="inpractice">In practice, how does it work ?</a>

TypeCobol's incremental parser runs as a server, aptly named **TypeCobol.Server**. Any client (be it an IDE plugin, a custom script, ...) can use its services using a properly documented [[interface|TypeCobolServer]] and make any desired processing with the results. That's it.

As long as it runs, **TypeCobol.Server** knows which resources it has parsed. If you give it only the changes brought to a specific file since last time it parsed it, **TypeCobol.Server** will only re-parse what is necessary and make only the necessary updates to the code abstract representation.

If you chose to write TypeCobol code, **TypeCobol.Server** recognize it, and generates the COBOL code corresponding to your changes.

## <a name="aboutus">And ... who are you ?</a>

The [[TypeCobol|https://github.com/TypeCobolTeam/TypeCobol]] project is maintened by the organization [[TypeCobol Team|https://github.com/TypeCobolTeam]]. Members of the [[Taskforce|https://github.com/orgs/TypeCobolTeam/teams/taskforce]] can freely commit, push and merge, but only [[Owners|https://github.com/orgs/TypeCobolTeam/teams/owners]] can directly act on the organization settings, on teams (_ie._ add or remove members) or on repositories (_ie._ create and delete repositories).
