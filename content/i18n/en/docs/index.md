---
id: index
title: Docs
---

Welcome to the TypeCobol documentation!

&nbsp;

TypeCobol is a superset to [COBOL 85](https://en.wikipedia.org/wiki/COBOL).

Following features are implemented:

- [Type mechanism](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolTypesNutshell)
  - TypeCobol comes with intrinsic types: [Boolean](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolTypeBool), [Date](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolTypeDate), DateDB2, ...
- [Fonctions and procedures](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolFunctionNutshell)
- [Operator `::`](https://github.com/TypeCobolTeam/TypeCobol/wiki/TypeCobolNameQualification) which allow to qualify a variable starting with the top most variable
  - Same behavior as operators `of` and `in`, but you have to start with the parent variable

&nbsp;

TypeCobol allow you to use these new keywords. TypeCobol code is then translated to Cobol 85.

We also provide [minimal integration with RDZ](https://github.com/TypeCobolTeam/TypeCobol/wiki/RDZPreprocessor)
