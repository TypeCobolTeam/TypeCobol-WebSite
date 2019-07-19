---
title: FAQ
redirectFrom:
  - /faq
---

#### Does your parser have exactly the same behaviour as the IBM Enterprise Cobol 5.1 compiler for zOS ?

Short answer: no.

Currently, we cannot exhibit exactly the same behaviour for the following two reasons :

- Currently, we first want to implement the full width of features of an incremental Cobol compiler.
  Once we have brought the technical proof that is is both doable and technically convincing, we will then implement the full depth of each remaining feature.
  Implementing the _whole list_ of IBM compiler errors is one of these remaining tasks, wich is not completed ... for now.
- Our Cobol parser does have the following [[limitations|Limitations]].

---

#### Are you planning to include embedded SQL / CICS into your compiler?

Writing a full parser for a language other than Cobol or TypeCobol is out of our scope.
However, we want to provide extension points so our parser can delegate some of the work to other existing parsers
(more details [[here|https://github.com/TypeCobolTeam/TypeCobol/issues/43#issuecomment-163233361]]).

---

#### If your primary goal is to integrate your parser in Eclipse/RDz, why did you choose C# over Java ?

1. First, for pragmatic reasons: as we wanted to clearly separate our parser from GUI-specific concerns, it has to be able to live as a server.
   So, why chose a language only suited for one of its purposes ?
2. Second, for legacy reasons. The prototype initaly written by **[@laurentprudhon](https://github.com/laurentprudhon)** was full C#, and we did stick with it.
3. Third, because our organization is more used to work in C# than in Java.

To be completely honest, we thought about converting from C# to Java. [[Here|CSharp2Java]] are some [[proofs|https://github.com/TypeCobolTeam/TypeCobol/issues?q=milestone%3A%22[CANCELED]+C%23+%3E+Java+conversion%22]].
However, we dismissed this solution for the reasons listed up there.

---

#### See, I've got this problem or question, and your FAQ doesn't even talk about it!

Well, you can always [[create an issue|https://github.com/TypeCobolTeam/TypeCobol/issues/new]], but please read [[our guidelines|DevGuidelines]] first. See you soon!

---

#### Hey, that is awesome ! Can I help?

Why, thanks a lot! Of course you can, please [[check out how|DevHowTo]].
