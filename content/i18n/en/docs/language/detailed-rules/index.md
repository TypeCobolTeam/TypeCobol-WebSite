---
title: Detailed Rules
---

In our wiki, a detailed rules is identifier by an unique name starting with `TC` followed by an identifier related to the theme.

- `TCNS` for namespace
- `TCRFUN` for function and procedure

For Codegen rules, we add: `_CGEN` just after the prefix:

- `TCNS_CGEN` for codegen of namespace
- `TCRFUN_CGEN` for codegen of function and procedure

For rules specific to Euro-Information, we add `_EI` just after the prefix:

- `TCNS_EI` for EI rules of namespace
- `TCRFUN_EI` for EI rules of function and procedure

Visually we use bold and italic:
\_**TCNS_ONLY_MAIN_CAN_DECLARE_NS**\_

A rule is also written in the C# source code so we can quickly identify where a rule is implemented.
