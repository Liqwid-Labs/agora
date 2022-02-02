# Spec status format

This document specifies a format used to denote the 'status' of a spec document.

## Overview

Each specification document should be headed by a simple table outlining aspects of its status in relation to:

-   Authorship: who has contributed to it?
-   Ownership: who is responsible for it?
-   Implementation(s): where is it used?
-   Completeness: is it done yet?

This format should be used at the start of any documentation that may be considered a _technical specification_, with a related implementation. 

## Format

The format is as follows, with {substitutions} in curly braces.

| Specification | Implementation | Last revision |
|:-------------:|:--------------:|:-------------:|
| {status}      | {status}       | {version} {date} |

--------------------

**Specification ownership:** {[Spec owner]}

**Authors**: 
 -   {[Spec owner]}
 -   {[Author]}

**Implementation ownership:** {[Impl owner]}
 
**Current status**:
 
{Short description of status regarding __both__ specification and implementation}.

[Spec owner]: https://genrandom.com/cats/
[Author]: https://genrandom.com/cats/
[Impl owner]: https://genrandom.com/cats/

```markdown
| Specification | Implementation | Last Revision |
|:-------------:|:--------------:|:-------------:|
| {status}      | {status}       | {version}, {date} |

--------------------

**Specification ownership:** [{owner name}]

**Authors**: 
 -   [{owner name}]
 -   [{author name}]


**Implementation ownership:** [{impl owner name}]
 
**Current status**:
 
{Short description of status}

[{owner name}]: {github url}
[{author name}]: {github url}
[{impl owner name}]: {github url}

--------------------
```

### Specification/Implementation status

The 'Specification' and 'Implementation' status should be one of the following:

-   `WIP`: Work In Progress, currently incomplete, pending current or future work by the current owner or a future owner.
-   `Draft`: Complete but pending further evaluation or changes to be accounted for in the future.
-   `Final`: Complete and finalised to some degree of certainty.

### Last revision

-   version - An optional version/revision number for the spec document.
-   date - date the document was last updated in [ISO 8601 format](https://www.wikiwand.com/en/ISO_8601#/Calendar_dates) (YYYY-MM-DD).

### Authors

The authors and contributors of the spec document.

### Specificiation ownership

The person currently, or most recently tasked with writing and maintaining the spec document.

### Implementation ownership

The person currently or most recently tasked with the implementation of the features described in the document.

-   For individual features, this will be the person most recently assigned to related GitHub issues.
-   For broader sections, this will be a person leading the implementation efforts for the particular system.

## Example

| Specification | Implementation | Last Revision |
|:-------------:|:--------------:|:-------------:|
| WIP   | Draft     | 0.1, 2022-01-31 |

--------------------

**Specification ownership:** [Jack Hodgkinson]

**Authors**: 
 -   [Jack Hodgkinson]
 -   [Emily Martins]

**Implementation ownership:** [Emily Martins]
 
**Current status**:
 
Draft completed in project repo. Spec needs revisiting to address issues outlined in #42. Section on staking pool behaviour is out-dated. 

[Jack Hodgkinson]: https://github.com/jhodgdev
[Emily Martins]: https://github.com/emiflake
***
