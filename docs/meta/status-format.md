# Spec Status Format

This document specifies a format used to denote the 'status' of a spec document.

## Overview

Each specification document should be headed by a simple table outlining aspects of its status in relation to:

-   Authorship: who has contributed to it?
-   Ownership: who is responsible for it?
-   Implementation(s): where is it used?
-   Completeness: is it done yet?

This format should be used at the start of any documentation that may be considered a _technical specification_, with a related implementation. For anything conceptual, information regarding implementations and completeness may be omitted.

## Format

In Markdown, the format is as follows, with {substitutions} in curly braces.

| Spec Status | Impl Status | Last Revision |
|-------------|-------------|---------------|
| {status}    |  {status}   | {version}, {date} |

--------------------

**Spec Ownership:** [{owner name}]

**Authors**: [{author name}], [{owner name}]

**Impl Owner:** [{impl owner name}]
 
**Current Status**:
 
{Short description of status}

[{owner name}]: {github url}
[{author name}]: {github url}
[{impl owner name}]: {github url}

```markdown
| Spec Status | Impl Status | Last Revision |
|-------------|-------------|---------------|
| {status}    |  {status}   | {version}, {date} |

--------------------

**Spec Ownership:** [{owner name}]

**Authors**: [{author name}], [{owner name}]

**Impl Owner:** [{impl owner name}]
 
**Current Status**:
 
{Short description of status}

[{owner name}]: {github url}
[{author name}]: {github url}
[{impl owner name}]: {github url}

--------------------
```

### Spec/Impl Status

The Spec and Impl status should be one of the following:

-   `WIP`: Work In Progress, currently incomplete, pending current or future work by the current owner or a future owner.
-   `Draft`: Complete but pending further evaluation or changes to be accounted for in the future.
-   `Final`: Complete, and finalised to some degree of certainty.

### Last Revision

-   version - An (optional) version/revision number for the spec document.
-   date - date the document was last updated. (ISO 8601 format)

### Authors

The authors/contributors of the spec document.

### Spec Ownership

The person currently, or most recently tasked with writing/maintaining the spec document.

### Impl Owner

The person currently or most recently tasked with the implementation of the feature/s described in the document.

-   For individual features, this will be the person currently/previously assigned to related GitHub Issues.
-   For broader sections, this will be a person leading the implementation efforts for the particular system.

## Example

| Spec Status | Impl Status | Last Revision |
|-------------|----------------|---------------|
| WIP         |  Draft      | 1.0, 2021-10-20 |

***

**Spec Ownership:** [@owner]

**Authors**: [@author1], [@author2]

**Impl Owner:** [@developer]

**Current Status**:

Draft completed in project repo.
Spec needs revisiting to address issues outlined in [#100](etc).

[@owner]: URL

[@author1]: URL

[@author2]: URL

[@developer]: URL

***
