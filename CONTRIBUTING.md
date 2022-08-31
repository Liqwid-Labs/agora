# Contributing

This document is intended for those whom wish to contribute to Agora, in the form of submitting issues or writing pull requests (PR). Thank you! The Agora core team is delighted to have community members contribute to our project.

Before making any form of contribution, it is advised that one familiarises themselves with the [existing documentation](https://liqwid.notion.site/e85c09d2c9a542b19aac8dd3d6caa98b?v=d863219cd6a14082a661c4959cabd6e7). This will enable the contributor to submit better, more informed issues and will potentially aid a developer in writing PRs.

Please follow the [Git policy](https://liqwid.notion.site/Git-Policy-9a7979b2fd5d4604b6d042b084e7e14f) when contributing to this project.

## Agora core team

This document will make reference to the _Agora core team_. These are the people who work on Agora professionally and will be responsible for maintaining the project in its open source life. They include:

- [Emily Martins](https://github.com/emiflake)
- [Jack Hodgkinson](https://github.com/jhodgdev)

## Issues

An _issue_ is a post on the Agora [Issues page](https://github.com/Liqwid-Labs/agora/issues). An issue may pertain to:

- A bug.
- A desired feature.
- A question one has that is not covered within documentation.

Before submitting an issue, please check that the same issue has not already been provided by another contributor. Such an issue could be _open_ (unresolved) or _closed_ (considered resolved). If it is open, please comment with your perspective on the issue. If it is closed, please read-through what has been posted on the issue page. If you believe the issue is still unresolved, feel free to re-open it along with a post explaining your reasons for doing so.

If your issue has _not_ been submitted hitherto, please submit a new issue. To assist the Agora community please provide _as much_ detail as you feel is relevant. For bugs, _please_ include instructions on how to reproduce the issue and provide any terminal outputs. Please remember to tag your issue with GitHub's labelling system.

Top-tier issues include a _minimal reproducible example_. This should take the form of a public GitHub repository containing _only the code required to reproduce the issue_. Alongside a link to such a repository, please detail steps on how a maintainer may recreate the issue on their system.

If you wish to work to resolve the issue, the Agora team would invite you to submit a PR.

## Pull requests

Only those within the core Agora team may contribute work to the project directly. If you wish to work on the project, you must [fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo) the repository and submit your changes to your fork. Instructions for getting started with the project may be found in the [README](./README.md). Once the work on your fork is completed, you may submit a PR [here](https://github.com/Liqwid-Labs/agora/pulls).

Before submitting a PR, please write an issue pertaining to the problem that your PR would solve e.g. a bug in the codebase or a missing feature. Read this document's section on _Issues_ and pay particular heed to the paragraph asking contributors to _look for pre-existing issues_. The prior experiences of existing contributors could save you a significant amount of time and effort. It is possible that a number of issues could be solved by your PR. Please reference any issues that would be ameliorated by your PR - including your own issue, if you have written one - clearly. Please label your PR using GitHub's tagging feature. Please state plainly:

- What your PR achieves.
- How your PR works.
- What your PR changes.
- Any aspects of your work that you believe merit especially careful review.

Contributors should expect that if their work is insufficiently documented (either on GitHub or within the codebase) that their PR will not be reviewed by core Agora team members. Contributors should expect that an Agora maintainer may offer constructive feedback and request changes to be made, prior to the PR being incorporated into the project.

### Technical requirements

Agora utilises [Plutarch](https://github.com/plutonomicon/plutarch) and your work must be written with Plutarch, when appropriate. Plutarch can prove _complicated_ but the documentation is extensive. We would encourage you to dive deeply into the documentation, whilst stating that Plutarch's [Tricks.md](https://github.com/Plutonomicon/plutarch/blob/master/docs/Tricks.md) could prove particularly helpful.

### Continuous integration

For your PR to be merged it must pass three automated checks:

1. A [`fourmolu`](https://github.com/fourmolu/fourmolu) formatting check.
2. A [`hlint`](https://github.com/ndmitchell/hlint) linting check.
3. A Cabal build check.

Our custom `fourmolu` rules may be found in the [base of the repository](./fourmolu.yaml). You can ensure that your work will pass CI by:

1. Running `make format` from the included `Makefile`.
2. Running `make lint` from the included `Makefile` and applying any recommendations.
3. Ensuring that `cabal build` terminates successfully on your machine in the provided Nix environment.

## Standards

Agora follows a strict standards to increase consistency, to minimize
legacy impacts, to use proper automated means, and more. The standard document 
can be discovered in [here](https://liqwid.notion.site/Coding-Standards-cd3c430e6e444fa292ecc3c57b7d95eb).

## Documentation

It is worth noting that the codebase is not the only aspect of the project that it is worth contributing to. In the event that one finds the docs unsatisfactory, the Agora team would welcome receiving any issues describing your reservations or PRs pertaining to documentation.

## Conclusion

Many thanks for reading. The Agora core team is delighted to be able to share the project with the Cardano community and we are thrilled by the prospect of collaborating with you all on improving our work.
