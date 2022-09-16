# Agora :classical_building:

Agora is a set of Plutus scripts that compose together to form a governance system.

### What is Agora

Goals:

-   Agora aims to reduce duplication in Liqwid and XplorerDAO and to serve as a one-size-fits-all governance library for projects on the Cardano blockchain.
-   Agora aims to be modular and flexible for specific needs but presents an opinionated architecture.

Non-goals:

-   Agora is not a DAO. It doesn't have tokenomics or even a token. It is simply a library for governance.
-   Agora doesn't aim to provide any primitive tools for Plutus that are not governance-specific. For this, see [liqwid-plutarch-extra](https://github.com/Liqwid-Labs/liqwid-plutarch-extra/).

## Project setup

An up to date version of the [Nix package manager](nixos.org) (>=2.3) is required to build this project. For information on how to install, see the [NixOS website](https://nixos.org/download.html). Important: see also [this section](https://github.com/input-output-hk/plutus#nix-advice) on binary caches.

Open a development shell with `nix develop` and build the project with `cabal build`. Those pained by the need to remember to enter a Nix shell may consider using [nix-direnv](https://github.com/nix-community/nix-direnv).

## `agora-scripts` HTTP export server

To use scripts in a frontend, you can use the `agora-scripts` executable which allows you to query them on-demand.

The CTL repo [`agora-offchain`](https://github.com/mlabs-haskell/agora-offchain) already has the setup prepared for this feature.

In order to run the server, simply run the following command:

```sh
cabal run agora-scripts -- --enable-cors-middleware
```

## Documentation

Documentation for Agora is hosted on Notion. You can find the specs [here](https://liqwid.notion.site/e85c09d2c9a542b19aac8dd3d6caa98b?v=d863219cd6a14082a661c4959cabd6e7).

### Using Agora for your protocol

If you are a protocol wanting to use Agora, read [Using Agora](https://liqwid.notion.site/Using-Agora-74ceb4a70d024992abd9ff07087013e6).

## Contributing

Please read [CONTRIBUTING.md](./CONTRIBUTING.md). Additionally, please follow the [Git policy](https://liqwid.notion.site/Git-Policy-9a7979b2fd5d4604b6d042b084e7e14f) when contributing to this project.

## Overview of components

<p align="center">
  <img src="/docs/diagrams/gov-overview.svg"/>
</p>

## Road-map

### v1

-   [x] Governor
-   [x] Treasury
-   [x] Stakes
-   [x] Proposals
-   [x] Effects

### v2

-   [ ] Flexible scripts using TxT pattern integrated with governance
-   [ ] Different voting mechanisms

### Available support channels info

You can find help, more information and ongoing discusion about the project here:

- The [Agora & Liqwid Libs Discord](https://discord.gg/yGkjxrYueB) - Most Agora discussion happens here.
