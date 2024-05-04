# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

Be aware that this project is still v0.y.z which means that anything can change anytime:

> "4. Major version zero (0.y.z) is for initial development. Anything MAY change at any time. The
> public API SHOULD NOT be considered stable."
>
> (Semantic Versioning Specification)

## Indicating incompatible changes on major version zero

We defined for this project that while being on major version zero we mark incompatible changes with
new minor version numbers. Please note that this is no version handling covered by `Semver`.

## 0.1.0 - Not released yet

To make the generation of parse tables more flexible we added a way to control this process.
The trait `Config` is used for this and a custom implementation can be provided to deviate from the
default.
To keep the default behavior as in previous versions the user can use the `DefaultConfig` structure
that provides the implementation of the standard behavior.
