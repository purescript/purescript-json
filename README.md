# purescript-json

[![Build status](https://github.com/purescript/purescript-json/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-json/actions?query=workflow%3ACI+branch%3Amaster)

Standard types and basic operations for working with JSON.

For efficiency and performance reasons this library provides an interface for working with JSON without using PureScript ADTs, and instead operates on the underlying representation.

## Differences from Argonaut

This library is similar to the traditionally used `argonaut-core` library, but has been implemented with an eye to making it backend agnostic. As such, it does not use `Foreign.Object` as the representation for JSON objects, does not use `Array JSON`, and instead provides its own `JObject` and `JArray` types.
