# Changelog for `noodle`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.1.0] - 2026-04-04

### 🚀 Features

- Initial Commit
- Successfully retrieved field names
- _(serialize)_ Start serializing any type
- _(serialization)_ Finished basic serialization
- _(serialization)_ Added generic serialization capabilities
- _(serializer)_ Add abstracted serializer
- _(serialization)_ Add toml file format
- _(json)_ Start basic json parsing
- _(json)_ Finished full json parser
- _(deserialization)_ Start implementing deserialization
- _(deserialization)_ Data selectors now works
- _(deserializer)_ Implement deserializer class for yaml and toml
- _(yaml/parser)_ Still doesnt work but I understand more the identation part
- _(yaml)_ Advance parsing of yaml
- _(yaml)_ Setup basic arrays parsing
- _(yaml)_ Add basic object parsing
- _(yaml)_ Add nested objects parsing
- Indented objects
- _(yaml)_ Add nested object parsing
- _(yaml)_ Expand numbers representation
- _(yaml/strings)_ Add more string parsing cases

### 🐛 Bug Fixes

- _(json-parser)_ Json parser was failling on blank lines
- Remove some warnings

### 🚜 Refactor

- _(serializer)_ Move things around
- Change file organization
- _(deserialization)_ Remove unuseful state monad
- _(types)_ Reorganize the way types are handled

### 📚 Documentation

- Write code documentation
- Update readme with more informations

### 🎨 Styling

- Run fourmolu
- Fix formatting issues

### 🧪 Testing

- _(serializer)_ Add json serializing test
- _(serialization)_ Add unit tests for toml and yaml
- _(Serializer)_ Create test serializer that takes any serialer
- _(deserializer)_ Add deserializer testing

### ⚙️ Miscellaneous Tasks

- Edit cabal file to have necessary information
- Fix hls warnings
- Add workflow for tests
- _(actions)_ Wrong branch name
- _(actions)_ Disable as its too long
- Test a stack ci
- _(build)_ Remove extensions arguments in favor of language pragma
- Finish testing ci
- Extend actions
