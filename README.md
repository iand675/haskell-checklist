# Preflight checklist for Haskell code. Aspire to greatness!

Releasing quality code to the public is hard. There are a lot of i's to dot and t's to cross. I'm typically guilty of releasing code once it works without taking the time to polish it to make it user friendly. In an effort to combat that, I present this preflight checklist to help make sure that your code is accessible and sparkles when others gaze upon it.

If you look at half the code I write, I'm guilty of not following a lot of these things. This is my way of trying to become publicly accountable for getting better! I challenge you to do the same!

If there's anything missing, or you don't agree with something, contributions are welcome!

## Initial release checklist

### Code things
- [ ] Do you have 1+ tests? Seriously, write at least one! The rest will usually come more easily after that.
- [ ] Do you have some documentation in each module?
  - [ ] Does your code have examples for anything that's not immediately obvious how to use for the uninitiated?
  - [ ] Does everything that throws an exception document what can be thrown?
- [ ] Do functions provide friendly interfaces to use?
  - [ ] If there are lots of values of the same type being passed around, consider using type synonyms or newtypes to disambiguate them.
  - [ ] Are your functions sufficiently general in their types, but not so general as to be incomprehensible?
- [ ] Do you provide the facilities to extend and add new features on top of the library where possible? This is a bit hard to quantify, but worth thinking about.
- [ ] Some code should explicitly not be extensible. Make sure that you explain why, and do your best to prevent it or at least provide cautionary warnings.
- [ ] Is performance important? If yes, write some criterion benchmarks.
- [ ] Does anything "algorithmic" or "data-structure-y" document it's big-O performance characteristics?
- [ ] Turn on -Wall & -Werror
- [ ] Ensure that the code is consistently formatted to your taste & linted
- [ ] Ensure that the code explicitly exports anything that should be publicly exposed
- [ ] Ensure that your functions are total

### Non-code things
- [ ] Do you have CI set up for your project?
  - [ ] Does your CI run tests against all versions of GHC that your library claims to support (e.g. the version range for `base`)?
    - For Travis, there's [multi-ghc-travis](https://github.com/hvr/multi-ghc-travis) to simplify this.
- [ ] Do you have a README?
  - [ ] Does your README make the license obvious?
  - [ ] Does your README explain what the library does?
  - [ ] Does your README explain why you might want to use it? Or any of the pros/cons?
  - [ ] Does your README provide examples?
  - [ ] Does your README cite prior art or inspiration?
  - [ ] Does your README provide contribution guidelines?
  - [ ] Does your README have badges and links to Haddock docs, CI & other immediately relevant pages?
- [ ] Add a sufficient introduction to the cabal description.
- [ ] Do you have a blog? Write a blog post about it!
- [ ] Ditto for twitter, reddit, whatever else you use... nobody will know about it if you don't point it out.

## Update checklist
- [ ] Re-check the initial release guidelines and update accordingly.
- [ ] Search your code for any TODOs and delete the ones that don't apply anymore.
- [ ] Similarly, close out any issues in your issue tracker that are fixed or no longer relevant.
- [ ] Add changes to the CHANGELOG.
- [ ] If you have contributions to your library from others, add them to a CONTRIBUTORS file
- [ ] Update the version in your cabal file.
- [ ] If you any libraries that build upon this library, ensure that they build, tests pass, and bump versions accordingly if any changes need to be made.
- [ ] If you believe in the [package versioning policy](https://wiki.haskell.org/Package_versioning_policy), update your depency ranges.
- [ ] Tag your release in git / the SCM of your choice
- [ ] `cabal sdist` and `cabal upload`!
- [ ] :beers:

## Ongoing maintenance checklist
- [ ] Can the library be added to Stackage? You probably want to know when breaking changes in your dependencies necessitate a new release!
- [ ] If there's a new release of GHC, does your code still compile with it?
- [ ] If your code makes heavy use of the FFI, are you up-to-date with the latest release of the underlying C/C++/whatever libraries?
- [ ] Are you not interested in the library anymore? Find a new maintainer.
- [ ] Is the code obsolete for some reason? Deprecate it.

