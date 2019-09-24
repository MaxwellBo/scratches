# TODO


- Rename Sam's API description to "Skedulo File API"
- Add 200 responses to the Files API
- Add the `@ApiModel` annotation to the `FilesMeta` class
- Write documentation for `SafeController.scala`
- Add string type parameter [Link](https://bitbucket.org/skedulo/elasticserver/commits/35e3b036bafb144f681b25dd089ad61514e233d4?at=dev#comment-4320513)
- Figure out where the wartremover is setup and initialized
- `delete` -> `deleteUFBFilterSet` -- brings in line with swagger documentation
- `delete` -> `deleteTimeslots`
- `createOrUpdate`
- Check whether `hasDuplicate` in `TimeslotDao` isn't wrong (tbh i don't understand the implementation)
- `CalenderDao` -> `CalenderConfigDao` OR ` CalendarConfigController` -> `CalendarController` 
- Nuke the `upsert` method from `TimeslotDao`
- Get `TimeslotDao.delete` to return ()
- Does `UFBConfigDao.createOrUpdate` 
- Bring all `UFBConfigDao` `createOrUpdate` in-line with the controller
- All the `*Daos` refer to `getConfigs` - should this be diferent?

- Make the error `case object` top level [Link](https://bitbucket.org/skedulo/elasticserver/pull-requests/89/els-221-rework-endpoints-to-return-json-or/diff)
- is the null pragma on? is the `var` pragma on?


- `<script src='https://www.google-analytics.com/analytics.js' async defer></script>` --> `async` AND `defer`? What the heck is this
- `UA-57861004-8`

- `/Users/Max/work/skedulocomplete/frontend/javascripts/analytics.js`

```javascript
ga('set', 'forceSSL', true);
    ga('set', 'screenResolution', window.screen.width + "x" + window.screen.height);
    ga('set', 'viewportSize', window.innerWidth + "x" + window.innerHeight);
```


---

### Notes

`method`, `path` `headers`?, `body`?, `description`

`headers: { Content-Type }` for both `request` and `response` can 


- `sbt publishLocal`

The current version of Scala we use is 2.11.8

The Swagger endpoint is `http://localhost:9000/swagger/<SCHEMA>`

To get the JWT token, use `copy(JSON.parse(localStorage.getItem('authInfo')).idToken)`

`Ctrl` + `Shift` + `P` yields the type of the expression

`Command` + `P` yields the types of the parameters required for a function

`Command` + `Enter` shows intention actions and quick-fixes

`Editor -> Code Style -> Scala -> Wrapping and Braces tab` -> untick `align when multiline`

---

### Questions

Q: What do `@field` annotations do?

Q: Do you normally wildcard import from the companion object (`import <COMPANION._>`), or namespace it?
A: No

Q: What is `circe.json[DeviceInfo])`

Q: What is `ResultOkEmpty` doing? Is `toResult` just a bind action?

Q: What does the question mark in URLs do, for example `finish?code=<CODE>`
A: It's a part of a query string, and is a useful way to name parameters

Q: Why do some `@Provides` provide object references, wheras others are just class
definitions

Q: Should I prefer `compose` over `andThen`

Q: Do you have a style guide? How is it enforced? How do I enforce it?

Q: What's the difference between `Try` and `Either`

Q: Are mixins bad practise?

Q: What does `Command` + `Shift` + `L` do?

---

### Links

- [New Engineer Onboarding](https://skedulo.atlassian.net/wiki/display/EN/New+Engineer+Onboarding)

- [FreeSpec](http://www.scalatest.org/at_a_glance/FreeSpec)

- [IntelliJ Keyboard Shortcut Reference](https://resources.jetbrains.com/storage/products/idea/docs/IntelliJIDEA_ReferenceCard.pdf)

---

### To Research

- EQL
