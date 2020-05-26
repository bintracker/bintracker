# How To Contribute

Welcome!

## How to...

#### Report a bug or suggest an improvement

Go to the [Issue Tracker](https://github.com/bintracker/bintracker/issues) on Github. If using Github is not an option for you, you can also use the [contact form](https://bintracker.org/contact/).


#### Get started as a new contributor

Have a look at our [TODO List](TODO.md). Any TODO item marked with **[*]** could be a good newcomer's task, given some knowledge in the relevant disciplines.


#### Properly format my code

Check out the [style guide](style-guide.md).


#### Chose a license for my code

When you contribute plugins or engine definitions to the official repository, use a MIT, BSD, Apache, LGPL, or a similar license, or commit your work to the public domain. Bintracker's core components are licensed under MIT terms.

The reason GPL is rejected for official plugins and extensions is because chiptunes, GPL, and artistic licenses like the [Creative Commons](https://creativecommons.org/) family do not mix well. In practise, it forms a legal jungle that we cannot reasonably expect users to penetrate.

For engine definitions, GPL is problematic because it prevents resulting works (chiptunes) from being licensed under [Creative Commons](https://creativecommons.org/) terms, as is common practise in the Chipmusic scene. The key issue is that chiptunes are not audio recordings - they contain the actual player code of the engine definition. This would mean that works should be licensed under GPL terms. However, Create Commons licenses are generally [not compliant](https://wiki.creativecommons.org/wiki/ShareAlike_compatibility:_GPLv3) with GPL terms.

Plugins may be automatically compiled as shared libraries and linked at runtime. This means that the GPL license of a plugin would spread to Bintracker proper when distributed as part of the main source. This again would impact user's choice regarding the licensing of created works.
