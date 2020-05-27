# Contributing to Bintracker

Welcome! Glad to hear you want to contribute to Bintracker. From testing and reporting bugs, writing tutorials and demotracks, to setting up infrastructure and writing code, there is no shortage of work to be done.

Beware that the Bintracker API is not yet stable. Nevertheless, we encourage you to already start writing plugins etc., and will provide assistance when a future update breaks your code.


## How to...

### Find a task to do

We always need user testing, so simply playing around with Bintracker and reporting bugs and bad user experience is a great way to help. The same goes for creating tutorials and how-to's. Last but not least, if you make a song with Bintracker, feel free to contribute it to the library of demo tunes.

Looking for a specific task to tackle? Have a look at our [TODO List](TODO.md). Any TODO item marked with **[*]** is potentially a good task to start with, given some knowledge in the relevant disciplines.
Otherwise, if you want to do some code, a [plugin](writing-plugins.md) is always a good idea.


### Report bugs or suggest new features/improvements

Open a ticket on the [Issue Tracker](https://github.com/bintracker/bintracker/issues). If using Github is not an option for you, you can also use the [contact form](https://bintracker.org/contact/).


### Get help

Same as above, open a ticket on the Issue tracker or get in touch via the contact form.


### Commit code to the official repository

Open a pull request on the [Github repository](https://github.com/bintracker/bintracker), using a feature branch. If you do want to use Github, get in touch through the [contact form](https://bintracker.org/contact/), and we'll set up commits via email for you.

The [style guide](style-guide.md) contains a few recommendations that you should stick to when commiting code to the main repository.


### Chose a license for my contributions

When you contribute plugins or engine definitions to the official repository, use a MIT, BSD, Apache, LGPL, or a similar license, or commit your work to the public domain. Bintracker's core components are licensed under MIT terms.

The reason GPL is rejected for official plugins and extensions is because chiptunes, GPL, and artistic licenses like the [Creative Commons](https://creativecommons.org/) family do not mix well. In practise, it forms a legal jungle that we cannot reasonably expect users to penetrate.

For engine definitions, GPL is problematic because it prevents resulting works (chiptunes) from being licensed under [Creative Commons](https://creativecommons.org/) terms, as is common practise in the Chipmusic scene. The key issue is that chiptunes are not audio recordings - they contain the actual player code of the engine definition. This would mean that works should be licensed under GPL terms. However, Create Commons licenses are generally [not compliant](https://wiki.creativecommons.org/wiki/ShareAlike_compatibility:_GPLv3) with GPL terms.

Plugins may be automatically compiled as shared libraries and linked at runtime. This means that the GPL license of a plugin would spread to Bintracker proper when distributed as part of the main source. This again would impact user's choice regarding the licensing of created works.


### Become a Bintracker team member

Currently, the team consists mostly of myself, ([utz](https://irrlichtproject.de)). However, my plan is to pass the project into community hands eventually. Therefore, I would be more than happy to see a team of regular contributors grow in the coming years, and I'll do my best to make it happen.
