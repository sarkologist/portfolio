# portfolio

## Haskell
### foci: composable bundles of traversals
- [gist](https://gist.github.com/sarkologist/4206ece148cbbe302ae4f341fcf687a4)
- [blog post](https://tech-blog.capital-match.com/posts/4-json-migration.html)

### streaming closest distances
This reads csv lines and streams out the closest distances on a trajectory encountered so far to a given point.

### item counter
This script parses a csv-esque file for a list of items, then counts the items, then prints out the item counts in descending order.

## Java/Javascript libraries
These are some code I have written at a previous place of work.
They are libraries I have written to factor out repeated code I have encountered in the codebase.
They are meant to showcase my understanding of coding and library design in the natural context of the work I have done.

### the JavaScript library
One is a JavaScript library for wrapping UI event handlers and callbacks associated with the asynchronous HTTP requests they make.
It puts up a loading indicator and disables form submission during the duration of the async request, but otherwise leaves page functionality unchanged.
There was previously no such general functionality, and only ad-hocly implemented in the pages where accidental form submission or slow async request were discovered to have been a problem.

### the Java library
The other one is a Java convenience library for outputting the HTML source for comboboxes. The preexisting library code for this was a mess of copy-and-paste methods catering to variants on combobox contents. I have reduced it to two natural methods called in the fluent style, with zero loss of specific functionality. The code reduction is a factor of ten, though the old code has been omitted to protect the guilty.

#### notes on the Java code
- The code was the best I could conceive respecting:
  - The use of old (pre-ECMAScript 5 and pre- Java 1.5) versions of the languages
  - The prevailing quality and level of abstraction of client code
  - The prevailing conventions within the team
  - my level of competence :D
- The purpose of the above disclaimer is not to indict my previous employer but solely to place the code in the context in which it was written
