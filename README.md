# portfolio

## Scala
### Protobuf to BigQuery converter
converts protobuf schemas and values to Google Bigquery
[repo](https://github.com/sarkologist/protobuf-to-bigquery)

### Beam InfluxDB writer
library to enhance Beam pipelines with ability to write metrics to InfluxDB
[repo](https://github.com/sarkologist/influx-beam)

### Solace ZIO wrapper
implements the Solace message bus API and request/response with ZIO in order to for concurrency/fault-tolerance
[repo](https://github.com/sarkologist/solace-zio)


## Haskell
### WIP: composable "lazy" parse-transforms
composable like parser combinators, but
- "lazy" in the sense of not parsing more structure than necessary
  - e.g. markdown: don't parse the italic inside the header if you are only interested in the raw text inside, but otherwise does if you need to transform it
- bidirectional: not only parse but render
- fusion: does all parse-transform-render in one pass!
- optics-based: everything is a traversal, so it is compatible with most `lens` combinators

see examples here: https://github.com/sarkologist/text-transforms/blob/master/tests/MarkdownLazyTest.hs

code is here: https://github.com/sarkologist/text-transforms/blob/master/src/LazyParseTransforms.hs

### foci: composable bundles of traversals
- [gist](https://gist.github.com/sarkologist/4206ece148cbbe302ae4f341fcf687a4)
- [blog post](https://tech-blog.capital-match.com/posts/4-json-migration.html)

### free applicative config parsing of environment variables
- [gist](https://gist.github.com/sarkologist/5dff67cb05759e438f08605de12db4ba)

### streaming closest distances
This reads csv lines and streams out the closest distances on a trajectory encountered so far to a given point.

### item counter
This script parses a csv-esque file for a list of items, then counts the items, then prints out the item counts in descending order.

## python
### composable folds in python
- [gist](https://gist.github.com/sarkologist/a4903853748f3c7948e0df4a48b3af46)
- in the fashion of: [blog post by Gabriel Gonzalez](http://www.haskellforall.com/2013/08/composable-streaming-folds.html)

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
