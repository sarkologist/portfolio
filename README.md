# portfolio

## Scala
### Protobuf to BigQuery converter
```scala
def makeTableSchema(descriptor: Descriptor): TableSchema

def makeTableRow(msg: Message,
                 customRow: (FieldDescriptor,
                   Yoneda[Repeated, Any]) => Yoneda[Repeated, Any]
                 = { case (_, x) => x })
  : TableRow
```
converts protobuf schemas and values to Google Bigquery:
[repo](https://github.com/sarkologist/protobuf-to-bigquery)
- ensures unabiguous converted bigquery `TableRow`s (notorious protobuf issue)

#### [defined using generic folds](https://github.com/sarkologist/protobuf-to-bigquery/blob/2d13140f9549c29dd1f58a1f2dd21f6ac7af3591/src/main/scala/util/Protobuf.scala#L47)
- fuses multiple transformations on `repeated` values
```scala
  /**
  - recursively traverse `Message`, producing `A`
  - Yoneda is for efficient `.map`-ing.
    it composes mapped functions without applying until `Yoneda.run` is called
  - note that this is a *paramorphism* instead of just a *catamorphism*, i.e.
    each recursive step keeps the `Message` value at that level.
    this is helpful e.g. to tell at runtime if the `Message` is a leaf
   */
  def foldMessage[A](
      recurse: Seq[(Yoneda[Repeated, (A, Message)], FieldDescriptor)] => A,
      base: (AnyRef, FieldDescriptor) => A)(message: Message): A = {
 ```
#### [extensively property tested](https://github.com/sarkologist/protobuf-to-bigquery/blob/main/src/test/scala/utils/ProtobufToBigquerySpec.scala) with ScalaCheck
- generate arbitary protobuf values to test for conversion
- [check that converted value is compatible converted schema](https://github.com/sarkologist/protobuf-to-bigquery/blob/2d13140f9549c29dd1f58a1f2dd21f6ac7af3591/src/test/scala/utils/ProtobufToBigquerySpec.scala#L68)
  - by generating from a generated bigquery `TableRow`, paths to traverse its structure
  - then using that same path to attempt to walk the corresponding `TableSchema`
- [check that distinct protobuf `Message` produce distinct bigquery `TableRow`](https://github.com/sarkologist/protobuf-to-bigquery/blob/2d13140f9549c29dd1f58a1f2dd21f6ac7af3591/src/test/scala/utils/ProtobufToBigquerySpec.scala#L186)
  - generate paths from two generated `Message`s
  - walk the converted `TableRow`s using those paths
  - we should expect different values

### Beam InfluxDB writer
library to enhance Beam pipelines with ability to write metrics to InfluxDB
[repo](https://github.com/sarkologist/influx-beam)

### Solace ZIO wrapper
implements the Solace message bus API and request/response with ZIO in order to for concurrency/fault-tolerance
[repo](https://github.com/sarkologist/solace-zio)


## Haskell
### WIP: composable partial parse-transforms
composable like parser combinators, but
- "partial" in the sense of not parsing more structure than necessary, contra a full parse
  - e.g. markdown: don't parse the italic inside the header if you are only interested in the raw text inside, but otherwise does if you need to transform it
- bidirectional: not only parse but render
- fusion: does all parse-transform-render in one pass! like with a hylomorphism!
- optics-based: everything is a traversal, so it is compatible with most `lens` combinators

see examples here: https://github.com/sarkologist/text-transforms/blob/master/tests/MarkdownLazyTest.hs

code is here: https://github.com/sarkologist/text-transforms/blob/master/src/LazyParseTransforms.hs

### foci: composable bundles of traversals
- [gist](https://gist.github.com/sarkologist/4206ece148cbbe302ae4f341fcf687a4)
- [blog post](https://tech-blog.capital-match.com/posts/4-json-migration.html)
- works with the `lens` library

### **free applicative** config parsing of environment variables
- [gist](https://gist.github.com/sarkologist/5dff67cb05759e438f08605de12db4ba)
- like `optparse-applicative` but for environment variables

### streaming closest distances
- reads csv lines and streams out the closest distances on a trajectory encountered so far to a given point
- uses `pipes` library for streaming

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
