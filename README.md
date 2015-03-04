# portfolio

These are some code I have written at my previous place of work.
They are libraries I have written to factor out repeated code I have encountered in the codebase.

## the JavaScript library
One is a JavaScript library for wrapping UI event handlers and callbacks associated with the asynchronous HTTP requests they make.
It puts up a loading indicator and disables form submission during the duration of the async request, but otherwise leaves page functionality unchanged.
There was previously no such general functionality, and only ad-hocly implemented in the pages where accidental form submission or slow async request were discovered to have been a problem.

## the Java library
The other one is a Java convenience library for outputting the HTML source for comboboxes. The preexisting library code for this was a mess of copy-and-paste methods catering to variants on combobox contents. I have reduced it to two natural methods called in the fluent style. The code reduction is a factor of ten, though the old code has been omitted to protect the guilty.
