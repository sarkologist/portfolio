# a JavaScript library
## for wrapping event handlers and associated callbacks for UI indication of asynchronous requests
this is a JavaScript library for wrapping UI event handlers and callbacks associated with the asynchronous HTTP requests they make.
It puts up a loading indicator and disables form submission during the duration of the async request, but otherwise leaves page functionality unchanged.
There was previously no such general functionality, and only ad-hocly implemented in the pages where accidental form submission or slow async request were discovered to have been a problem.

### documentation
The documentation for client code resides in the file AsyncLoadingStatus.pdf
