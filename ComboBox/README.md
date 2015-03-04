# a Java library
## for outputting the HTML source for comboboxes
The preexisting library code for this was a mess of copy-and-paste methods catering to variants on combobox contents. I have reduced it to two natural methods called in the fluent style. 

### note
- Please note that the non-use of generics among other design decisions was because our team was using Java 1.4.
- The code has been edited slightly to remove obscuring details
- The code reduction is a factor of ten
- I did not include the mess this replaces because it would reveal too much of the codebase
