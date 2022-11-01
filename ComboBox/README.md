# a Java library
## for outputting the HTML(JSP) source for comboboxes
The preexisting library code for this was a mess of copy-and-paste methods catering to variants on combobox contents. I have reduced it to two natural methods called in the fluent style. 

### examples

```jsp
<td class="caption">Order Factor Type</td>
<td>
  <select name="FACT_TYPE">
    <%=
    new ComboBox.Builder()
        .emptyOption()
        .option("MIN", "Coefficient for Minimum Quantity")
        .option("MAX", "Coefficient for Maximum Quantity")
        .option("TOP", "Top Sales Item")
        .option("NONTOP", "Non Top Sales Item")
        .option("STK", "Safety Stock Day for Minimum Quantity")

        .selected(FACT_TYPE)
      .build()
      .print()
    %>
  </select>
</td>
```

```jsp
<td class="caption">Store</td>
<td>
 <select name="STORE" style="width:100%">
 <%=
  new ComboBox.Builder()
    .emptyOption()
    .allOption()
    .option(new Query.Builder("strmst","store")
                 .descColumn("store_name")
                 .delColumn("store_del_cd")
                 .build()
                )
    .selected(STORE)
  .build()
  .print()
 %>
 </select>
</td>
```

#### notes
- Please note that the non-use of generics among other design decisions was because our team was using Java 1.4.
- The code has been edited slightly to remove obscuring details
- The code reduction is a factor of ten
- I did not include the mess this replaces because it would reveal too much of the codebase
