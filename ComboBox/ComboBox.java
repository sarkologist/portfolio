/*
encapsulates what defines a combobox
 - which items
 - in what order
 - which items are selected (support for drop down multi check list)
 */

package qrcom.PROFIT.webbean.HTTPObj.ComboBox;

import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.LinkedHashMap;

public class ComboBox
{
  private final List commands;
  private final Set selected;

  private final StringBuilder html = new StringBuilder();
  private final String output;

  private ComboBox(Builder builder)
  {
    this.commands = builder.commands;
    this.selected = builder.selected;

    for (Iterator i=commands.iterator(); i.hasNext(); )
    {
      ((Option) i.next()).write(this);
    }

    output = this.html.toString();
  }

  public static class Builder
  {
    private List commands = new ArrayList();
    private Set selected = new HashSet();

    public Builder selected(String selected)
    {
      this.selected.add(selected);
      return this;
    }

    public Builder selected(Set selected)
    {
      this.selected.addAll(selected);
      return this;
    }


    public Builder emptyOption()
    {
      commands.add(new EmptyOption());
      return this;
    }

    public Builder allOption()
    {
      commands.add(new AllOption());
      return this;
    }

    public Builder option(String val, String text)
    {
      commands.add(new SingleOption(val, text));
      return this;
    }

    public Builder option(Query query)
    {
      commands.add(new QueryOption(query));
      return this;
    }


    /**
     * a list of value, text pairs with map keys for values and map values for texts
     * as in <option value="key">value</option>
     * @param  map Map<String, String>; for dependable order use an ordered map like LinkedHashMap
     */
    public Builder options(Map map)
    {
      for (Iterator i=map.entrySet().iterator(); i.hasNext(); )
      {
        Map.Entry entry = (Map.Entry) i.next();
        commands.add(new SingleOption((String)entry.getKey(), (String)entry.getValue()));
      }

      return this;
    }

    public Builder options(String serialized)
    {
      return options(deserializeToMap(serialized));
    }


    /**
     * builds the ComboBox instance
     * @return ComboBox instance
     */
    public ComboBox build()
    {
      return new ComboBox(this);
    }
  }

  private static Map deserializeToMap(String serialized)
  {
    Map map = new LinkedHashMap(); // <String,String>

    if (serialized != null) // return empty Map on null
    {
      String[] split_into_options = serialized.split("\\|");

      for (int i=0; i < split_into_options.length; i++)
      {
        String[] split_into_val_text = split_into_options[i].split("-");
        map.put(split_into_val_text[0], split_into_val_text[1]);
      }
    }

    return map;
  }

  ComboBox appendHtml(String s)
  {
    html.append(s);
    return this;
  }

  boolean hasSelected(String candidate)
  {
    return selected.contains(candidate);
  }



  public String print()
  {
    return output;
  }

}



