package qrcom.PROFIT.webbean.HTTPObj.ComboBox;

abstract class Option
{
  abstract void write(ComboBox instance);

  private static void appendHtmlOption(ComboBox comboBox, String value, String text)
  {
    comboBox
    .appendHtml("<OPTION value=\"")
    .appendHtml(value)
    .appendHtml("\"");

    if (comboBox.hasSelected(value))
      comboBox.appendHtml(" selected");

    comboBox
    .appendHtml(">")
    .appendHtml(text)
    .appendHtml("</OPTION>");
  }
}