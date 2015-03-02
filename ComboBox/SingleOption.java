package qrcom.PROFIT.webbean.HTTPObj.ComboBox;

class SingleOption extends Option
{
  String value;
  String text;

  SingleOption(String value, String text)
  {
    this.value = value;
    this.text = text;
  }

  void write(ComboBox comboBox)
  {
    appendHtmlOption(comboBox, value, text);
  }
}