package qrcom.PROFIT.webbean.HTTPObj.ComboBox;

class EmptyOption extends Option
{
  void write(ComboBox comboBox)
  {
    appendHtmlOption(comboBox, "", "");
  }
}