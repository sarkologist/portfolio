package qrcom.PROFIT.webbean.HTTPObj.ComboBox;

class AllOption extends Option
{
  void write(ComboBox comboBox)
  {
    appendHtmlOption(comboBox, "ALL", "All");
  }
}