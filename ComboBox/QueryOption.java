package qrcom.PROFIT.webbean.HTTPObj.ComboBox;

import java.text.DecimalFormat;
import org.apache.commons.lang.StringUtils;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.ResultSetMetaData;

import qrcom.util.ejb.connection.*;
import qrcom.PROFIT.files.info.AltDescUtil;

class QueryOption extends Option
{
  private Query q;

  QueryOption(Query query)
  {
    this.q = query;
  }

  void write(ComboBox comboBox)
  {
    Connection conn = null;
    PreparedStatement stmt = null;
    ResultSet rs = null;

    try
    {
      conn = DataSource.getLocalConnection();
      stmt = conn.prepareStatement(q.getSql());
      rs = stmt.executeQuery();

      ResultSetMetaData md = rs.getMetaData();

      while ( rs.next() )
      {
        String val = rs.getString(q.optColumn);
        String text = format(md.getColumnType(1), md.getScale(1), rs.getString(q.optColumn));

        if (StringUtils.isNotBlank(q.descColumn))
        {
          String description = format(md.getColumnType(2), md.getScale(2), AltDescUtil.getDesc(q.lang_code, rs.getString(q.descColumn)));
          text = text + " - " + description;
        }

        if (StringUtils.isNotBlank(q.delColumn)
            && StringUtils.equals(rs.getString(q.delColumn), "Y"))
          text = "(D) " + text;

        appendHtmlOption(comboBox, val, text);
      }
    }
    catch (SQLException e)
    {
      e.printStackTrace();
    }
    finally
    {
      try
      {
        rs.close();
        stmt.close();
        conn.close();
        rs = null;
        stmt = null;
        conn = null;
      }
      catch (Exception ignore) {}
    }
  }

  /**
   * This method is to format the numeric value only
   * Example: column format declaration = NUMERIC (5,2)
   * if value is 0.20, DB always return ".2".
   * We want to format it to "0.20"
   */
  private static String format(int colType, int scale, String value)
  {
    if (colType == 2) // NUMBER
    {
      if (scale > 0) // with decimal
      {
        StringBuffer strBuf = new StringBuffer();
        strBuf.append("0.");
        for (int i=0; i<scale; i++) // number of decimal
          strBuf.append("0");
        DecimalFormat df = new DecimalFormat(strBuf.toString());
        return (df.format(Double.parseDouble(value)));
      }
      /* // reserved for future update
               else          // interger
               {
               }
      */
    }

    return (value);
  }
}