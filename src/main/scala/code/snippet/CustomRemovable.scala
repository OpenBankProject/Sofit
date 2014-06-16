/** 
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011, 2012, TESOBE / Music Pictures Ltd

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Email: contact@tesobe.com 
TESOBE / Music Pictures Ltd 
Osloerstrasse 16/17
Berlin 13359, Germany

  This product includes software developed at
  TESOBE (http://www.tesobe.com/)
  by 
  Simon Redfern : simon AT tesobe DOT com
  Stefan Bethge : stefan AT tesobe DOT com
  Everett Sochowski : everett AT tesobe DOT com
  Ayoub Benali: ayoub AT tesobe DOT com
  Nina Gänsdorfer: nina AT tesobe.com
 */
package code.snippet

import net.liftweb.http.SHtml
import scala.xml.NodeSeq
import net.liftweb.http.js.JsCmd
import scala.xml.Text

object CustomRemovable extends CustomEditable {

  val removeClass = "remove"
  val noAlias = "No alias is set, so the real account name will be displayed."

  def displayRemoveMarkup(label : => String, editForm: => NodeSeq, onSubmit: () => JsCmd, defaultValue: String): NodeSeq = {

    val dispText = displayText(label, defaultValue)
   
    label match {
      case "" => {
        <div title={ noAlias } >
          <a href="#" class={ editClass } onclick={ setAndSwap(editName, editMarkup(label, editForm, onSubmit, defaultValue), dispName).toJsCmd + " return false;" } />
          <br/><span class="text-add-edit">{ dispText }</span>
        </div>
      }
      case _ => {
        <div>
          <a href="#" class={ editClass } onclick={ setAndSwap(editName, editMarkup(label, editForm, onSubmit, defaultValue), dispName).toJsCmd + " return false;" }/>
          <a href="#" class={ removeClass } onclick="window.alert('delete this!')" />
          <br/><span class="text">{ label }</span>
        </div>
      }
    }
  }

	override def editable(label : => String, editForm: => NodeSeq, onSubmit: () => JsCmd, defaultValue: String): NodeSeq = {

    <div>
      <div id={ dispName }>
        { displayRemoveMarkup(label, editForm, onSubmit, defaultValue) }
      </div>
      <div id={ editName } style="display: none;">
        { editMarkup(label, editForm, onSubmit, defaultValue) }
      </div>
    </div>
  }
}
