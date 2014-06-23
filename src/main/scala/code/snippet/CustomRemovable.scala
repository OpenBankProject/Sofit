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

import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.SHtml
import scala.xml.NodeSeq
import net.liftweb.util.Helpers

object CustomRemovable extends CustomEditable {

  val removeClass = "remove"
  val noAlias = "No alias is set, so the real account name will be displayed."

  def displayRemoveMarkup(label : => String, editForm: => NodeSeq, onSubmit: () => JsCmd, onDelete: () => Unit, defaultValue: String, divName: String): NodeSeq = {
    val dispText = displayText(label, defaultValue)
   
    label match {
      case "" => {
        <div title={ noAlias } >
          <a href="#" class={ editClass } onclick={ setAndSwap(editName(divName), editRemoveMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName), dispName(divName)).toJsCmd + " return false;" } />
          <br/><span class="text-add-edit">{ dispText }</span>
        </div>
      }
      case _ => {
        <div>
          <a href="#" class={ editClass } onclick={ setAndSwap(editName(divName), editRemoveMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName), dispName(divName)).toJsCmd + " return false;" }/>
          <a href="#" class={ removeClass } onclick={ removeAlias("", editForm, onSubmit, onDelete, defaultValue, divName) } />
          <br/><span class="text">{ label }</span>
        </div>
      }
    }
  }

  def editRemoveMarkup(label : => String, editForm: => NodeSeq, onSubmit: () => JsCmd, onDelete: () => Unit, defaultValue: String, divName: String): NodeSeq = {
    val formData: NodeSeq =
      editForm ++ <br />
          <input type="image" src="/media/images/submit.png" class="submit" style="float:left;"/> ++
        SHtml.hidden(onSubmit, ("float", "left")) ++
          <input type="image" src="/media/images/cancel.png" onclick={ swapJsCmd(dispName(divName), editName(divName)).toJsCmd + " return false;" }/>

    SHtml.ajaxForm(formData,
      Noop,
      setAndSwap(dispName(divName), displayRemoveMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName), editName(divName)))
  }

  def removeAlias(label: String, editForm: NodeSeq, onSubmit: () => JsCmd, onDelete: () => Unit, defaultValue: String, divName: String): String = {

    def removalConfirmed: JsCmd = {

      SHtml.ajaxInvoke(() => {
        onDelete()
        var empty = ""
        setAndSwap(dispName(divName), displayRemoveMarkup(empty, SHtml.text(empty, empty = _), onSubmit, onDelete, defaultValue, divName), editName(divName))
      })._2
    }

    val conf = JsCmds.Confirm("If no Alias is selected, the real account name will be displayed.", removalConfirmed)
    conf
  }


	def editable(label : => String, editForm: => NodeSeq, onSubmit: () => JsCmd, onDelete: () => Unit, defaultValue: String): NodeSeq = {
    val divName = Helpers.nextFuncName

    <div>
      <div id={ dispName(divName) }>
        { displayRemoveMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName) }
      </div>
      <div id={ editName(divName) } style="display: none;">
        { editRemoveMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName) }
      </div>
    </div>
  }
}
