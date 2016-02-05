/** 
Open Bank Project - Transparency / Social Finance Web Application
Copyright (C) 2011 - 2016, TESOBE Ltd

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
TESOBE Ltd
Osloer Str. 16/17
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

object CustomEditable {

  import net.liftweb.http.js
  import net.liftweb.util.Helpers
  import js.{ jquery, JsCmd, JsCmds, JE }
  import jquery.JqJsCmds
  import JsCmds.{ Noop, SetHtml }
  import JE.Str
  import JqJsCmds.{ Hide, Show }


  val addClass = "add"
  val editClass = "edit"
  val removeClass = "remove"
  val noAliasTooltip = "No alias is set, so the real account name will be displayed."
  val confirmRemoval = "If no alias is set, the real account name will be displayed."

  def dispName(divName: String) : String = divName + "_display"
  def editName(divName: String) : String = divName + "_edit"


  /*
   * Depending on what was clicked only either the "_edit" or the "_display" div is visible
   */
  def swapJsCmd(show: String, hide: String): JsCmd = Show(show) & Hide(hide)
  def setAndSwap(show: String, showContents: => NodeSeq, hide: String): JsCmd =
    (SHtml.ajaxCall(Str("ignore"), { ignore: String => SetHtml(show, showContents) })._2.cmd & swapJsCmd(show, hide))


  /*
   * The edit markup consists of the input field (editForm), a submit button and a cancel button
   * Clicking on either of buttons will swap back to the display markup, the submit button will save the data of the input field
   */
  def editMarkup(label : => String, editForm: => NodeSeq, onSubmit: () => JsCmd, onDelete: () => Unit, defaultValue: String, divName: String, removable: Boolean): NodeSeq = {

    val formData: NodeSeq =
      editForm ++ <br />
        <input class="submit" style="float:left;" type="image" src="/media/images/submit.png"/> ++
        SHtml.hidden(onSubmit, ("float", "left")) ++
        <input type="image" src="/media/images/cancel.png" onclick={ swapJsCmd(dispName(divName), editName(divName)).toJsCmd + " return false;" }/>

    SHtml.ajaxForm(formData,
      Noop,
      setAndSwap(dispName(divName), displayMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName, removable), editName(divName)))
  }


  /*
   * The display markup shows the label (default value if none is set), an edit button and for some fields (currently only the alias fields) also a remove button
   * Clicking on the edit button will swap to the edit markup
   * Clicking on the remove button will pop up a confirmation window
   */
  def displayMarkup(label : => String, editForm: => NodeSeq, onSubmit: () => JsCmd, onDelete: () => Unit, defaultValue: String, divName: String, removable: Boolean): NodeSeq = {

    label match {
      case "" => {
        if(removable){
          <div title={ noAliasTooltip } >
            <a href="#" class={ editClass } onclick={ setAndSwap(editName(divName), editMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName, removable), dispName(divName)).toJsCmd + " return false;" } />
            <br/><span class="text-add-edit">{ defaultValue }</span>
          </div>

        } else{
          <div>
            <a href="#" class={ addClass } onclick={ setAndSwap(editName(divName), editMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName, removable), dispName(divName)).toJsCmd + " return false;" }>
              { " " ++ defaultValue }
            </a>
          </div>
        }
      }
      case _ => {
        <div>
          <a href="#" class={ editClass } onclick={ setAndSwap(editName(divName), editMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName, removable), dispName(divName)).toJsCmd + " return false;" }/>
          { if (removable)
            <a href="#" class={ removeClass } onclick={ removeAlias("", editForm, onSubmit, onDelete, defaultValue, divName, removable) } />
          }
          <br/><span class="text">{ label }</span>
        </div>
      }
    }
  }


  /*
   * Pop-up window when removing alias: on approval deletes alias and empties value in input field of the edit markup
   */

  def removeAlias(label: String, editForm: NodeSeq, onSubmit: () => JsCmd, onDelete: () => Unit, defaultValue: String, divName: String, removable: Boolean): String = {

    def removalConfirmed: JsCmd = {
      SHtml.ajaxInvoke(() => {
        onDelete()
        var empty = ""
        setAndSwap(dispName(divName), displayMarkup(empty, SHtml.text(empty, empty = _), onSubmit, onDelete, defaultValue, divName, removable), editName(divName))
      })._2.cmd
    }

    val confirmationPopup = JsCmds.Confirm(confirmRemoval, removalConfirmed)
    confirmationPopup
  }

  /*
   *
   */

  def editable(label : => String, editForm: => NodeSeq, onSubmit: () => JsCmd, onDelete: () => Unit, defaultValue: String, removable: Boolean): NodeSeq ={
    val divName = Helpers.nextFuncName

    <div>
      <div id={ dispName(divName) }>
        { displayMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName, removable) }
      </div>
      <div id={ editName(divName) } style="display: none;">
        { editMarkup(label, editForm, onSubmit, onDelete, defaultValue, divName, removable) }
      </div>
    </div>
  }
}
