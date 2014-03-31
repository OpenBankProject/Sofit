$(document).ready(function(){
  $(".delete").click(function(){
    alert("You want to delete the view \""+$(this).attr("data-id")+"\"?");
  });

  /* clicking on edit */
  $(".edit").click(function(){
  $viewId = $(this).attr("data-id")

    /* permissions and is public checkboxes get activated */
    $(".permission_value_cb").each(function(i){
      if($(this).attr("data-viewid") == $viewId){
        $(this).removeAttr("disabled")
      }
    })

    $(".is_public_cb").each(function(i){
      if($(this).attr("data-viewid") == $viewId){
        $(this).removeAttr("disabled")
      }
    })

    /* edit button will become a save button, delete button will become a cancel button */


    /* description become editable */
    $(".description").each(function(i){
      if($(this).attr("data-viewid") == $viewId){
        $content = $(this).html()
        $(this).html("<input type='text' value='"+$content+"'>")
      }
    })

    /* alias field will become a select box */
    $(".alias").each(function(i){
      if($(this).attr("data-viewid") == $viewId){
        $content = $(this).html()
        console.log($content)
        $(this).html(
          "<select id='selectAlias'>"+getOptions()+"</option></select>"
        )
      }
      $("#selectAlias option[value="+$content+"]").attr('selected',true)
    })

   function getOptions () {
      $aliasOptions = new Array("public", "private", "")
      $option = ""
      for($a in $aliasOptions){
       $option += "<option value='"+$aliasOptions[$a]+"'>"+$aliasOptions[$a]+"</option>"
      }
      return $option
   };

  });
})