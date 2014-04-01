$(document).ready(function(){

  /* clicking on delete: if confirmed delete and reload the page, else nothing */
  $(".delete").click(function(){
    $doDelete = confirm("You want to delete the view \""+$(this).attr("data-id")+"\"?")
    if($doDelete)
        console.log("ok, delete \""+$(this).attr("data-id")+"\" and reload the page.")
  });

  /* clicking on edit: change view to edit mode for selected view */
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

    /* edit button disappears, save and cancel button appear */
    $(".edit_head").hide();
    $(".save_head").show();
    $(".cancel_head").show();

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

  /* clicking on save: save changes and reload the page */
    $(".save").click(function(){
        $viewId = $(this).attr("data-id")
        console.log("Save changes for view '"+$viewId+"' and reload the page.")
        console.log("description: "+$(".description input").val())
        console.log("alias: "+$(".alias select").val())
        console.log("is_public: "+$(".is_public_cb").is(':checked'))
        $permissionObjects = $("input.permission_value_cb")
        for($p in $permissionObjects){
            if($($permissionObjects[$p]).attr("data-viewid") == $viewId){
               console.log($($permissionObjects[$p]).attr("name")+": "+$($permissionObjects[$p]).is(':checked'))
            }
        }
        console.log("Then reload the page.")
    });

    /* clicking on cancel: reload the page */
    $(".cancel").click(function(){
        $viewId = $(this).attr("data-id")
        location.reload();
    });

})