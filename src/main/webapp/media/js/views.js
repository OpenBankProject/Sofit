$(document).ready(function(){

  $('#view-edit-advanced-options').click(function(){
	  var thisButton = $(this)
	  
	  if(thisButton.text() === "Show advanced options") {
		  
		  $('.advanced-option').css("visibility", "visible")
		  
		  thisButton.text("Hide advanced options");
	  } else {
		  
		  $('.advanced-option').css("visibility", "hidden")
		  
		  thisButton.text("Show advanced options");
	  }
  })

  /* clicking on edit: change view to edit mode for selected view */
  $(".edit").click(function(){
    var $viewId = $(this).attr("data-id")

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

    /* make Save / Cancel / Delete unclickable for not selected columns */
    var $actionButtons = $(".action")
    for(i=0; i<$actionButtons.length; i++){
        var $action = $($actionButtons[i])
        if($action.attr("data-id") != $viewId)
            $action.removeAttr("onclick").css("color", "grey").css("cursor", "default")
    }

    /* edit button disappears, save and cancel button appear */
    $(".edit_head").hide();
    $(".save_head").show();
    $(".cancel_head").show();

    /* description become editable */
    $(".description").each(function(i){
      if($(this).attr("data-viewid") == $viewId){
        var $content = $(this).html()
        $(this).html("<input type='text' value='"+$content+"'>")
      }
    })

    /* alias field will become a select box */
    $(".alias").each(function(i){
      if($(this).attr("data-viewid") == $viewId){
        var $content = $(this).html()
        $(this).html(
          "<select id='selectAlias'>"+getOptions()+"</option></select>"
        )
      }
      $("#selectAlias option[value="+$content+"]").attr('selected',true)
    })

   function getOptions () {
      var aliasOptions = new Array("public", "private", "")
      var option = ""
      for(a in aliasOptions){
       var alias = aliasOptions[a]
       option += "<option value='"+alias+"'>"+alias+"</option>"
      }
      return option
   };

  });

    /* clicking on cancel: reload the page */
    $(".cancel").click(function(){
        location.reload();
    });

})

/* clicking on save: save changes and reload the page */
    var collectData = function(viewId){
        var saveJson = new Object();
        saveJson.viewId = viewId;
        var viewData = new Object();
        viewData.description = $(".description[data-viewId=" + viewId + "] input").val();
        viewData.which_alias_to_use = $(".alias[data-viewId=" + viewId + "] select").val();
        viewData.is_public = $(".is_public_cb[data-viewId=" + viewId + "]").is(':checked');
        viewData.hide_metadata_if_alias_used = $(".hide_metadata_if_alias_used[data-viewId=" + viewId + "]").is(':checked');

        var $permissions = $("input.permission_value_cb");
        var allowedActions = new Array();
        for(i=0; i < $permissions.length; i++){
            var $permission = $($permissions[i])
            if($permission.attr("data-viewid") == viewId && $permission.is(':checked')){
               allowedActions.push($permission.attr("name"));
            }
        }
        viewData.allowed_actions = allowedActions;
        saveJson.updateJson = viewData;
        return JSON.stringify(saveJson);
    //  console.log("Then reload the page.")
    };

