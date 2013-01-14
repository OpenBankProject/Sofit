$(document).ready(function() {
	$("#feedBack").toggle(
		function(){
			$("#feedBack").animate({left:"0px"});
				return false;
		},
		function(){
			$("#feedBack").animate({left:"-195px"});	
			return false;
		})
});