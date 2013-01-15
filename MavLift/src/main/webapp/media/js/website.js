$(document).ready(function() {
  $(".feedback-slider").toggle(
      function(){
          $("#feedBack").animate({left:"0px"});
              return false;
      },
      function(){
          $("#feedBack").animate({left:"-195px"});
          return false;
      }
  );
  $('#feedback-angel').submit(function(e) {
    e.preventDefault();
    $.post($(this).attr("action"), $(this).serialize(), function(data) {
      console.log("it works");
      console.log(data);
    });
  });

  $('#feedback-demon').submit(function(e) {
    $.post($(this).attr("action"), $(this).serialize(), function(data) {
      console.log("it works");
      console.log(data);
    });
    return false;
  });

  $("#feedback-idea .submit-button").click(function() {
    e.preventDefault();
    $('#feedback-idea').submit(function(e) {
      $.post($(this).attr("action"), $(this).serialize(), function(data) {
        console.log("it works"),
        console.log(data);
      });
    });
  });
});
