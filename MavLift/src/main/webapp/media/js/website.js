$(document).ready(function() {
    $.ajaxSetup({
      statusCode: {
          200: function() {
            console.log("it works");
            console.log(data);
          },
          403: function() {
            console.log("no permission");
            console.log(data);
          },
          404: function() {
            console.log("page not found");
            console.log(data);
          }
        },
      type: "POST"
    });

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
  $('#feedback-angel').submit(function() {
    $.ajax({
        url: $(this).attr("action"),
        data: $(this).serialize(),
        success: function() {
            $('#feedback-angel').html("<p>Thank you!</p>");
        }
    });
    return false;
  });

  $('#feedback-demon').submit(function(e) {
    $.ajax({
        url: $(this).attr("action"),
        data: $(this).serialize(),
        success: function() {
            $('#feedback-demon').html("<p>Thank you!</p>");
        }
    });
    return false;
  });

  $('#feedback-idea').submit(function() {
    $.ajax({
        url: $(this).attr("action"),
        data: $(this).serialize(),
        success: function() {
            $('#feedback-idea').html("<p>Thank you!</p>");
        }
    });
    return false;
  });

});


