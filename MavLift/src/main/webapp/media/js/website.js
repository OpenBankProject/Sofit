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
          $("#feedBack").find('form').show();
          $("#feedBack").find('.thanks').hide();
          $("#feedBack").animate({left:"0px"});
          return false;
      },
      function(){
          $("#feedBack").animate({left:"-195px"});
          return false;
      }
  );
  $('#feedback-angel').submit(function() {
    if ($.trim($("#feedback-angel input[type=text]").val()).length !== 0) {
      $.ajax({
          url: $(this).attr("action"),
          data: $(this).serialize(),
          success: function() {
              $('#feedback-angel').find(".polarize-input").hide().parent().append("<p class='thanks'>Thank you to be an angel!</p>");
          }
      });
    }
    return false;
  });

  $('#feedback-demon').submit(function(e) {
    if ($.trim($("#feedback-angel input[type=text]").val()).length !== 0) {
      $.ajax({
          url: $(this).attr("action"),
          data: $(this).serialize(),
          success: function() {
              $('#feedback-demon').find(".polarize-input").hide().parent().append("<p class='thanks'>Thank you to be a demon!</p>");
          }
      });
    }
    return false;
  });

  $('#feedback-idea').submit(function() {
    if ($.trim($("#feedback-angel textarea").val()).length !== 0) {
      $.ajax({
          url: $(this).attr("action"),
          data: $(this).serialize(),
          success: function() {
              $('#feedback-idea').find(".polarize-input").hide().parent().append("<p class='thanks'>Thank you for you Ideas!</p>");
          }
      });
    }
    return false;
  });

});


