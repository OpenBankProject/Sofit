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
          var $feedback = $("#feedBack").removeClass("feedback-bg");
          $feedback.find('.polarize-input').show();
          $feedback.find('input[type="text"], textarea').val("");
          $feedback.find('.thanks').hide();
          $feedback.animate({left:"0px"});
          return false;
      },
      function(){
          $("#feedBack").addClass("feedback-bg").animate({left:"-195px"});
          return false;
      }
  );
  $('#feedback-angel').submit(function() {
    var $this = $(this);
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

  $('#feedback-demon').submit(function() {
    var $this = $(this);
    if ($.trim($this.find("input[type=text]").val()).length !== 0) {
      $.ajax({
          url: $(this).attr("action"),
          data: $(this).serialize(),
          success: function() {
              $this.find(".polarize-input").hide().parent().append("<p class='thanks'>Thank you to be a demon!</p>");
          }
      });
    }
    return false;
  });

  $('#feedback-idea').submit(function() {
    var $this = $(this);
    if ($.trim($this.find("textarea").val()).length !== 0) {
      $.ajax({
          url: $(this).attr("action"),
          data: $(this).serialize(),
          success: function() {
              $this.find(".polarize-input").hide().parent().append("<p class='thanks'>Thank you for you Ideas!</p>");
          }
      });
    }
    return false;
  });

});


