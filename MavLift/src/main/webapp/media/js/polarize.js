;(function ( $, window, document, undefined ) {

    var methods = {
        init : function( options ) {

            this.each(function() {
                var settings = $.extend( {
                    "location": "left",
                    "polarize_url": ""
                }, options);
                createUI(options);
                createEvents();
            });
        }
    };

    function createUI(options) {
        // Create root element
        $( "<div></div>", {
            "id": "feedBack",
            "class": "feedback-bg"
        }).prependTo("body");

        // Create slider
        $( "<div></div>", {
            "class": "feedback-slider"
        }).appendTo("#feedBack");

        // Create ad anchor to polarize with an image
        $( "<a></a>", {
            "class": "polarize-anchor",
            "target": "_blank",
            "href": "http://polarize.it"
        }).appendTo("#feedBack");
        $( "<img></img>", {
            "alt": "Polarize Logo",
            "src": "/media/images/polarize-logo.png"
        }).appendTo(".polarize-anchor");

        // Create forms
        // Angel form
        $( "<form></form>", {
            "action": options.polarize_url,
            "method": "post",
            "id": "feedback-angel"
        }).appendTo("#feedBack");
        $( "<input></input>", {
            "type": "hidden",
            "name": "redirect",
            "value": "NO"
        }).appendTo("#feedback-angel");
        $( "<input></input>", {
            "type": "hidden",
            "name": "who",
            "value": "ANGEL"
        }).appendTo("#feedback-angel");
        $( "<input></input>", {
            "type": "text",
            "name": "opinion_text",
            "required": "required",
            "class": "polarize-input",
            "placeholder": "Things you like"
        }).appendTo("#feedback-angel");
        $( "<input></input>", {
            "type": "submit",
            "class": "polarize-input",
            "value": "Praise it!"
        }).appendTo("#feedback-angel");

        // Demon form
        $( "<form></form>", {
            "action": options.polarize_url,
            "method": "post",
            "id": "feedback-demon"
        }).appendTo("#feedBack");
        $( "<input></input>", {
            "type": "hidden",
            "name": "redirect",
            "value": "NO"
        }).appendTo("#feedback-demon");
        $( "<input></input>", {
            "type": "hidden",
            "name": "who",
            "value": "DEMON"
        }).appendTo("#feedback-demon");
        $( "<input></input>", {
            "type": "text",
            "name": "opinion_text",
            "required": "required",
            "class": "polarize-input",
            "placeholder": "Things you don't like"
        }).appendTo("#feedback-demon");
        $( "<input></input>", {
            "type": "submit",
            "class": "polarize-input",
            "value": "Badmouth it!"
        }).appendTo("#feedback-demon");

        // Idea form
        $( "<form></form>", {
            "action": options.polarize_url,
            "method": "post",
            "id": "feedback-idea"
        }).appendTo("#feedBack");
        $( "<input></input>", {
            "type": "hidden",
            "name": "redirect",
            "value": "NO"
        }).appendTo("#feedback-idea");
        $( "<input></input>", {
            "type": "hidden",
            "name": "who",
            "value": "THINKER"
        }).appendTo("#feedback-idea");
        $( "<textarea></textarea>", {
            "name": "opinion_text",
            "required": "required",
            "class": "polarize-input",
            "placeholder": "Ideas you have!"
        }).appendTo("#feedback-idea");
        $( "<input></input>", {
            "type": "submit",
            "class": "polarize-input",
            "value": "Send your ideas!"
        }).appendTo("#feedback-idea");
    }

    function createEvents() {
      $(".feedback-slider").toggle(
          function(){
              var $feedback = $("#feedBack").removeClass("feedback-bg");
              $feedback.find('.polarize-input').show();
              $feedback.find('input[type="text"], textarea').val("");
              $feedback.find('.thanks').hide();
              $feedback.animate({left:"0px", height:"250px"});
              return false;
          },
          function(){
              $("#feedBack").addClass("feedback-bg").animate({left:"-195px", height:"120px"});
              return false;
          }
      );
      $('#feedback-angel').submit(function() {
        var $this = $(this);
        if ($.trim($("#feedback-angel input[type=text]").val()).length !== 0) {
          $.ajax({
              type: "POST",
              url: $(this).attr("action"),
              data: $(this).serialize(),
              success: function() {
                  $('#feedback-angel').find(".polarize-input").hide().parent().append("<p class='thanks'>Thank you for being positive!</p>");
              }
          });
        }
        return false;
      });

      $('#feedback-demon').submit(function() {
        var $this = $(this);
        if ($.trim($this.find("input[type=text]").val()).length !== 0) {
          $.ajax({
              type: "POST",
              url: $(this).attr("action"),
              data: $(this).serialize(),
              success: function() {
                  $this.find(".polarize-input").hide().parent().append("<p class='thanks'>Thank you for being negative!</p>");
              }
          });
        }
        return false;
      });

      $('#feedback-idea').submit(function() {
        var $this = $(this);
        if ($.trim($this.find("textarea").val()).length !== 0) {
          $.ajax({
              type: "POST",
              url: $(this).attr("action"),
              data: $(this).serialize(),
              success: function() {
                  $this.find(".polarize-input").hide().parent().append("<p class='thanks'>Thank you for your ideas!</p>");
              }
          });
        }
        return false;
      });
    }

    $.fn.polarize = function(method) {
        if ( methods[method] ) {
                return methods[ method ].apply( this, Array.prototype.slice.call( arguments, 1 ));
            } else if ( typeof method === 'object' || ! method ) {
                return methods.init.apply( this, arguments );
            } else {
                $.error( 'Method ' +  method + ' does not exist on jQuery.tooltip' );
            }
      };

})( jQuery, window, document );
