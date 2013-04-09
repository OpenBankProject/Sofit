;(function ( $, window, document, undefined ) {

    var methods = {
        init : function( options ) {
            this.each(function() {
                var settings = $.extend( {
                    "direction": "left",
                    "polarize_url": "",
                    "directions": {
                        "left": {
                            "container-style": "left: -195px;background-position: 200px 130px;",
                            "slider-style": "float:right;",
                            "content-style":"float:left;",
                            "link-style": "text-align:left;",
                            "animation": {
                                "show": '{"left":"0", "height": "250px"}',
                                "hide": '{"left":"-195px", "height": "120px"}'
                            }
                        },
                        "right": {
                            "container-style": "right: -195px;background-position: 10px 130px;",
                            "slider-style": "float:left;",
                            "content-style":"float:right;",
                            "link-style": "text-align:right;",
                            "animation": {
                                "show": '{"right":"0", "height": "250px"}',
                                "hide": '{"right":"-195px", "height": "120px"}'
                            }
                        }
                    }
                }, options);
                createUI(settings);
                createEvents(settings);
            });
        }
    };

    function add_direction_style(settings) {
        return settings.directions[settings.direction];
    }

    function createUI(settings) {
        // Create root element
        $( "<div></div>", {
            "id": "feedback",
            "class": "feedback-bg",
            "style": add_direction_style(settings)["container-style"]
        }).prependTo("body").parent().css("overflow", "hidden");

        // Create slider
        $( "<div></div>", {
            "class": "feedback-slider",
            "style": add_direction_style(settings)["slider-style"]
        }).appendTo("#feedback");

        $( "<div></div>", {
            "id": "feedback-content",
            "style": add_direction_style(settings)["content-style"]
        }).appendTo("#feedback");

        // Create ad anchor to polarize with an image
        $( "<a></a>", {
            "class": "polarize-anchor",
            "target": "_blank",
            "href": "http://polarize.it",
            "style": add_direction_style(settings)["link-style"]
        }).appendTo("#feedback-content");
        $( "<img></img>", {
            "alt": "Polarize Logo",
            "src": "/media/images/polarize-logo.png"
        }).appendTo(".polarize-anchor");

        // Create forms
        // Angel form
        $( "<form></form>", {
            "action": settings.polarize_url,
            "method": "post",
            "id": "feedback-angel"
        }).appendTo("#feedback-content");
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
            "action": settings.polarize_url,
            "method": "post",
            "id": "feedback-demon"
        }).appendTo("#feedback-content");
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
            "action": settings.polarize_url,
            "method": "post",
            "id": "feedback-idea"
        }).appendTo("#feedback-content");
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

    function createEvents(settings) {
      $(".feedback-slider").toggle(
          function(){
              var $feedback = $("#feedback").removeClass("feedback-bg");
              $feedback.find('.polarize-input').show();
              $feedback.find('input[type="text"], textarea').val("");
              $feedback.find('.thanks').hide();
              $feedback.animate(JSON.parse(add_direction_style(settings)["animation"]["show"]));
              return false;
          },
          function(){
              $("#feedback").addClass("feedback-bg").animate(JSON.parse(add_direction_style(settings)["animation"]["hide"]));
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
