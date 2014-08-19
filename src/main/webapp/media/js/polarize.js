;(function ( $, window, document, undefined ) {

    var methods = {
        init : function( options ) {
            this.each(function() {
                var settings = $.extend( {
                    "position": "left",
                    "default_topic_url": "",
                    "images_dir": "",
                    "topic_list_url": "",
                    "positions": {
                        "left": {
                            "container-style": "left: -195px;background-position: 200px 130px;border-radius: 0 5px 5px 0;",
                            "slider-style": "float:right;",
                            "content-style":"float:left;",
                            "animation": {
                                "show": '{"left":"0", "height": "300px"}',
                                "hide": '{"left":"-195px", "height": "120px"}'
                            }
                        },
                        "right": {
                            "container-style": "right: -195px;background-position: 10px 130px;border-radius: 5px 0 0 5px;",
                            "slider-style": "float:left;",
                            "content-style":"float:right;",
                            "animation": {
                                "show": '{"right":"0", "height": "300px"}',
                                "hide": '{"right":"-195px", "height": "120px"}'
                            }
                        }
                    }
                }, options);
                if (settings.topic_list_url !== "") {
                    jQuery.getJSON("http://polarize.it/api/v0.1/poll/" + settings.topic_list_url, function(data) {
                        var title = $("title").text();
                        jQuery.each(data, function() {
                            if (title === this.body) {
                                settings.default_topic_url = "http://polarize.it/polarize/" + this.topic_permalink;
                            }
                        });
                        createUI(settings);
                        createEvents(settings);
                    });
                }
                else {
                    createUI(settings);
                    createEvents(settings);
                }

            });
        }
    };

    function add_position_style(settings) {
        return settings.positions[settings.position];
    }

    function elemCreate(type, content, attrs) {
       /* type: string tag name
        * content: string element content
        * attrs: associative array of attrs and values
        */
        elem = '<' + type + '></' + type + '>';
        e = $(elem).attr(attrs);
        e.append(content);
        return e[0];
    }

    function createUI(settings) {
        var html_container = [],
            html_inner = [],
            html_inner_content = [],
            html_form_content =  [];


        // Anchor
        html_inner_content.push(elemCreate("a", "", {
            "class": "polarize-anchor",
            "target": "_blank",
            "href": settings.default_topic_url,
            "title": "Show Feedback on Polarize.it",
            "style": "background-image: url(" + settings.images_dir + "polarize-logo.png);background-position:" + settings.position + ";"
        }));

        // Create forms
        // Angel form
        html_form_content.push(elemCreate("input", "", {
            "type": "hidden",
            "name": "redirect",
            "value": "NO"
        }));
        html_form_content.push(elemCreate("input", "", {
            "type": "hidden",
            "name": "who",
            "value": "ANGEL"
        }));
        html_form_content.push(elemCreate("input", "", {
            "type": "text",
            "name": "opinion_text",
            "required": "required",
            "class": "polarize-input",
            "placeholder": "Things you like"
        }));
        html_form_content.push(elemCreate("input", "", {
            "type": "submit",
            "class": "polarize-input",
            "value": "Praise it!"
        }));
        html_inner_content.push(elemCreate("form", html_form_content, {
            "action": settings.default_topic_url,
            "method": "post",
            "id": "feedback-angel"
        }));

        html_form_content.length = 0;

        // Demon form
        html_form_content.push(elemCreate("input", "", {
            "type": "hidden",
            "name": "redirect",
            "value": "NO"
        }));
        html_form_content.push(elemCreate("input", "", {
            "type": "hidden",
            "name": "who",
            "value": "DEMON"
        }));
        html_form_content.push(elemCreate("input", "", {
            "type": "text",
            "name": "opinion_text",
            "required": "required",
            "class": "polarize-input",
            "placeholder": "Things you don't like"
        }));
        html_form_content.push(elemCreate("input", "", {
            "type": "submit",
            "class": "polarize-input",
            "value": "Badmouth it!"
        }));
        html_inner_content.push(elemCreate("form", html_form_content, {
            "action": settings.default_topic_url,
            "method": "post",
            "id": "feedback-demon"
        }));

        html_form_content.length = 0;

        // Idea form
        html_form_content.push(elemCreate("input", "", {
            "type": "hidden",
            "name": "redirect",
            "value": "NO"
        }));
        html_form_content.push(elemCreate("input", "", {
            "type": "hidden",
            "name": "who",
            "value": "THINKER"
        }));
        html_form_content.push(elemCreate("textarea", "", {
            "name": "opinion_text",
            "required": "required",
            "class": "polarize-input",
            "placeholder": "Ideas you have!"
        }));
        html_form_content.push(elemCreate("input", "", {
            "type": "submit",
            "class": "polarize-input",
            "value": "Send your ideas!"
        }));
        html_inner_content.push(elemCreate("form", html_form_content, {
            "action": settings.default_topic_url,
            "method": "post",
            "id": "feedback-idea"
        }));

        // Create slider and inner container
        html_inner.push(elemCreate("div", "", {
            "class": "feedback-slider",
            "style": add_position_style(settings)["slider-style"] + "background-image: url(" + settings.images_dir + "feedback-button.png);"
        }));
        html_inner.push(elemCreate("div", html_inner_content, {
            "id": "feedback-content",
            "style": add_position_style(settings)["content-style"]
        }));

        // Create root element
        html_container.push(elemCreate("div", html_inner, {
            "id": "feedback",
            "class": "feedback-bg",
            "style": add_position_style(settings)["container-style"] + "background-image: url(" + settings.images_dir + "polarize-logo.png);"
        }));

        // Append the string to the body
        $(html_container).prependTo("body");
    }

    function createEvents(settings) {
      $(".feedback-slider").on("click", function(){
        var $feedback = $("#feedback");
        if ($feedback.css("background-image") !== "none") {
          $feedback.removeClass("feedback-bg");
          $feedback.css({"background-image":"none"});
          $feedback.find('.polarize-input').show();
          $feedback.find('input[type="text"], textarea').val("");
          $feedback.find('.thanks').hide();
          $feedback.animate(JSON.parse(add_position_style(settings).animation.show));
        } else {
          $feedback.addClass("feedback-bg");
          $feedback.animate(JSON.parse(add_position_style(settings).animation.hide));
          $feedback.css({"background-image": "url(" + settings.images_dir + "polarize-logo.png)"});
        }
        return false;
      });
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
                $.error( 'Method ' +  method + ' does not exist on polarize Widget' );
            }
      };

})( jQuery, window, document );
