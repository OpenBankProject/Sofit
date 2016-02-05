(function ( $ ) {

	$.fn.evenColumns = function() {

		var object = this;
		var argumentsPassed = arguments;

		function setHeight(options) {

			for (var j = 0; j < options.length; j++) {

				var settings = $.extend({
					maxScreenWidth: 9999,
					columns: 0
				}, options[j] );

				minWidth = options[j + 1] ? options[j + 1].maxScreenWidth + 1 : 0;

				if ($('html').width() >= minWidth && $('html').width() <= settings.maxScreenWidth) {

					object.css('height', '');

					if (settings.columns != 1) {
						var height = 0;
						var group = [];
						var full = 0;

						object.each(function(index) {
							full = 0;

							if ($(this).outerHeight() > height) {
								height = $(this).outerHeight();
							}

							if (settings.columns) {

								group.push($(this));

								if (!((index + 1)%settings.columns)) {

									for (var i = group.length - 1; i >= 0; i--) {
										group[i].outerHeight(height);
									}

									height = 0;
									full = 1;
									group = [];
								}
							}
						});

						if (!full) {

							for (var i = group.length - 1; i >= 0; i--) {
								group[i].outerHeight(height);
							}
						}

						if (!settings.columns) {
							object.outerHeight(height);
						}
					}
				}
			}
		}

		setHeight(argumentsPassed);

		$(window).load(function() {
			setHeight(argumentsPassed);
		});

		var changeScreenWidth;

		$(window).resize(function() {

			clearTimeout(changeScreenWidth);

			if ($('html').width() > 0 && $('html').width() < 9999) {

				changeScreenWidth = setTimeout(function(){
					setHeight(argumentsPassed);
				}, 200);
			}
		});

    	return this;
    };

}( jQuery ));
