(function($) {
	'use strict';

	var project = {};

	project.init = function() {
		project.counterpartiesFilter();
		project.removeUserFromView();
		project.managementTableSorter();
	};

	project.counterpartiesFilter = function() {
		$('.counterparties-table-head__cell').on('click', function() {
			$('.counterparties-table-head__cell').removeClass('counterparties-table-head__cell--active');
			$(this).addClass('counterparties-table-head__cell--active');
		});
	};

	project.removeUserFromView = function() {
		$('.users-table-body__cell .remove').click(function () {
			$(this).closest('.row').css('background-color', '#CC0000').fadeOut(1000, function() {$(this).remove();});
		});
	};

	project.managementTableSorter = function() {
		$("#management .tablesorter").tablesorter();
	}

	$(document).ready(project.init);

})(jQuery);

