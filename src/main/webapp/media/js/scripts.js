(function($) {
	'use strict';

	var project = {};

	project.init = function() {
		project.evenColumns();
		project.counterpartiesFilter();
		project.removeUserFromView();
		project.managementTableSorter();
	};

	project.evenColumns = function() {
		$('.account-info .account-info__box').evenColumns(
			{
				columns: 2
			}
		);

		$('.account-info__top-wrapper .account-info__box-headline').evenColumns(
			{
				columns: 2
			}
		);
		$('.users-table-body__row .users-table-body__cell').evenColumns(
			{
				columns: 3
			}
		);

		$('.views-table-body__row .views-table-body__cell').evenColumns(
			{
				columns: 3
			}
		);
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

