(function($) {
	'use strict';

	var project = {};

	project.init = function() {
		project.evenColumns();
		project.counterpartiesFilter();
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

	$(document).ready(project.init);

})(jQuery);

