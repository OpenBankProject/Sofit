(function($) {
	'use strict';

	var project = {};

	project.init = function() {
		project.counterpartiesFilter();
		project.removeUserFromView();
		project.managementTableSorter();
		project.transloadit();
		project.fileUpload();
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

	project.transloadit = function() {
		$('#imageUploader').transloadit({
			wait: true
		});
	}

	project.fileUpload = function() {
		$(".file-upload").change(function() {
			    $(this).siblings('.file-upload-filename').html(this.value);
		});
	}

	$(document).ready(project.init);

})(jQuery);

